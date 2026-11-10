{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | A diagrams backend that outputs JSON commands for HTML Canvas rendering.
module Diagrams.Backend.CanvasJson (
    -- * Backend token
    CanvasJson (..),
    B,

    -- * Rendering
    renderCanvasJson,
    sizeFromSpec,
    CanvasJsonOptions (..),
    canvasJsonOptions,

    -- * JSON precision
    JsonPrecision (..),
    defaultJsonPrecision,
    roundN,
    encodeCmd,
    encodeBBox,
    encodingCmd,

    -- * Canvas commands
    CanvasCmd (..),
    CanvasDiagram (..),
    BBox (..),
    CompositeOp (..),
    compositeOpToText,
    compositeOpFromText,

    -- * Image masks
    MaskPng (..),
    parseMaskPng,
    maskPngWidth,
    maskPngHeight,
    maskPngBase64,
    maskPngFromBase64,
    maxMaskPngSide,
    imageMaskRectDecimals,

    -- * Multi-layer diagrams
    MaskLayer (..),
    LayeredDiagram (..),
    encodeMaskLayer,
    punchedLayers,

    -- * Optimization
    optimizeCommands,
) where

import Control.Monad (unless, when)
import Control.Monad.State.Strict
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Aeson.Encoding qualified as E
import Data.Aeson.Types qualified as A
import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Maybe (fromMaybe, isNothing)
import Data.Scientific (scientific)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Tree (Tree (..))
import Data.Typeable (Typeable)
import Diagrams.Core.Compile (RNode (..), RTree, fromDTree, toDTree)
import Diagrams.Core.Style (unmeasureAttrs)
import Diagrams.Core.Transform (matrixHomRep)
import Diagrams.Core.Types (Annotation (..), DNode (..))
import Diagrams.Prelude hiding ((<>))
import Diagrams.TwoD.Attributes (FillTexture (..), LineTexture (..))
import Diagrams.TwoD.Text (Text (..))
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- JSON Precision
--------------------------------------------------------------------------------

{- | Controls the number of decimal places used for each category of floating
point value when serializing to JSON. This avoids emitting full IEEE 754
double precision (15-17 digits) when the source data has far less precision.
-}
data JsonPrecision = JsonPrecision
    { jpCoordinates :: !Int
    -- ^ Path coordinates: MoveTo, LineTo, BezierTo, QuadTo, Arc center/radius (default: 4)
    , jpAlpha :: !Int
    -- ^ Alpha component 0-1 (default: 2)
    , jpTransform :: !Int
    -- ^ Transformation matrix components (default: 8)
    , jpDimensions :: !Int
    -- ^ Canvas width, height, bounding box (default: 4)
    , jpLineWidth :: !Int
    -- ^ Stroke line width (default: 4)
    , jpDash :: !Int
    -- ^ Line dash pattern values (default: 4)
    , jpAngles :: !Int
    -- ^ Arc start\/end angles in radians (default: 4)
    }
    deriving (Show, Eq)

{- | Sensible defaults: coordinates 2dp, colors integer, alpha 2dp,
transforms 6dp, dimensions 2dp, line width 2dp, dash 2dp, angles 4dp.
-}
defaultJsonPrecision :: JsonPrecision
defaultJsonPrecision =
    JsonPrecision
        { jpCoordinates = 4
        , jpAlpha = 2
        , jpTransform = 8
        , jpDimensions = 4
        , jpLineWidth = 4
        , jpDash = 4
        , jpAngles = 4
        }

{- | Round a 'Double' to @n@ decimal places and encode as a JSON 'A.Number'
using 'Data.Scientific.Scientific' to avoid IEEE 754 representation artifacts.
-}
roundN :: Int -> Double -> A.Value
roundN dp x = A.Number $ scientific sig (negate dp)
  where
    factor = (10 :: Integer) ^ dp
    sig = round (x * fromIntegral factor)

-- | 'roundN' written straight to an 'E.Encoding', with the same digits.
roundE :: Int -> Double -> E.Encoding
roundE dp x = E.scientific $ scientific sig (negate dp)
  where
    factor = (10 :: Integer) ^ dp
    sig = round (x * fromIntegral factor)

--------------------------------------------------------------------------------
-- Canvas Commands
--------------------------------------------------------------------------------

{- | Canvas drawing commands that map directly to the HTML Canvas API.
These are serialized as compact JSON arrays for efficiency.
-}
data CanvasCmd
    = -- State management
      CmdSave
    | CmdRestore
    | -- Transformation (a, b, c, d, e, f)
      CmdTransform !Double !Double !Double !Double !Double !Double
    | -- Path commands (all use ABSOLUTE coordinates)
      CmdBeginPath
    | CmdMoveTo !Double !Double
    | CmdLineTo !Double !Double
    | CmdBezierTo !Double !Double !Double !Double !Double !Double
    | CmdQuadTo !Double !Double !Double !Double
    | CmdArc !Double !Double !Double !Double !Double
    | CmdClosePath
    | -- Style and drawing
      CmdFill !Double !Double !Double !Double -- RGBA (sets fillStyle and fills)
    | CmdStroke !Double !Double !Double !Double !Double -- RGBA + lineWidth in diagram coords (sets strokeStyle/lineWidth and strokes)
    | CmdStrokeView !Double !Double !Double !Double !Double -- RGBA + lineWidth relative to view/output (divided by scale on render)
    | CmdSetFillColor !Double !Double !Double !Double -- Set fillStyle only
    | CmdSetStrokeColor !Double !Double !Double !Double !Double -- Set strokeStyle + lineWidth in diagram coords only
    | CmdSetStrokeColorView !Double !Double !Double !Double !Double -- Set strokeStyle + lineWidth relative to view/output only
    | CmdFillCurrent -- Fill using current fillStyle
    | CmdStrokeCurrent -- Stroke using current strokeStyle/lineWidth
    | CmdSetLineCap !Int -- 0=butt, 1=round, 2=square
    | CmdSetLineJoin !Int -- 0=miter, 1=round, 2=bevel
    | CmdSetLineDash ![Double]
    | CmdSetLineDashView ![Double] -- Line dash relative to view (divided by scale on render)
    | -- Text
      CmdFillText !String !Double !Double
    | CmdSetFont !String
    | -- Canvas state
      CmdSetGlobalCompositeOperation !CompositeOp
    | {- Image mask: RGBA, then the rectangle x0 y0 x1 y1 the PNG covers. Fills
      the PNG's set pixels with the colour under the current GCO and transform,
      leaving the path and fill style alone. PNG row 0 is at y1.
      -}
      CmdImageMask !Double !Double !Double !Double !Double !Double !Double !Double !MaskPng
    deriving (Show, Eq, Generic)

{- | Longest side of a 'MaskPng'. It is the WebGL texture size floor, and it
bounds what a decoder inflates before it can reject a payload.
-}
maxMaskPngSide :: Int
maxMaskPngSide = 4096

{- | Decimals of the @IM@ rectangle on the wire, at least the coordinate
precision. The decoder demands square pixels, so the edges must survive
rounding: producers keep them on a grid of @10^-imageMaskRectDecimals@.
-}
imageMaskRectDecimals :: Int
imageMaskRectDecimals = 6

{- | The PNG bytes of a 'CmdImageMask': 1-bit greyscale, non-interlaced, with a
@tRNS@ making grey 0 transparent, so unset pixels leave the destination alone
under source-over and destination-out. 'parseMaskPng' checks this profile.
-}
newtype MaskPng = MaskPng BS.ByteString
    deriving newtype (Eq, Ord)

-- Payloads are hundreds of kilobytes, so show only their size.
instance Show MaskPng where
    showsPrec _ png@(MaskPng bs) =
        showString "<MaskPng "
            . shows (maskPngWidth png)
            . showChar 'x'
            . shows (maskPngHeight png)
            . showString ", "
            . shows (BS.length bs)
            . showString " bytes>"

-- | Pixel width from the IHDR, 0 if there is none.
maskPngWidth :: MaskPng -> Int
maskPngWidth (MaskPng bs) = be32At 16 bs

-- | Pixel height from the IHDR, 0 if there is none.
maskPngHeight :: MaskPng -> Int
maskPngHeight (MaskPng bs) = be32At 20 bs

be32At :: Int -> BS.ByteString -> Int
be32At i bs
    | BS.length bs < i + 4 = 0
    | otherwise = foldl (\acc k -> acc `shiftL` 8 .|. fromIntegral (BS.index bs (i + k))) 0 [0 .. 3]

-- | Standard padded base64, as the payload is sent on the wire.
maskPngBase64 :: MaskPng -> T.Text
maskPngBase64 (MaskPng bs) = TE.decodeLatin1 (B64.encode bs)

-- | Decode a wire payload and check it with 'parseMaskPng'.
maskPngFromBase64 :: T.Text -> Either String MaskPng
maskPngFromBase64 t = case B64.decode (TE.encodeUtf8 t) of
    Left err -> Left ("IM payload is not standard padded base64: " <> err)
    Right bs -> parseMaskPng bs

{- | Check the 'MaskPng' profile. Chunk CRCs and the image data are left to the
decoders that inflate it.
-}
parseMaskPng :: BS.ByteString -> Either String MaskPng
parseMaskPng bs = do
    unless (pngSignature `BS.isPrefixOf` bs) $ Left "IM payload is not a PNG: bad signature"
    chunks <- pngChunks (BS.drop 8 bs)
    case chunks of
        (("IHDR", ihdr) : rest) -> checkIhdr ihdr >> checkChunks rest
        _ -> Left "IM PNG must start with an IHDR chunk"
    pure $! MaskPng bs
  where
    checkIhdr ihdr = do
        unless (BS.length ihdr == 13) $ Left "IM PNG IHDR must be 13 bytes"
        unless (be32At 0 ihdr > 0 && be32At 4 ihdr > 0) $ Left "IM PNG width and height must be positive"
        unless (be32At 0 ihdr <= maxMaskPngSide && be32At 4 ihdr <= maxMaskPngSide) $
            Left ("IM PNG sides must be at most " <> show maxMaskPngSide <> " px")
        let field = BS.index ihdr
        unless (field 8 == 1) $ Left ("IM PNG bit depth must be 1, got " <> show (field 8))
        unless (field 9 == 0) $ Left ("IM PNG colour type must be 0 (greyscale), got " <> show (field 9))
        unless (field 10 == 0 && field 11 == 0) $ Left "IM PNG compression and filter methods must be 0"
        unless (field 12 == 0) $ Left "IM PNG must not be interlaced"
    -- Browsers reject unknown critical chunks and IDATs split by other chunks,
    -- so allow only what every decoder reads the same way.
    checkChunks rest = do
        let (beforeData, fromData) = break ((== "IDAT") . fst) rest
            afterData = dropWhile ((== "IDAT") . fst) fromData
            outside = beforeData ++ afterData
        when (null fromData) $ Left "IM PNG has no IDAT chunk"
        case filter (not . isAncillary) (map fst outside) of
            "IDAT" : _ -> Left "IM PNG IDAT chunks must be consecutive"
            "IHDR" : _ -> Left "IM PNG must have one IHDR chunk"
            ty : _ -> Left ("IM PNG must not have a " <> show ty <> " chunk")
            [] -> pure ()
        -- Their layout depends on the colour type, which tinting rewrites.
        case filter (`elem` ["bKGD", "sBIT", "hIST"]) (map fst outside) of
            ty : _ -> Left ("IM PNG must not have a " <> show ty <> " chunk")
            [] -> pure ()
        case [trns | ("tRNS", trns) <- beforeData] of
            [] -> Left "IM PNG must have a tRNS chunk before IDAT"
            [trns] -> unless (trns == BS.pack [0, 0]) $ Left "IM PNG tRNS must make grey 0 transparent"
            _ -> Left "IM PNG must have one tRNS chunk"
        when (any ((== "tRNS") . fst) afterData) $ Left "IM PNG must have one tRNS chunk"
    isAncillary ty = BS.index ty 0 .&. 0x20 /= 0

pngSignature :: BS.ByteString
pngSignature = BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

-- The chunks before IEND as (type, data).
pngChunks :: BS.ByteString -> Either String [(BS.ByteString, BS.ByteString)]
pngChunks bs
    | BS.length bs < 12 = Left "IM PNG is truncated or has no IEND chunk"
    | BS.length bs < 12 + len = Left "IM PNG is truncated"
    | ty == "IEND" =
        if len /= 0
            then Left "IM PNG IEND must be empty"
            else if BS.length bs == 12 then Right [] else Left "IM PNG has data after IEND"
    | otherwise = ((ty, BS.take len (BS.drop 8 bs)) :) <$> pngChunks (BS.drop (12 + len) bs)
  where
    len = be32At 0 bs
    ty = BS.take 4 (BS.drop 4 bs)

{- | Subset of Canvas 2D @globalCompositeOperation@ values that this backend
actually emits. Using a closed enumeration lets producers and consumers
exhaustively pattern-match on it and keeps the wire format stable.

Both operations affect how subsequent draw calls interact with the pixels
already on the canvas ("destination"). The HTML Canvas 2D spec defines
them in terms of a per-pixel Porter-Duff composite;
-}
data CompositeOp
    = {- | @source-over@ — the Canvas 2D default. New shapes are drawn on top
      of the existing content using normal alpha compositing. Emit this to
      reset the blend mode after a destination-* group without relying on
      a surrounding 'CmdSave'\/'CmdRestore' to pop it back.
      -}
      SourceOver
    | {- | @destination-out@ — wherever a new shape is drawn, the existing
      (destination) pixel is erased to transparent. The new shape's own
      colour is ignored.
      -}
      DestinationOut
    | {- | @destination-in@ — existing pixels are kept only where the new
      shape overlaps them; elsewhere the destination is erased to
      transparent.
      -}
      DestinationIn
    deriving (Show, Eq, Generic)

-- | Wire-level string for a 'CompositeOp', matching the Canvas 2D spec.
compositeOpToText :: CompositeOp -> String
compositeOpToText SourceOver = "source-over"
compositeOpToText DestinationOut = "destination-out"
compositeOpToText DestinationIn = "destination-in"

-- | Inverse of 'compositeOpToText'.
compositeOpFromText :: T.Text -> Maybe CompositeOp
compositeOpFromText "source-over" = Just SourceOver
compositeOpFromText "destination-out" = Just DestinationOut
compositeOpFromText "destination-in" = Just DestinationIn
compositeOpFromText _ = Nothing

instance FromJSON CompositeOp where
    parseJSON = A.withText "CompositeOp" $ \t -> case compositeOpFromText t of
        Just co -> pure co
        Nothing -> fail $ "Unknown globalCompositeOperation: " <> T.unpack t

-- | Encode a command as a compact JSON array using the given precision settings.
encodeCmd :: JsonPrecision -> CanvasCmd -> A.Value
encodeCmd jp cmd = case cmd of
    CmdSave -> A.toJSON [A.String "S"]
    CmdRestore -> A.toJSON [A.String "R"]
    CmdTransform a b c d e f -> A.toJSON [A.String "T", t a, t b, t c, t d, t e, t f]
    CmdBeginPath -> A.toJSON [A.String "B"]
    CmdMoveTo x y -> A.toJSON [A.String "M", co x, co y]
    CmdLineTo x y -> A.toJSON [A.String "L", co x, co y]
    CmdBezierTo x1 y1 x2 y2 x y -> A.toJSON [A.String "C", co x1, co y1, co x2, co y2, co x, co y]
    CmdQuadTo x1 y1 x y -> A.toJSON [A.String "Q", co x1, co y1, co x, co y]
    CmdArc cx cy r startA endA -> A.toJSON [A.String "A", co cx, co cy, co r, ang startA, ang endA]
    CmdClosePath -> A.toJSON [A.String "Z"]
    CmdFill r g b a -> A.toJSON [A.String "F", col r, col g, col b, al a]
    CmdStroke r g b a lineW -> A.toJSON [A.String "K", col r, col g, col b, al a, lw' lineW]
    CmdStrokeView r g b a lineW -> A.toJSON [A.String "KV", col r, col g, col b, al a, lw' lineW]
    CmdSetFillColor r g b a -> A.toJSON [A.String "FS", col r, col g, col b, al a]
    CmdSetStrokeColor r g b a lineW -> A.toJSON [A.String "KS", col r, col g, col b, al a, lw' lineW]
    CmdSetStrokeColorView r g b a lineW -> A.toJSON [A.String "KSV", col r, col g, col b, al a, lw' lineW]
    CmdFillCurrent -> A.toJSON [A.String "f"]
    CmdStrokeCurrent -> A.toJSON [A.String "k"]
    CmdSetLineCap c -> A.toJSON [A.String "LC", A.toJSON c]
    CmdSetLineJoin j -> A.toJSON [A.String "LJ", A.toJSON j]
    CmdSetLineDash ds -> A.toJSON (A.String "LD" : map da ds)
    CmdSetLineDashView ds -> A.toJSON (A.String "LDV" : map da ds)
    CmdFillText txt x y -> A.toJSON [A.String "FT", A.toJSON txt, co x, co y]
    CmdSetFont f -> A.toJSON [A.String "SF", A.toJSON f]
    CmdSetGlobalCompositeOperation gco -> A.toJSON [A.String "GCO", A.toJSON (compositeOpToText gco)]
    CmdImageMask r g b a x0 y0 x1 y1 png ->
        A.toJSON [A.String "IM", col r, col g, col b, al a, imc x0, imc y0, imc x1, imc y1, A.String (maskPngBase64 png)]
  where
    imc = roundN (max imageMaskRectDecimals (jpCoordinates jp))
    co = roundN (jpCoordinates jp)
    col = roundN 0
    al = roundN (jpAlpha jp)
    t = roundN (jpTransform jp)
    lw' = roundN (jpLineWidth jp)
    da = roundN (jpDash jp)
    ang = roundN (jpAngles jp)

{- | 'encodeCmd' written straight to an 'E.Encoding'. A large diagram has
millions of commands, and building their 'A.Value's first holds them all.
-}
encodingCmd :: JsonPrecision -> CanvasCmd -> E.Encoding
encodingCmd jp cmd = case cmd of
    CmdSave -> tag "S" []
    CmdRestore -> tag "R" []
    CmdTransform a b c d e f -> tag "T" [t a, t b, t c, t d, t e, t f]
    CmdBeginPath -> tag "B" []
    CmdMoveTo x y -> tag "M" [co x, co y]
    CmdLineTo x y -> tag "L" [co x, co y]
    CmdBezierTo x1 y1 x2 y2 x y -> tag "C" [co x1, co y1, co x2, co y2, co x, co y]
    CmdQuadTo x1 y1 x y -> tag "Q" [co x1, co y1, co x, co y]
    CmdArc cx cy r startA endA -> tag "A" [co cx, co cy, co r, ang startA, ang endA]
    CmdClosePath -> tag "Z" []
    CmdFill r g b a -> tag "F" [col r, col g, col b, al a]
    CmdStroke r g b a lineW -> tag "K" [col r, col g, col b, al a, lw' lineW]
    CmdStrokeView r g b a lineW -> tag "KV" [col r, col g, col b, al a, lw' lineW]
    CmdSetFillColor r g b a -> tag "FS" [col r, col g, col b, al a]
    CmdSetStrokeColor r g b a lineW -> tag "KS" [col r, col g, col b, al a, lw' lineW]
    CmdSetStrokeColorView r g b a lineW -> tag "KSV" [col r, col g, col b, al a, lw' lineW]
    CmdFillCurrent -> tag "f" []
    CmdStrokeCurrent -> tag "k" []
    CmdSetLineCap c -> tag "LC" [E.int c]
    CmdSetLineJoin j -> tag "LJ" [E.int j]
    CmdSetLineDash ds -> tag "LD" (map da ds)
    CmdSetLineDashView ds -> tag "LDV" (map da ds)
    CmdFillText txt x y -> tag "FT" [E.string txt, co x, co y]
    CmdSetFont f -> tag "SF" [E.string f]
    CmdSetGlobalCompositeOperation gco -> tag "GCO" [E.string (compositeOpToText gco)]
    CmdImageMask r g b a x0 y0 x1 y1 png ->
        tag "IM" [col r, col g, col b, al a, imc x0, imc y0, imc x1, imc y1, E.text (maskPngBase64 png)]
  where
    tag name fields = E.list id (E.text name : fields)
    imc = roundE (max imageMaskRectDecimals (jpCoordinates jp))
    co = roundE (jpCoordinates jp)
    col = roundE 0
    al = roundE (jpAlpha jp)
    t = roundE (jpTransform jp)
    lw' = roundE (jpLineWidth jp)
    da = roundE (jpDash jp)
    ang = roundE (jpAngles jp)

-- | Encode commands using 'defaultJsonPrecision'.
instance ToJSON CanvasCmd where
    toJSON = encodeCmd defaultJsonPrecision
    toEncoding = encodingCmd defaultJsonPrecision

{- | Parse the compact array form emitted by 'encodeCmd'. Precision is lost
on the round-trip (each field is a plain JSON number) but the command
structure is preserved.
-}
instance FromJSON CanvasCmd where
    parseJSON = A.withArray "CanvasCmd" $ \arr -> case toListV arr of
        [A.String "S"] -> pure CmdSave
        [A.String "R"] -> pure CmdRestore
        [A.String "T", a, b, c, d, e, f] ->
            CmdTransform <$> num a <*> num b <*> num c <*> num d <*> num e <*> num f
        [A.String "B"] -> pure CmdBeginPath
        [A.String "M", x, y] -> CmdMoveTo <$> num x <*> num y
        [A.String "L", x, y] -> CmdLineTo <$> num x <*> num y
        [A.String "C", x1, y1, x2, y2, x, y] ->
            CmdBezierTo <$> num x1 <*> num y1 <*> num x2 <*> num y2 <*> num x <*> num y
        [A.String "Q", cx, cy, x, y] ->
            CmdQuadTo <$> num cx <*> num cy <*> num x <*> num y
        [A.String "A", cx, cy, r, sa, ea] ->
            CmdArc <$> num cx <*> num cy <*> num r <*> num sa <*> num ea
        [A.String "Z"] -> pure CmdClosePath
        [A.String "F", r, g, b, a] ->
            CmdFill <$> num r <*> num g <*> num b <*> num a
        [A.String "K", r, g, b, a, w] ->
            CmdStroke <$> num r <*> num g <*> num b <*> num a <*> num w
        [A.String "KV", r, g, b, a, w] ->
            CmdStrokeView <$> num r <*> num g <*> num b <*> num a <*> num w
        [A.String "FS", r, g, b, a] ->
            CmdSetFillColor <$> num r <*> num g <*> num b <*> num a
        [A.String "KS", r, g, b, a, w] ->
            CmdSetStrokeColor <$> num r <*> num g <*> num b <*> num a <*> num w
        [A.String "KSV", r, g, b, a, w] ->
            CmdSetStrokeColorView <$> num r <*> num g <*> num b <*> num a <*> num w
        [A.String "f"] -> pure CmdFillCurrent
        [A.String "k"] -> pure CmdStrokeCurrent
        [A.String "LC", c] -> CmdSetLineCap <$> parseJSON c
        [A.String "LJ", j] -> CmdSetLineJoin <$> parseJSON j
        (A.String "LD" : ds) -> CmdSetLineDash <$> traverse num ds
        (A.String "LDV" : ds) -> CmdSetLineDashView <$> traverse num ds
        [A.String "FT", txt, x, y] ->
            CmdFillText <$> parseJSON txt <*> num x <*> num y
        [A.String "SF", f] -> CmdSetFont <$> parseJSON f
        [A.String "GCO", gco] -> CmdSetGlobalCompositeOperation <$> parseJSON gco
        [A.String "IM", r, g, b, a, x0, y0, x1, y1, png] -> do
            cmd <-
                CmdImageMask
                    <$> num r
                    <*> num g
                    <*> num b
                    <*> num a
                    <*> num x0
                    <*> num y0
                    <*> num x1
                    <*> num y1
                    <*> A.withText "IM payload" (either fail pure . maskPngFromBase64) png
            either fail (const (pure cmd)) (checkImageMaskRect cmd)
        _ -> fail $ "Unrecognised canvas command array: " <> show (A.Array arr)
      where
        toListV = foldr (:) []
        num :: A.Value -> A.Parser Double
        num = parseJSON

{- | Renderers derive one pixel pitch from the rectangle, so a stretched mask
would be placed differently by each of them.
-}
checkImageMaskRect :: CanvasCmd -> Either String ()
checkImageMaskRect (CmdImageMask _ _ _ _ x0 y0 x1 y1 png) = do
    unless (all finite [x0, y0, x1, y1]) $ Left "IM rectangle must be finite"
    unless (x1 > x0 && y1 > y0) $ Left "IM rectangle must have x1 > x0 and y1 > y0"
    let sx = (x1 - x0) / fromIntegral (maskPngWidth png)
        sy = (y1 - y0) / fromIntegral (maskPngHeight png)
    unless (abs (sx - sy) <= 1e-9 * max sx sy) $
        Left ("IM pixels must be square, got pitches " <> show sx <> " by " <> show sy)
  where
    finite v = not (isNaN v || isInfinite v)
checkImageMaskRect _ = Right ()

-- | Bounding box for the diagram
data BBox = BBox
    { bbMinX :: !Double
    , bbMinY :: !Double
    , bbMaxX :: !Double
    , bbMaxY :: !Double
    }
    deriving (Show, Eq, Generic)

-- | Encode a bounding box using the given precision for dimensions.
encodeBBox :: JsonPrecision -> BBox -> A.Value
encodeBBox jp bb =
    A.object
        [ "minX" A..= dim (bbMinX bb)
        , "minY" A..= dim (bbMinY bb)
        , "maxX" A..= dim (bbMaxX bb)
        , "maxY" A..= dim (bbMaxY bb)
        ]
  where
    dim = roundN (jpDimensions jp)

-- | 'encodeBBox' written straight to an 'E.Encoding'; keys in the order 'A.object' sorts them.
encodingBBox :: JsonPrecision -> BBox -> E.Encoding
encodingBBox jp bb =
    E.pairs $
        E.pair "maxX" (dim (bbMaxX bb))
            <> E.pair "maxY" (dim (bbMaxY bb))
            <> E.pair "minX" (dim (bbMinX bb))
            <> E.pair "minY" (dim (bbMinY bb))
  where
    dim = roundE (jpDimensions jp)

-- | Encode using 'defaultJsonPrecision'.
instance ToJSON BBox where
    toJSON = encodeBBox defaultJsonPrecision
    toEncoding = encodingBBox defaultJsonPrecision

instance FromJSON BBox where
    parseJSON = A.withObject "BBox" $ \o ->
        BBox
            <$> o A..: "minX"
            <*> o A..: "minY"
            <*> o A..: "maxX"
            <*> o A..: "maxY"

-- | A complete canvas diagram with dimensions and commands
data CanvasDiagram = CanvasDiagram
    { cdWidth :: !Double
    , cdHeight :: !Double
    , cdBounds :: !BBox
    , cdCommands :: ![CanvasCmd]
    , cdPrecision :: !JsonPrecision
    }
    deriving (Show, Eq)

-- | Encode using the precision stored in the diagram.
instance ToJSON CanvasDiagram where
    toJSON cd =
        let jp = cdPrecision cd
         in A.object
                [ "width" A..= roundN (jpDimensions jp) (cdWidth cd)
                , "height" A..= roundN (jpDimensions jp) (cdHeight cd)
                , "bounds" A..= encodeBBox jp (cdBounds cd)
                , "commands" A..= map (encodeCmd jp) (cdCommands cd)
                ]
    -- Matched up front, so only the encoder holds the commands and they are freed as it writes them.
    toEncoding (CanvasDiagram width height bounds commands jp) =
        E.pairs $
            E.pair "bounds" (encodingBBox jp bounds)
                <> E.pair "commands" (E.list (encodingCmd jp) commands)
                <> E.pair "height" (roundE (jpDimensions jp) height)
                <> E.pair "width" (roundE (jpDimensions jp) width)

-- | Decoding loses the original precision; 'defaultJsonPrecision' is used.
instance FromJSON CanvasDiagram where
    parseJSON = A.withObject "CanvasDiagram" $ \o ->
        CanvasDiagram
            <$> o A..: "width"
            <*> o A..: "height"
            <*> o A..: "bounds"
            <*> o A..: "commands"
            <*> pure defaultJsonPrecision

--------------------------------------------------------------------------------
-- Multi-layer diagrams
--------------------------------------------------------------------------------

{- | A single coloured layer in a multi-layer output. The command stream is
interpreted in mask semantics: the commands describe shape and alpha, and
the layer @mlColor@ is the tint applied with Canvas 2D @source-in@ (or the
equivalent PixiJS mask-texture compositing). Matches the TypeScript
@MaskLayer@ type in @diagrams-canvas-json-web@.
-}
data MaskLayer = MaskLayer
    { mlName :: !(Maybe T.Text)
    -- ^ Optional human-readable layer name.
    , mlColor :: !(Double, Double, Double, Double)
    -- ^ Tint colour: RGB 0-255, alpha 0-1.
    , mlCommands :: ![CanvasCmd]
    }
    deriving (Show, Eq)

-- | Encode a 'MaskLayer' using the given precision.
encodeMaskLayer :: JsonPrecision -> MaskLayer -> A.Value
encodeMaskLayer jp cl =
    let (r, g, b, a) = mlColor cl
        col = roundN 0
        al = roundN (jpAlpha jp)
        base =
            [ "color" A..= [col r, col g, col b, al a]
            , "commands" A..= map (encodeCmd jp) (mlCommands cl)
            ]
        nameField = case mlName cl of
            Nothing -> []
            Just n -> ["name" A..= n]
     in A.object (nameField ++ base)

-- | 'encodeMaskLayer' written straight to an 'E.Encoding'.
encodingMaskLayer :: JsonPrecision -> MaskLayer -> E.Encoding
encodingMaskLayer jp cl =
    let (r, g, b, a) = mlColor cl
     in E.pairs $
            E.pair "color" (E.list id [roundE 0 r, roundE 0 g, roundE 0 b, roundE (jpAlpha jp) a])
                <> E.pair "commands" (E.list (encodingCmd jp) (mlCommands cl))
                <> maybe mempty (E.pair "name" . E.text) (mlName cl)

-- | Encode using 'defaultJsonPrecision'.
instance ToJSON MaskLayer where
    toJSON = encodeMaskLayer defaultJsonPrecision
    toEncoding = encodingMaskLayer defaultJsonPrecision

instance FromJSON MaskLayer where
    parseJSON = A.withObject "MaskLayer" $ \o -> do
        name <- o A..:? "name"
        col <- o A..: "color"
        commands <- o A..: "commands"
        case col of
            [r, g, b, a] -> pure (MaskLayer name (r, g, b, a) commands)
            _ -> fail "MaskLayer.color must be a 4-element array"

{- | Multi-layer diagram output — the Haskell counterpart of the TypeScript
@LayeredDiagram@ in @diagrams-canvas-json-web@. Layers are composited in
order (first = bottom).
-}
data LayeredDiagram = LayeredDiagram
    { ldWidth :: !Double
    , ldHeight :: !Double
    , ldBounds :: !BBox
    , ldLayers :: ![MaskLayer]
    , ldPunch :: ![CanvasCmd]
    {- ^ Drawn after every layer's own commands, such as the holes through
    the whole board, so they are kept once rather than in each layer.
    -}
    , ldPrecision :: !JsonPrecision
    }
    deriving (Show, Eq)

-- | The layers with 'ldPunch' drawn into each, as they are composited.
punchedLayers :: LayeredDiagram -> [MaskLayer]
punchedLayers mld
    | null (ldPunch mld) = ldLayers mld
    | otherwise = [layer{mlCommands = mlCommands layer ++ ldPunch mld} | layer <- ldLayers mld]

-- | Encode using the precision stored in the diagram.
instance ToJSON LayeredDiagram where
    toJSON mld =
        let jp = ldPrecision mld
         in A.object $
                [ "width" A..= roundN (jpDimensions jp) (ldWidth mld)
                , "height" A..= roundN (jpDimensions jp) (ldHeight mld)
                , "bounds" A..= encodeBBox jp (ldBounds mld)
                , "layers" A..= map (encodeMaskLayer jp) (ldLayers mld)
                ]
                    ++ ["punch" A..= map (encodeCmd jp) (ldPunch mld) | not (null (ldPunch mld))]
    toEncoding (LayeredDiagram width height bounds layers punch jp) =
        E.pairs $
            E.pair "bounds" (encodingBBox jp bounds)
                <> E.pair "height" (roundE (jpDimensions jp) height)
                <> E.pair "layers" (E.list (encodingMaskLayer jp) layers)
                <> (if null punch then mempty else E.pair "punch" (E.list (encodingCmd jp) punch))
                <> E.pair "width" (roundE (jpDimensions jp) width)

-- | Decoding loses the original precision; 'defaultJsonPrecision' is used.
instance FromJSON LayeredDiagram where
    parseJSON = A.withObject "LayeredDiagram" $ \o ->
        LayeredDiagram
            <$> o A..: "width"
            <*> o A..: "height"
            <*> o A..: "bounds"
            <*> o A..: "layers"
            <*> o A..:? "punch" A..!= []
            <*> pure defaultJsonPrecision

--------------------------------------------------------------------------------
-- Backend Definition
--------------------------------------------------------------------------------

-- | The CanvasJson backend token
data CanvasJson = CanvasJson
    deriving (Eq, Ord, Read, Show, Typeable)

-- | Convenient type alias for the backend
type B = CanvasJson

-- | Backend options
data CanvasJsonOptions = CanvasJsonOptions
    { _canvasJsonSize :: SizeSpec V2 Double
    , _canvasJsonPrecision :: JsonPrecision
    }

-- | Default options with specified size and 'defaultJsonPrecision'.
canvasJsonOptions :: SizeSpec V2 Double -> CanvasJsonOptions
canvasJsonOptions sz = CanvasJsonOptions sz defaultJsonPrecision

type instance V CanvasJson = V2
type instance N CanvasJson = Double

--------------------------------------------------------------------------------
-- Render Monad
--------------------------------------------------------------------------------

-- | Whether the current line width is in diagram coordinates or view-relative.
data LineWidthMode = LWCoord | LWView
    deriving (Show, Eq)

-- | Render state accumulating commands
data RenderState = RenderState
    { _rsCommands :: ![CanvasCmd] -- Commands in reverse order
    , _rsStyle :: !(Style V2 Double) -- Current accumulated style (measures resolved)
    , _rsLineWidthMode :: !LineWidthMode -- Whether current line width is coordinate or view-relative
    , _rsDashMode :: !LineWidthMode -- Whether current dashing is coordinate or view-relative
    , _rsPos :: !(P2 Double) -- Current path position for offset conversion
    }

-- | The render monad
type RenderM a = State RenderState a

-- | Emit a canvas command
emit :: CanvasCmd -> RenderM ()
emit cmd = modify' $ \s -> s{_rsCommands = cmd : _rsCommands s}

-- | Update current position
setPos :: P2 Double -> RenderM ()
setPos p = modify' $ \s -> s{_rsPos = p}

-- | Get current position
getPos :: RenderM (P2 Double)
getPos = gets _rsPos

-- | Run render monad and extract commands
runRenderM :: RenderM () -> [CanvasCmd]
runRenderM m = reverse $ _rsCommands $ execState m (RenderState [] mempty LWView LWView origin)

--------------------------------------------------------------------------------
-- Backend Instance
--------------------------------------------------------------------------------

instance Backend CanvasJson V2 Double where
    newtype Render CanvasJson V2 Double = R {unR :: RenderM ()}

    type Result CanvasJson V2 Double = [CanvasCmd]

    data Options CanvasJson V2 Double = CanvasJsonOpts

    renderRTree _ _ rt = runRenderM (renderRTree' 1 1 rt)

    adjustDia _ CanvasJsonOpts d = (CanvasJsonOpts, mempty, d)

instance Semigroup (Render CanvasJson V2 Double) where
    R ra <> R rb = R (ra >> rb)

instance Monoid (Render CanvasJson V2 Double) where
    mempty = R (return ())

--------------------------------------------------------------------------------
-- RTree Rendering
--------------------------------------------------------------------------------

{- | Render an RTree to canvas commands.
Styles in the tree may contain unresolved measures; we resolve them
here while classifying line width measures as local vs view-relative.
-}
renderRTree' ::
    -- | Global-to-output scale (for resolving measures)
    Double ->
    -- | Normalized-to-output scale (for resolving measures)
    Double ->
    RTree CanvasJson V2 Double Annotation ->
    RenderM ()
renderRTree' _ _ (Node (RPrim p) _) = unR $ render CanvasJson p
renderRTree' gToO nToO (Node (RStyle sty) children) = do
    emit CmdSave
    oldSty <- gets _rsStyle
    oldLWMode <- gets _rsLineWidthMode
    oldDashMode <- gets _rsDashMode
    -- Classify measures before resolving, but only if the style actually
    -- sets them. Otherwise keep the parent's mode.
    -- Resolve the style with the actual scales, and also with a large nToO
    -- to probe whether measures are view-relative (normalized/output).
    let resolvedSty = unmeasureAttrs gToO nToO sty
        probeSty = unmeasureAttrs gToO 1000 sty
        -- Classify line width: compare resolved value at nToO=1 vs nToO=1000
        lwMode = case (view _lineWidthU resolvedSty, view _lineWidthU probeSty) of
            (Just lw1, Just lw2) -> if lw1 == lw2 then LWCoord else LWView
            _ -> oldLWMode
        -- Classify dashing: compare resolved dash lengths at nToO=1 vs nToO=1000
        dashMode = case (view _dashingU resolvedSty, view _dashingU probeSty) of
            (Just (Dashing ds1 _), Just (Dashing ds2 _)) -> if ds1 == ds2 then LWCoord else LWView
            _ -> oldDashMode
    modify' $ \s ->
        s
            { _rsStyle = resolvedSty Prelude.<> _rsStyle s
            , _rsLineWidthMode = lwMode
            , _rsDashMode = dashMode
            }
    mapM_ (renderRTree' gToO nToO) children
    modify' $ \s -> s{_rsStyle = oldSty, _rsLineWidthMode = oldLWMode, _rsDashMode = oldDashMode}
    emit CmdRestore
renderRTree' gToO nToO (Node (RAnnot _) children) = mapM_ (renderRTree' gToO nToO) children
renderRTree' gToO nToO (Node REmpty children) = mapM_ (renderRTree' gToO nToO) children

-- | Render a 2D transformation
renderTransform :: T2 Double -> RenderM ()
renderTransform tr = emit $ CmdTransform a1 a2 b1 b2 c1 c2
  where
    mat = matrixHomRep tr
    (a1, a2, b1, b2, c1, c2) = case mat of
        [[x1, x2], [y1, y2], [z1, z2]] -> (x1, x2, y1, y2, z1, z2)
        _ -> error "renderTransform: unexpected matrix shape from matrixHomRep"

--------------------------------------------------------------------------------
-- Renderable Instances
--------------------------------------------------------------------------------

instance Renderable (Path V2 Double) CanvasJson where
    render _ p = R $ renderPath p

-- | Render a path with current style
renderPath :: Path V2 Double -> RenderM ()
renderPath p = do
    sty <- gets _rsStyle
    emit CmdBeginPath
    mapM_ renderLocTrail (pathTrails p)

    -- Only fill closed paths (loops). Open paths (lines) should not be
    -- filled — canvas auto-closes open paths for fill, creating large
    -- unintended polygons.
    let hasClosed = any (isLoop . unLoc) (pathTrails p)
    when hasClosed $
        case extractFillColor sty of
            Just (r, g, b, a) -> emit $ CmdFill r g b a
            Nothing -> return ()

    -- Apply stroke if present and line width > 0.
    -- Skip stroking when lineWidth is 0 (fill-only shapes like gerber regions).
    let lineW = fromMaybe 1 (extractLineWidth sty)
    when (lineW > 0) $ do
        lwMode <- gets _rsLineWidthMode
        let strokeCmd = case lwMode of
                LWCoord -> CmdStroke
                LWView -> CmdStrokeView
        case extractLineColor sty of
            Just (r, g, b, a) -> do
                -- Set line properties
                case getAttr sty of
                    Just LineCapButt -> emit $ CmdSetLineCap 0
                    Just LineCapRound -> emit $ CmdSetLineCap 1
                    Just LineCapSquare -> emit $ CmdSetLineCap 2
                    Nothing -> return ()
                case getAttr sty of
                    Just LineJoinMiter -> emit $ CmdSetLineJoin 0
                    Just LineJoinRound -> emit $ CmdSetLineJoin 1
                    Just LineJoinBevel -> emit $ CmdSetLineJoin 2
                    Nothing -> return ()
                case getAttr sty :: Maybe (Dashing Double) of
                    Just (Dashing ds _) -> do
                        dashMode <- gets _rsDashMode
                        emit $ case dashMode of
                            LWCoord -> CmdSetLineDash ds
                            LWView -> CmdSetLineDashView ds
                    Nothing -> return ()
                emit $ strokeCmd r g b a lineW
            Nothing ->
                -- Default stroke if no fill and no explicit line color
                when (not hasClosed || isNothing (extractFillColor sty)) $
                    emit $
                        strokeCmd 0 0 0 1 lineW

-- | Render a located trail
renderLocTrail :: Located (Trail V2 Double) -> RenderM ()
renderLocTrail lt = do
    let startPt@(P (V2 x y)) = loc lt
    emit $ CmdMoveTo x y
    setPos startPt
    renderTrail (unLoc lt) startPt

-- | Render a trail starting from a point
renderTrail :: Trail V2 Double -> P2 Double -> RenderM ()
renderTrail t startPt = withTrail renderLine renderLoop t
  where
    renderLine ln = mapM_ renderSeg (lineSegments ln)
    renderLoop lp = do
        let segs = lineSegments $ cutLoop lp
        case loopCircle startPt segs of
            Just (V2 cx cy, r, startAngle) -> emit $ CmdArc cx cy r startAngle (startAngle + 2 * pi)
            Nothing -> mapM_ renderSeg segs
        emit CmdClosePath
        setPos startPt -- Loop returns to start

{- | The centre, radius and start angle of a loop of cubic segments tracing a
whole circle anticlockwise, as diagrams draws circles, so that it can be sent as
one arc rather than its Béziers. Clockwise circles are left as Béziers, as an arc
command only sweeps anticlockwise in every renderer.
-}
loopCircle :: P2 Double -> [Segment Closed V2 Double] -> Maybe (V2 Double, Double, Double)
loopCircle (P first) segs@(Cubic v1 _ (OffsetClosed w) : _ : _)
    | w `dot` inward > 0
    , and (zipWith3 tracesArc ends (drop 1 ends) segs)
    , abs (sum (zipWith sweep ends (drop 1 ends)) - 2 * pi) <= 1e-9 =
        Just (centre, r, angleOf first)
  where
    ends = scanl (\p seg -> p + segOffset seg) first segs
    -- The centre is on the left of the first segment's start, equally far from its end.
    inward = perp (signorm v1)
    r = quadrance w / (2 * (w `dot` inward))
    centre = first + r *^ inward
    tolerance = 1e-9 * max 1 r
    near a b = norm (a - b) <= tolerance
    angleOf p = let V2 x y = p - centre in atan2 y x
    sweep p q = let a = angleOf q - angleOf p in if a <= 0 then a + 2 * pi else a
    -- The Bézier diagrams draws for an arc of at most a quarter turn
    tracesArc p q (Cubic c1 c2 _) =
        let s = sweep p q
            k = 4 / 3 * tan (s / 4)
         in s <= pi / 2 + 1e-9
                && abs (norm (q - centre) - r) <= tolerance
                && near c1 (k *^ perp (p - centre))
                && near (p + c2) (q - k *^ perp (q - centre))
    tracesArc _ _ _ = False
loopCircle _ _ = Nothing

-- | Render a single segment, converting offsets to absolute coordinates
renderSeg :: Segment Closed V2 Double -> RenderM ()
renderSeg (Linear (OffsetClosed off)) = do
    P (V2 cx cy) <- getPos
    let V2 dx dy = off
        newX = cx + dx
        newY = cy + dy
    emit $ CmdLineTo newX newY
    setPos (P (V2 newX newY))
renderSeg (Cubic (V2 c1x c1y) (V2 c2x c2y) (OffsetClosed (V2 dx dy))) = do
    P (V2 cx cy) <- getPos
    -- Control points are relative to current position
    let cp1x = cx + c1x
        cp1y = cy + c1y
        cp2x = cx + c2x
        cp2y = cy + c2y
        newX = cx + dx
        newY = cy + dy
    emit $ CmdBezierTo cp1x cp1y cp2x cp2y newX newY
    setPos (P (V2 newX newY))

instance Renderable (Text Double) CanvasJson where
    render _ (Text tr _al str) = R $ do
        emit CmdSave
        renderTransform tr
        sty <- gets _rsStyle
        emit $ CmdSetFont "16px sans-serif"
        case extractFillColor sty of
            Just (r, g, b, a) -> emit $ CmdFill r g b a
            Nothing -> return ()
        emit $ CmdFillText str 0 0
        emit CmdRestore

--------------------------------------------------------------------------------
-- Style Helpers
--------------------------------------------------------------------------------

{- | Extract fill color from style as RGBA (0-255 for RGB, 0-1 for alpha).
Multiplies color alpha by the FillOpacity attribute (default 1.0).
-}
extractFillColor :: Style V2 Double -> Maybe (Double, Double, Double, Double)
extractFillColor sty = do
    ft <- getAttr sty :: Maybe (FillTexture Double)
    SomeColor c <- toColor (getFillTexture ft)
    let (r, g, b, a) = colorToSRGBA c
        fo = view _fillOpacity sty
    Just (r * 255, g * 255, b * 255, a * fo)
  where
    toColor (SC (SomeColor c)) = Just (SomeColor c)
    toColor _ = Nothing

{- | Extract line color from style as RGBA.
Multiplies color alpha by the StrokeOpacity attribute (default 1.0).
-}
extractLineColor :: Style V2 Double -> Maybe (Double, Double, Double, Double)
extractLineColor sty = do
    lt <- getAttr sty :: Maybe (LineTexture Double)
    SomeColor c <- toColor (getLineTexture lt)
    let (r, g, b, a) = colorToSRGBA c
        so = view _strokeOpacity sty
    Just (r * 255, g * 255, b * 255, a * so)
  where
    toColor (SC (SomeColor c)) = Just (SomeColor c)
    toColor _ = Nothing

{- | Extract line width from style.
Uses the '_lineWidthU' lens which gives the unmeasured (resolved) value.
-}
extractLineWidth :: Style V2 Double -> Maybe Double
extractLineWidth sty = view _lineWidthU sty

--------------------------------------------------------------------------------
-- High-level Rendering
--------------------------------------------------------------------------------

-- | Render a diagram to a CanvasDiagram
renderCanvasJson ::
    CanvasJsonOptions ->
    QDiagram CanvasJson V2 Double Any ->
    CanvasDiagram
renderCanvasJson opts d =
    let sz = _canvasJsonSize opts
        V2 w h = sizeFromSpec 400 sz

        -- Get bounding box
        bb = boundingBox d
        bounds = case getCorners bb of
            Nothing -> BBox 0 0 0 0
            Just (P (V2 minX minY), P (V2 maxX maxY)) ->
                BBox minX minY maxX maxY

        -- Use toDTree + fromDTree to get an RTree with unresolved measures.
        -- We resolve measures ourselves in renderRTree' so we can classify
        -- line width measures as coordinate-space (local) vs view-relative (output).
        gToO = 1 :: Double -- global-to-output scale (identity since we use raw coords)
        nToO = sqrt (w * h) :: Double -- normalized-to-output scale (standard diagrams convention)
        rt = fromDTree $ fromMaybe (Node DEmpty []) $ toDTree gToO nToO d
        cmds = runRenderM (renderRTree' gToO nToO rt)
     in CanvasDiagram w h bounds cmds (_canvasJsonPrecision opts)

-- | Convert size spec to actual dimensions
sizeFromSpec :: Double -> SizeSpec V2 Double -> V2 Double
sizeFromSpec defSize spec = case getSpec spec of
    V2 (Just w) (Just h) -> V2 w h
    V2 (Just w) Nothing -> V2 w defSize
    V2 Nothing (Just h) -> V2 defSize h
    V2 Nothing Nothing -> V2 defSize defSize

--------------------------------------------------------------------------------
-- Command optimization
--------------------------------------------------------------------------------

{- | Optimize a command stream by collapsing consecutive Save\/Restore groups
that share the same context-setting commands into a single group, and by
stripping transparent fills and strokes that draw nothing.

Before optimization each primitive is wrapped in its own group:
@[S, B, M .., L .., F r g b a, LC 1, K r g b a lw, R]@ repeated many times.

After optimization, consecutive groups with identical context (fill color,
stroke color, line cap, line join, line dash, GCO) are merged into one group
where the context is set once using @FS@\/@KS@ and each primitive uses the
no-args @f@\/@k@ commands.
-}
optimizeCommands :: [CanvasCmd] -> [CanvasCmd]
optimizeCommands = mergeSaveRestore . stripTransparent

-- | Remove fills and strokes with zero alpha since they draw nothing.
stripTransparent :: [CanvasCmd] -> [CanvasCmd]
stripTransparent = filter (not . isTransparent)
  where
    isTransparent (CmdFill _ _ _ a) = a < 0.001
    isTransparent (CmdStroke _ _ _ a _) = a < 0.001
    isTransparent (CmdStrokeView _ _ _ a _) = a < 0.001
    isTransparent (CmdImageMask _ _ _ a _ _ _ _ _) = a < 0.001
    isTransparent _ = False

{- | Merge consecutive Save\/Restore groups that have identical context-setting
commands into a single group. Context commands (fill color, stroke color, line
cap, join, dash, GCO) are extracted and set once at the top of the merged group
using @CmdSetFillColor@\/@CmdSetStrokeColor@, and the per-primitive
@CmdFill@\/@CmdStroke@ are replaced with @CmdFillCurrent@\/@CmdStrokeCurrent@.
-}
mergeSaveRestore :: [CanvasCmd] -> [CanvasCmd]
mergeSaveRestore [] = []
mergeSaveRestore cmds@(x : xs) =
    case stripSaveRestore cmds of
        Nothing ->
            x : mergeSaveRestore xs
        Just (group, remaining) ->
            case extractContext group of
                Nothing ->
                    -- Non-uniform context: emit group unchanged
                    (CmdSave : group ++ [CmdRestore]) ++ mergeSaveRestore remaining
                Just ctx ->
                    let (merged, unmerged) = mergeWhileSameContext ctx [group] remaining
                     in merged ++ mergeSaveRestore unmerged

-- The groups merged so far are kept newest first and joined once, as
-- appending each group to the merged ones would copy them every time.
mergeWhileSameContext :: GroupContext -> [[CanvasCmd]] -> [CanvasCmd] -> ([CanvasCmd], [CanvasCmd])
mergeWhileSameContext ctx groups next =
    case stripSaveRestore next of
        Just (nextGroup, nextRemaining)
            | extractContext nextGroup == Just ctx ->
                mergeWhileSameContext ctx (nextGroup : groups) nextRemaining
        _ ->
            (CmdSave : (contextToSetCmds ctx ++ stripContext (concat (reverse groups)) ++ [CmdRestore]), next)

-- Strip a blanced CmdSave CmdRestore from the command stream.
--
-- If the command stream starts with a CmdSave strip it and its balanced CmdRestore skipping
-- over inner CmdSave and CmdRestore pairs returning
stripSaveRestore :: [CanvasCmd] -> Maybe ([CanvasCmd], [CanvasCmd])
stripSaveRestore (CmdSave : xs) = go (1 :: Int) [] xs
  where
    go _ _ [] = Nothing
    go 1 ys (CmdRestore : xs') = Just (reverse ys, xs')
    go d ys (CmdSave : xs') = go (d + 1) (CmdSave : ys) xs'
    go d ys (CmdRestore : xs') = go (d - 1) (CmdRestore : ys) xs'
    go d ys (x' : xs') = go d (x' : ys) xs'
stripSaveRestore _ = Nothing

-- | Whether a stroke uses coordinate-space or view-relative line widths.
data StrokeMode = StrokeCoord | StrokeView
    deriving (Show, Eq)

-- | Context extracted from a Save/Restore group for comparison.
data GroupContext = GroupContext
    { gcFill :: Maybe (Double, Double, Double, Double)
    , gcStroke :: Maybe (StrokeMode, Double, Double, Double, Double, Double)
    , gcLineCap :: Maybe Int
    , gcLineJoin :: Maybe Int
    , gcLineDash :: Maybe (StrokeMode, [Double])
    , gcGCO :: Maybe CompositeOp
    }
    deriving (Eq)

{- | Extract uniform context from a group's body (with the outer
CmdSave\/CmdRestore already stripped), skipping inner CmdSave\/CmdRestore
sections. Returns 'Nothing' if:

* Any context field has conflicting values (e.g. two different fill colours).
* A context-setting command appears after a draw it would affect — line
  styling (cap, join, dash) after a stroke draw, or GCO after any draw —
  because hoisting such a command would change the semantics of earlier draws.
-}
extractContext :: [CanvasCmd] -> Maybe GroupContext
extractContext = go False False emptyCtx . skipSaveRestore (0 :: Int)
  where
    emptyCtx = GroupContext Nothing Nothing Nothing Nothing Nothing Nothing

    -- go seenFill seenStroke ctx cmds
    go _ _ ctx [] = Just ctx
    -- Fill draws
    go _ ss ctx (CmdFill r g b a : rest)
        | isUniform (gcFill ctx) (r, g, b, a) = go True ss ctx{gcFill = Just (r, g, b, a)} rest
        | otherwise = Nothing
    -- An image mask carries its own colour, so it is a fill draw without context
    go _ ss ctx (CmdImageMask{} : rest) = go True ss ctx rest
    -- Stroke draws
    go sf _ ctx (CmdStroke r g b a lw' : rest)
        | isUniform (gcStroke ctx) (StrokeCoord, r, g, b, a, lw') = go sf True ctx{gcStroke = Just (StrokeCoord, r, g, b, a, lw')} rest
        | otherwise = Nothing
    go sf _ ctx (CmdStrokeView r g b a lw' : rest)
        | isUniform (gcStroke ctx) (StrokeView, r, g, b, a, lw') = go sf True ctx{gcStroke = Just (StrokeView, r, g, b, a, lw')} rest
        | otherwise = Nothing
    -- GCO affects both fills and strokes: reject after any draw
    go sf ss ctx (CmdSetGlobalCompositeOperation gco : rest)
        | sf || ss = Nothing
        | isUniform (gcGCO ctx) gco = go sf ss ctx{gcGCO = Just gco} rest
        | otherwise = Nothing
    -- Line styling only affects strokes: reject after a stroke draw
    go sf ss ctx (CmdSetLineCap c : rest)
        | ss = Nothing
        | isUniform (gcLineCap ctx) c = go sf ss ctx{gcLineCap = Just c} rest
        | otherwise = Nothing
    go sf ss ctx (CmdSetLineJoin j : rest)
        | ss = Nothing
        | isUniform (gcLineJoin ctx) j = go sf ss ctx{gcLineJoin = Just j} rest
        | otherwise = Nothing
    go sf ss ctx (CmdSetLineDash ds : rest)
        | ss = Nothing
        | isUniform (gcLineDash ctx) (StrokeCoord, ds) = go sf ss ctx{gcLineDash = Just (StrokeCoord, ds)} rest
        | otherwise = Nothing
    go sf ss ctx (CmdSetLineDashView ds : rest)
        | ss = Nothing
        | isUniform (gcLineDash ctx) (StrokeView, ds) = go sf ss ctx{gcLineDash = Just (StrokeView, ds)} rest
        | otherwise = Nothing
    -- Non-context, non-draw: pass through
    go sf ss ctx (_ : rest) = go sf ss ctx rest

    isUniform :: (Eq a) => Maybe a -> a -> Bool
    isUniform Nothing _ = True
    isUniform (Just old) val = old == val

    skipSaveRestore _ [] = []
    skipSaveRestore d (CmdRestore : xs) = skipSaveRestore (d - 1) xs
    skipSaveRestore d (CmdSave : xs) = skipSaveRestore (d + 1) xs
    skipSaveRestore d (x : xs)
        | d > 0 = skipSaveRestore d xs
        | otherwise = x : skipSaveRestore d xs

-- | Convert a GroupContext to setup commands using the set-only variants.
contextToSetCmds :: GroupContext -> [CanvasCmd]
contextToSetCmds ctx =
    maybe [] (\(r, g, b, a) -> [CmdSetFillColor r g b a]) (gcFill ctx)
        ++ maybe
            []
            ( \case
                (StrokeCoord, r, g, b, a, lw') -> [CmdSetStrokeColor r g b a lw']
                (StrokeView, r, g, b, a, lw') -> [CmdSetStrokeColorView r g b a lw']
            )
            (gcStroke ctx)
        ++ maybe [] (\c -> [CmdSetLineCap c]) (gcLineCap ctx)
        ++ maybe [] (\j -> [CmdSetLineJoin j]) (gcLineJoin ctx)
        ++ maybe
            []
            ( \case
                (StrokeCoord, ds) -> [CmdSetLineDash ds]
                (StrokeView, ds) -> [CmdSetLineDashView ds]
            )
            (gcLineDash ctx)
        ++ maybe [] (\gco -> [CmdSetGlobalCompositeOperation gco]) (gcGCO ctx)

{- | Strip context-setting commands from a group body with the outer CmdSave CmdRestore stipped of and not stripping commands from any inner CmdSave CmdRestore sections

When stripping context-setting commands replace inline Fill/Stroke with FillCurrent/StrokeCurrent.
-}
stripContext :: [CanvasCmd] -> [CanvasCmd]
stripContext = go (0 :: Int)
  where
    go _ [] = []
    -- track entering leaving CmdSave CmdRestore sections
    go d (CmdSave : xs) = CmdSave : go (d + 1) xs
    go d (CmdRestore : xs) = CmdRestore : go (d - 1) xs
    -- modify the stream since we are not inside a CmdSave CmdRestore section
    go 0 (x : xs) =
        case x of
            -- remapped command
            CmdFill{} -> CmdFillCurrent : go 0 xs
            CmdStroke{} -> CmdStrokeCurrent : go 0 xs
            CmdStrokeView{} -> CmdStrokeCurrent : go 0 xs
            -- filtered out command so skip
            CmdSetLineCap _ -> go 0 xs
            CmdSetLineJoin _ -> go 0 xs
            CmdSetLineDash _ -> go 0 xs
            CmdSetLineDashView _ -> go 0 xs
            CmdSetGlobalCompositeOperation _ -> go 0 xs
            -- not filtered out command so keep
            _ -> x : go 0 xs
    -- inside an inner CmdSave CmdRestore group so don't modify the stream
    go d (x : xs) = x : go d xs
