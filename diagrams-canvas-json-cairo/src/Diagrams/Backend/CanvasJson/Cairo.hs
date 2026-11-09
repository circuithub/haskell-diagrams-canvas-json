{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Render the JSON command stream produced by
'Diagrams.Backend.CanvasJson' — and multi-layer images built on top of
it — to raster or vector images using @gi-cairo-render@.

The goal is to produce an image that matches what the browser viewer
renders: the same fit-bounds framing, the same mask-layer tint
semantics (white-on-transparent commands tinted to the layer colour via
@source-in@), and the same per-layer isolation for @destination-out@ /
@destination-in@ compositing.

Supported output formats are PNG and SVG (via native cairo surfaces) and
JPEG (via JuicyPixels re-encoding of the rendered pixel buffer).
-}
module Diagrams.Backend.CanvasJson.Cairo (
    -- * Image options
    ImageFormat (..),
    Background (..),
    ImageOptions (..),
    defaultImageOptions,
    formatFromExtension,

    -- * Rendering
    renderCanvasDiagramTo,
    renderLayeredDiagramTo,

    -- * Low level
    MaskSurfaces,
    withMaskSurfaces,
    runCommands,
    executeCommands,
) where

import Codec.Picture qualified as JP
import Codec.Picture.Types qualified as JPT
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, execStateT, get, gets, lift, modify, put)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Word (Word8)
import Diagrams.Backend.CanvasJson (
    BBox (..),
    CanvasCmd (..),
    CanvasDiagram (..),
    CompositeOp (..),
    LayeredDiagram (..),
    MaskLayer (..),
    MaskPng,
    maskPngHeight,
    maskPngWidth,
    punchedLayers,
 )
import Diagrams.Backend.CanvasJson.MaskPng (MaskDecodeError (..), withMaskSurface)
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Matrix (Matrix (..))
import System.FilePath (takeExtension)

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

-- | Supported output formats.
data ImageFormat
    = -- | PNG raster via cairo's image surface.
      FormatPNG
    | -- | SVG vector via cairo's SVG surface.
      FormatSVG
    | -- | JPEG raster — rendered to an image surface and re-encoded via JuicyPixels.
      FormatJPEG
    deriving (Show, Eq)

-- | Page background. Mirrors the viewer's @background@ option.
data Background
    = -- | No background fill — useful for SVG or PNG that will be composited.
      BackgroundTransparent
    | {- | Flat-colour background. Components are RGB 0-255 and alpha 0-1 to
      match the rest of the command stream.
      -}
      BackgroundSolid !Double !Double !Double !Double
    deriving (Show, Eq)

{- | Rendering configuration. The defaults ('defaultImageOptions') match the
viewer's initial framing — fit bounds with 10% padding, white background.
-}
data ImageOptions = ImageOptions
    { ioWidth :: !Int
    -- ^ Output width in pixels (SVG) or pixels (PNG\/JPEG).
    , ioHeight :: !Int
    -- ^ Output height in the same units.
    , ioPadding :: !Double
    -- ^ Fit-bounds padding factor (@0.9@ leaves 10% empty).
    , ioBackground :: !Background
    , ioJpegQuality :: !Int
    -- ^ JPEG quality 1..100 (ignored for other formats).
    , ioMirrorH :: !Bool
    -- ^ Mirror the image horizontally (flip left\/right).
    , ioMirrorV :: !Bool
    -- ^ Mirror the image vertically (flip top\/bottom).
    }
    deriving (Show, Eq)

-- | White background, 800x800, 10% padding, JPEG quality 85, no mirror.
defaultImageOptions :: ImageOptions
defaultImageOptions =
    ImageOptions
        { ioWidth = 800
        , ioHeight = 800
        , ioPadding = 0.9
        , ioBackground = BackgroundSolid 255 255 255 1
        , ioJpegQuality = 85
        , ioMirrorH = False
        , ioMirrorV = False
        }

{- | Guess an 'ImageFormat' from a file extension (@".png"@, @".svg"@, @".jpg"@,
@".jpeg"@, case-insensitive).
-}
formatFromExtension :: FilePath -> Maybe ImageFormat
formatFromExtension path = case map toLower (takeExtension path) of
    ".png" -> Just FormatPNG
    ".svg" -> Just FormatSVG
    ".jpg" -> Just FormatJPEG
    ".jpeg" -> Just FormatJPEG
    _ -> Nothing

--------------------------------------------------------------------------------
-- Top-level renderers
--------------------------------------------------------------------------------

-- | Render a single-layer 'CanvasDiagram' to an image.
renderCanvasDiagramTo :: ImageFormat -> FilePath -> ImageOptions -> CanvasDiagram -> IO ()
renderCanvasDiagramTo fmt path opts cd =
    withMaskSurfaces (cdCommands cd) $ \masks ->
        withSurface fmt path opts $ \surf -> C.renderWith surf $ do
            drawBackground opts
            let bounds = cdBounds cd
                scale = fitScale opts bounds
            C.save
            applyFitTransform opts bounds
            runCommands masks scale (cdCommands cd)
            C.restore

{- | Render a 'LayeredDiagram' — each layer is drawn in isolation (so its
@destination-out@\/@destination-in@ commands only affect that layer's
pixels), then tinted with @source-in@ using its layer colour, then painted
back onto the page surface. This matches the web viewer's mask-layer
semantics pixel-for-pixel.
-}
renderLayeredDiagramTo :: ImageFormat -> FilePath -> ImageOptions -> LayeredDiagram -> IO ()
renderLayeredDiagramTo fmt path opts mld =
    -- The punch is drawn into every layer, so its rasters are decoded once;
    -- a layer's own are decoded only while it draws, bounding the peak.
    withMaskSurfaces (ldPunch mld) $ \punchMasks ->
        withSurface fmt path opts $ \surf -> do
            C.renderWith surf (drawBackground opts)
            let bounds = ldBounds mld
                scale = fitScale opts bounds
            for_ (punchedLayers mld) $ \layer ->
                extendMaskSurfaces punchMasks (mlCommands layer) $ \masks ->
                    C.renderWith surf $ do
                        C.pushGroup
                        C.save
                        applyFitTransform opts bounds
                        runCommands masks scale (mlCommands layer)
                        C.restore
                        tintLayer opts (mlColor layer)
                        C.popGroupToSource
                        C.save
                        C.setOperator C.OperatorOver
                        C.paint
                        C.restore

--------------------------------------------------------------------------------
-- Surface setup
--------------------------------------------------------------------------------

withSurface :: ImageFormat -> FilePath -> ImageOptions -> (C.Surface -> IO ()) -> IO ()
withSurface FormatPNG path opts body =
    C.withImageSurface C.FormatARGB32 (ioWidth opts) (ioHeight opts) $ \surf -> do
        body surf
        C.surfaceWriteToPNG surf path
withSurface FormatSVG path opts body =
    C.withSVGSurface
        path
        (fromIntegral (ioWidth opts))
        (fromIntegral (ioHeight opts))
        $ \surf -> do
            body surf
            -- Force cairo to flush pending draws and close out the SVG file
            -- before 'withSVGSurface' tears it down — on at least some setups
            -- the bracket alone leaves a 0-byte file.
            C.surfaceFinish surf
withSurface FormatJPEG path opts body =
    C.withImageSurface C.FormatARGB32 (ioWidth opts) (ioHeight opts) $ \surf -> do
        body surf
        bytes <- imageSurfaceToJpeg surf (ioWidth opts) (ioHeight opts) (ioJpegQuality opts)
        BL.writeFile path bytes

{- | Extract the pixel buffer from a cairo ARGB32 image surface and encode it
as JPEG. Cairo ARGB32 stores pixels as 32-bit native-endian words, which is
BGRA byte order on little-endian systems.
-}
imageSurfaceToJpeg :: C.Surface -> Int -> Int -> Int -> IO BL.ByteString
imageSurfaceToJpeg surf w h quality = do
    raw <- C.imageSurfaceGetData surf
    stride <- C.imageSurfaceGetStride surf
    let
        -- Flatten alpha over white so JPEG (which has no alpha) matches the
        -- visible viewer background.
        pixelAt x y =
            let base = y * stride + x * 4
                b = raw `BS.index` base
                g = raw `BS.index` (base + 1)
                r = raw `BS.index` (base + 2)
                a = raw `BS.index` (base + 3)
                af = fromIntegral a / 255.0 :: Double
                composite ch =
                    let chf = fromIntegral ch / 255.0 :: Double
                        -- Cairo pre-multiplies by alpha, so un-premultiply first.
                        chUn = if af > 0 then chf / af else 0
                        mixed = chUn * af + 1.0 * (1.0 - af)
                     in toWord8 mixed
             in JP.PixelRGB8 (composite r) (composite g) (composite b)
        img = JP.generateImage pixelAt w h
    pure (JP.encodeJpegAtQuality (fromIntegral quality) (JPT.convertImage img))

toWord8 :: Double -> Word8
toWord8 x = round (max 0 (min 1 x) * 255)

--------------------------------------------------------------------------------
-- Background + transform helpers
--------------------------------------------------------------------------------

drawBackground :: ImageOptions -> C.Render ()
drawBackground opts = case ioBackground opts of
    BackgroundTransparent -> pure ()
    BackgroundSolid r g b a -> do
        C.save
        C.setOperator C.OperatorSource
        C.setSourceRGBA (r / 255) (g / 255) (b / 255) a
        C.rectangle 0 0 (fromIntegral (ioWidth opts)) (fromIntegral (ioHeight opts))
        C.fill
        C.restore

-- | View scale used to fit the diagram bounds into the output dimensions.
fitScale :: ImageOptions -> BBox -> Double
fitScale opts (BBox minX minY maxX maxY) =
    let dw = maxX - minX
        dh = maxY - minY
        w = fromIntegral (ioWidth opts)
        h = fromIntegral (ioHeight opts)
        p = ioPadding opts
     in if dw > 0 && dh > 0
            then min (w * p / dw) (h * p / dh)
            else 1

{- | Apply the fit-bounds transform: scale so the diagram fills the viewport
with 'ioPadding' space, flip Y (canvas Y is down, diagram Y is up), translate
so the diagram centre is at the viewport centre.
-}
applyFitTransform :: ImageOptions -> BBox -> C.Render ()
applyFitTransform opts bounds@(BBox minX minY maxX maxY) = do
    let scale = fitScale opts bounds
        dcx = (minX + maxX) / 2
        dcy = (minY + maxY) / 2
        w = fromIntegral (ioWidth opts) :: Double
        h = fromIntegral (ioHeight opts) :: Double
        sx = if ioMirrorH opts then negate scale else scale
        sy = if ioMirrorV opts then scale else negate scale
        tx = w / 2 - dcx * sx
        ty = h / 2 - dcy * sy
    C.transform (Matrix sx 0 0 sy tx ty)

{- | Replace all opaque pixels of the current group with the given colour,
preserving alpha. Mirrors the Canvas 2D @source-in@ tint in 'viewer.ts'.
-}
tintLayer :: ImageOptions -> (Double, Double, Double, Double) -> C.Render ()
tintLayer opts (r, g, b, a) = do
    C.save
    C.identityMatrix
    C.setOperator C.OperatorIn
    C.setSourceRGBA (r / 255) (g / 255) (b / 255) a
    C.rectangle 0 0 (fromIntegral (ioWidth opts)) (fromIntegral (ioHeight opts))
    C.fill
    C.restore

--------------------------------------------------------------------------------
-- Command interpreter
--------------------------------------------------------------------------------

{- | Per-context drawing state that cairo doesn't track for us (cairo has one
source pattern, the command stream needs separate fill and stroke colours
plus a line width for the @f@\/@k@ "use current style" commands). We carry a
stack that shadows cairo's own Save\/Restore stack.
-}
data DrawState = DrawState
    { dsFill :: !(Maybe (Double, Double, Double, Double))
    , dsStroke :: !(Maybe (Double, Double, Double, Double))
    , dsLineWidth :: !Double
    }
    deriving (Show, Eq)

initialDrawState :: DrawState
initialDrawState = DrawState Nothing Nothing 1

type Interp = StateT [DrawState] C.Render

updateHead :: (DrawState -> DrawState) -> Interp ()
updateHead f = modify $ \case
    (s : ss) -> f s : ss
    [] -> [f initialDrawState]

-- | Decoded @IM@ rasters, keyed by payload.
type MaskSurfaces = Map.Map MaskPng C.Surface

{- | Decode every distinct @IM@ payload in the commands for the duration of
the action. Throws 'MaskDecodeError' for a payload outside the profile, as
drawing the board without it would be wrong.
-}
withMaskSurfaces :: [CanvasCmd] -> (MaskSurfaces -> IO a) -> IO a
withMaskSurfaces = extendMaskSurfaces Map.empty

-- | 'withMaskSurfaces' reusing the payloads already decoded.
extendMaskSurfaces :: MaskSurfaces -> [CanvasCmd] -> (MaskSurfaces -> IO a) -> IO a
extendMaskSurfaces decoded cmds act = go (Map.keys pngs) decoded
  where
    pngs = Map.fromList [(png, ()) | CmdImageMask _ _ _ _ _ _ _ _ png <- cmds, Map.notMember png decoded]
    go [] acc = act acc
    go (png : rest) acc = do
        r <- withMaskSurface png $ \surf -> go rest (Map.insert png surf acc)
        either (throwIO . MaskDecodeError) pure r

{- | Run a command stream from a fresh drawing state, then restore any saves
it left unmatched.
-}
runCommands :: MaskSurfaces -> Double -> [CanvasCmd] -> C.Render ()
runCommands masks scale cmds = do
    finalStack <- execStateT (executeCommands masks scale cmds) [initialDrawState]
    for_ [2 .. length finalStack] $ const C.restore

{- | Interpret a command stream onto the cairo context. The @scale@ is the
current view scale and is used to divide the values of the view-relative
line width and dash commands (@KV@, @KSV@, @LDV@). Every @IM@ payload must
be in the 'MaskSurfaces'.
-}
executeCommands :: MaskSurfaces -> Double -> [CanvasCmd] -> Interp ()
executeCommands masks scale = mapM_ (executeCmd masks scale)

executeCmd :: MaskSurfaces -> Double -> CanvasCmd -> Interp ()
executeCmd masks scale = \case
    CmdSave -> do
        lift C.save
        st <- get
        put (case st of s : _ -> s : st; [] -> [initialDrawState])
    CmdRestore -> do
        lift C.restore
        modify $ \case
            _ : ss@(_ : _) -> ss
            ss -> ss
    CmdTransform a b c d e f ->
        lift (C.transform (Matrix a b c d e f))
    CmdBeginPath -> lift C.newPath
    CmdMoveTo x y -> lift (C.moveTo x y)
    CmdLineTo x y -> lift (C.lineTo x y)
    CmdBezierTo x1 y1 x2 y2 x y ->
        lift (C.curveTo x1 y1 x2 y2 x y)
    CmdQuadTo cx cy x y -> lift $ do
        -- Convert quadratic to cubic bezier: cairo has no native quad.
        (curX, curY) <- C.getCurrentPoint
        let cp1x = curX + 2 / 3 * (cx - curX)
            cp1y = curY + 2 / 3 * (cy - curY)
            cp2x = x + 2 / 3 * (cx - x)
            cp2y = y + 2 / 3 * (cy - y)
        C.curveTo cp1x cp1y cp2x cp2y x y
    CmdArc cx cy r sa ea ->
        lift $
            if ea >= sa
                then C.arc cx cy r sa ea
                else C.arcNegative cx cy r sa ea
    CmdClosePath -> lift C.closePath
    CmdFill r g b a -> do
        lift (setSourceRGBA255 r g b a)
        lift C.fill
        updateHead (\s -> s{dsFill = Just (r, g, b, a)})
    CmdStroke r g b a lw -> do
        lift (setSourceRGBA255 r g b a)
        lift (C.setLineWidth lw)
        lift C.stroke
        updateHead (\s -> s{dsStroke = Just (r, g, b, a), dsLineWidth = lw})
    CmdStrokeView r g b a lw -> do
        let lw' = lw / scale
        lift (setSourceRGBA255 r g b a)
        lift (C.setLineWidth lw')
        lift C.stroke
        updateHead (\s -> s{dsStroke = Just (r, g, b, a), dsLineWidth = lw'})
    CmdSetFillColor r g b a ->
        updateHead (\s -> s{dsFill = Just (r, g, b, a)})
    CmdSetStrokeColor r g b a lw ->
        updateHead (\s -> s{dsStroke = Just (r, g, b, a), dsLineWidth = lw})
    CmdSetStrokeColorView r g b a lw ->
        updateHead (\s -> s{dsStroke = Just (r, g, b, a), dsLineWidth = lw / scale})
    CmdFillCurrent -> do
        mf <- gets (fmap dsFill . safeHead)
        case mf of
            Just (Just (r, g, b, a)) -> do
                lift (setSourceRGBA255 r g b a)
                lift C.fill
            _ -> pure ()
    CmdStrokeCurrent -> do
        ms <- gets safeHead
        case ms of
            Just s | Just (r, g, b, a) <- dsStroke s -> do
                lift (setSourceRGBA255 r g b a)
                lift (C.setLineWidth (dsLineWidth s))
                lift C.stroke
            _ -> pure ()
    CmdSetLineCap c -> lift (C.setLineCap (toLineCap c))
    CmdSetLineJoin j -> lift (C.setLineJoin (toLineJoin j))
    CmdSetLineDash ds -> lift (C.setDash ds 0)
    CmdSetLineDashView ds -> lift (C.setDash (map (/ scale) ds) 0)
    CmdFillText t x y -> do
        mf <- gets (fmap dsFill . safeHead)
        lift $ do
            C.save
            -- Counter the Y-flip so glyph orientation is correct.
            C.translate x y
            C.scale 1 (-1)
            case mf of
                Just (Just (r, g, b, a)) -> setSourceRGBA255 r g b a
                _ -> pure ()
            C.moveTo 0 0
            C.showText t
            C.restore
    CmdSetFont font -> lift $ do
        -- Minimal parser: accept "NNpx family" and use the size; ignore the
        -- rest. Unmatched strings fall back to 16px sans-serif.
        let (sizePx, family) = parseCssFont font
        C.selectFontFace family C.FontSlantNormal C.FontWeightNormal
        C.setFontSize sizePx
    CmdSetGlobalCompositeOperation op -> lift $ case op of
        SourceOver -> C.setOperator C.OperatorOver
        DestinationOut -> C.setOperator C.OperatorDestOut
        DestinationIn -> C.setOperator C.OperatorDestIn
    CmdImageMask r g b a x0 y0 x1 y1 png -> case Map.lookup png masks of
        Nothing -> liftIO (throwIO (MaskDecodeError "IM payload was not decoded before the render"))
        Just surf -> lift $ do
            let p = (x1 - x0) / fromIntegral (maskPngWidth png)
            C.save
            Matrix xx yx xy yy _ _ <- C.getMatrix
            -- Device pixels per raster pixel: crisp squares when magnified,
            -- filtered when minified so sub-pixel holes still show.
            let k = sqrt (abs (xx * yy - xy * yx)) * p
            setSourceRGBA255 r g b a
            -- Row 0 is at y1; the negative scale undoes the view's Y flip.
            C.translate x0 y1
            C.scale p (negate ((y1 - y0) / fromIntegral (maskPngHeight png)))
            -- The operator is left as set by GCO, so destination-out punches.
            C.withPatternForSurface surf $ \pat -> do
                C.patternSetFilter pat (if k > 1 then C.FilterNearest else C.FilterGood)
                C.mask pat
            C.restore

setSourceRGBA255 :: Double -> Double -> Double -> Double -> C.Render ()
setSourceRGBA255 r g b a = C.setSourceRGBA (r / 255) (g / 255) (b / 255) a

toLineCap :: Int -> C.LineCap
toLineCap 1 = C.LineCapRound
toLineCap 2 = C.LineCapSquare
toLineCap _ = C.LineCapButt

toLineJoin :: Int -> C.LineJoin
toLineJoin 1 = C.LineJoinRound
toLineJoin 2 = C.LineJoinBevel
toLineJoin _ = C.LineJoinMiter

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

{- | Parse a CSS font string like @"16px sans-serif"@ into @(size, family)@.
Only extracts the leading @Npx@ size; the rest becomes the family. Returns
@(16, "sans-serif")@ for anything it can't parse.
-}
parseCssFont :: String -> (Double, String)
parseCssFont s = case words s of
    (sizeTok : rest)
        | Just sz <- stripPx sizeTok ->
            (sz, if null rest then "sans-serif" else unwords rest)
    _ -> (16, "sans-serif")
  where
    stripPx tok = case reverse tok of
        'x' : 'p' : rest ->
            case reads (reverse rest) :: [(Double, String)] of
                [(n, "")] -> Just n
                _ -> Nothing
        _ -> Nothing
