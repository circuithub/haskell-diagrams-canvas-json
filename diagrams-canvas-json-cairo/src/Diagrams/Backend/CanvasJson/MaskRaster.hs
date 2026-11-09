{-# LANGUAGE OverloadedStrings #-}

{- | Rasterise a command stream into @IM@ tiles, so a layer of hundreds of
thousands of vector commands becomes a few kilobytes of PNG.

The pixel grid is anchored at the diagram origin, so every raster at the
same pitch on a board shares one grid. Pixel @(i, j)@ (column, PNG row) of
a tile covers @[x0 + i*p, x0 + (i+1)*p] x [y1 - (j+1)*p, y1 - j*p]@.
-}
module Diagrams.Backend.CanvasJson.MaskRaster (
    MaskRasterSpec (..),
    defaultMaskRasterSpec,
    MaskGrid (..),
    planMaskGrid,
    MaskTile (..),
    rasteriseMask,
    maskTileCommands,
) where

import Control.Exception (IOException, evaluate, handle)
import Data.ByteString qualified as BS
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Diagrams.Backend.CanvasJson (BBox (..), CanvasCmd (..), MaskPng, imageMaskRectDecimals, maxMaskPngSide)
import Diagrams.Backend.CanvasJson.Cairo (runCommands, withMaskSurfaces)
import Diagrams.Backend.CanvasJson.MaskPng (MaskDecodeError (..), encodeMaskPng, packMaskRows, withSurfacePixels)
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Matrix (Matrix (..))

data MaskRasterSpec = MaskRasterSpec
    { mrsPitches :: ![Double]
    {- ^ Candidate pixel pitches, finest first; the first that fits is used.
    Each must be a whole multiple of @10^-'imageMaskRectDecimals'@, so the
    tile edges survive JSON encoding with square pixels.
    -}
    , mrsMaxPixels :: !Int
    -- ^ Pixel budget for the whole raster, which bounds viewer memory.
    , mrsMaxTile :: !Int
    -- ^ Longest tile side, at most 'maxMaskPngSide'.
    , mrsThreshold :: !Word8
    -- ^ Minimum antialiased A8 coverage of a set pixel.
    }
    deriving (Show, Eq)

{- | 25-100 um ladder, 32 Mpx, 4096 px tiles, 25% coverage. The low threshold
dilates by under half a pixel so holes near the pitch survive.
-}
defaultMaskRasterSpec :: MaskRasterSpec
defaultMaskRasterSpec =
    MaskRasterSpec
        { mrsPitches = [0.025, 0.050, 0.075, 0.100]
        , mrsMaxPixels = 32_000_000
        , mrsMaxTile = 4096
        , mrsThreshold = 64
        }

-- | The snapped grid: pitch, origin cell and size in pixels.
data MaskGrid = MaskGrid
    { mgPitch :: !Double
    , mgCol0 :: !Int
    -- ^ Column index of the left edge, @x0 = mgCol0 * mgPitch@.
    , mgRow1 :: !Int
    -- ^ Row index of the top edge, @y1 = mgRow1 * mgPitch@.
    , mgWidth :: !Int
    , mgHeight :: !Int
    }
    deriving (Show, Eq)

-- | Pick the finest pitch whose grid over the bounds fits the budget.
planMaskGrid :: MaskRasterSpec -> BBox -> Either Text MaskGrid
planMaskGrid spec (BBox minX minY maxX maxY)
    | not (all finite [minX, minY, maxX, maxY]) || maxX < minX || maxY < minY =
        Left "mask bounds must be finite and non-empty"
    | mrsMaxTile spec < 2 || mrsMaxTile spec > maxMaskPngSide =
        Left ("mask tiles must be 2 to " <> T.pack (show maxMaskPngSide) <> " px")
    | any (\p -> not (finite p) || p <= 0) (mrsPitches spec) = Left "mask pitches must be positive"
    | not (all onWireGrid (mrsPitches spec)) =
        Left ("mask pitches must be whole multiples of 1e-" <> T.pack (show imageMaskRectDecimals))
    | otherwise = case find fits (map grid (mrsPitches spec)) of
        Just g -> Right g
        Nothing -> Left "no mask pitch fits the pixel budget"
  where
    finite x = not (isNaN x || isInfinite x)
    onWireGrid p =
        let u = p * 10 ^^ imageMaskRectDecimals
         in abs (u - fromIntegral (round u :: Integer)) <= 1e-9 * u
    grid p =
        let c0 = floor (minX / p)
            c1 = max (c0 + 1) (ceiling (maxX / p))
            r0 = floor (minY / p)
            r1 = max (r0 + 1) (ceiling (maxY / p))
         in MaskGrid p c0 r1 (c1 - c0) (r1 - r0)
    -- 32767 is cairo's image side limit for the single render surface.
    fits g =
        mgWidth g <= 32767
            && mgHeight g <= 32767
            && toInteger (mgWidth g) * toInteger (mgHeight g) <= toInteger (mrsMaxPixels spec)

data MaskTile = MaskTile
    { mtRect :: !BBox
    , mtPng :: !MaskPng
    }
    deriving (Show, Eq)

{- | Render the commands once, antialiased, onto one A8 surface of the grid
over the bounds, then cut it into tiles of at most 'mrsMaxTile' px. Tiles
share their edge row or column, so their rectangles overlap by one pixel and
smoothed edges leave no seam. All-empty tiles are omitted. 'Left' when no
pitch fits or a nested @IM@ payload fails to decode; the caller stays vector.
-}
rasteriseMask :: MaskRasterSpec -> BBox -> [CanvasCmd] -> IO (Either Text [MaskTile])
rasteriseMask spec bounds cmds = case planMaskGrid spec bounds of
    Left err -> pure (Left err)
    Right g ->
        handle (\(MaskDecodeError e) -> pure (Left (T.pack e))) $
            handle (\e -> pure (Left (T.pack (show (e :: IOException))))) $
                Right <$> renderTiles spec g cmds

renderTiles :: MaskRasterSpec -> MaskGrid -> [CanvasCmd] -> IO [MaskTile]
renderTiles spec g cmds =
    withMaskSurfaces cmds $ \masks ->
        C.withImageSurface C.FormatA8 w h $ \surf -> do
            -- An unallocated surface would draw nothing and read out of bounds.
            withSurfacePixels surf $ \_ _ -> pure ()
            C.renderWith surf $ do
                C.setOperator C.OperatorClear
                C.paint
                C.setOperator C.OperatorOver
                -- User (x, y) to pixel ((x - x0) / p, (y1 - y) / p).
                C.setMatrix (Matrix (1 / p) 0 0 (-(1 / p)) (-(fromIntegral (mgCol0 g))) (fromIntegral (mgRow1 g)))
                runCommands masks (1 / p) cmds
            -- Every tile is forced while the buffer is alive.
            withSurfacePixels surf $ \stride dat ->
                catMaybes
                    <$> sequence
                        [ evaluate (tileAt stride dat c cw r rh)
                        | (r, rh) <- spans h
                        , (c, cw) <- spans w
                        ]
  where
    p = mgPitch g
    w = mgWidth g
    h = mgHeight g
    t = mrsMaxTile spec
    spans n = go 0
      where
        go s
            | s + t >= n = [(s, n - s)]
            | otherwise = (s, t) : go (s + t - 1)
    tileAt stride dat c cw r rh =
        let rows = packMaskRows (mrsThreshold spec) stride dat c r cw rh
            cell i = fromIntegral i * p
            rect =
                BBox
                    (cell (mgCol0 g + c))
                    (cell (mgRow1 g - r - rh))
                    (cell (mgCol0 g + c + cw))
                    (cell (mgRow1 g - r))
         in if all (BS.all (== 0)) rows
                then Nothing
                else Just $! MaskTile rect (encodeMaskPng cw rows)

-- | Each tile as a black source-over @IM@ in its own save group.
maskTileCommands :: [MaskTile] -> [CanvasCmd]
maskTileCommands tiles =
    concat
        [ [CmdSave, CmdImageMask 0 0 0 1 x0 y0 x1 y1 png, CmdRestore]
        | MaskTile (BBox x0 y0 x1 y1) png <- tiles
        ]
