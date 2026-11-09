{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.Picture qualified as JP
import Control.Exception (ErrorCall, IOException, bracket, evaluate, try)
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Bits (setBit, testBit)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word8)
import Diagrams.Backend.CanvasJson (
    BBox (..),
    CanvasCmd (..),
    CanvasDiagram (..),
    CompositeOp (..),
    JsonPrecision (..),
    LayeredDiagram (..),
    MaskLayer (..),
    MaskPng (..),
    defaultJsonPrecision,
    encodeCmd,
    maskPngBase64,
    parseMaskPng,
 )
import Diagrams.Backend.CanvasJson.Cairo (
    Background (..),
    ImageFormat (..),
    ImageOptions (..),
    defaultImageOptions,
    renderLayeredDiagramTo,
    runCommands,
    withMaskSurfaces,
 )
import Diagrams.Backend.CanvasJson.MaskPng (
    decodeMaskRows,
    encodeMaskPng,
    packMaskRows,
    pngFromScanlines,
    withMaskSurface,
    withSurfacePixels,
 )
import Diagrams.Backend.CanvasJson.MaskRaster (
    MaskGrid (..),
    MaskRasterSpec (..),
    MaskTile (..),
    defaultMaskRasterSpec,
    maskTileCommands,
    planMaskGrid,
    rasteriseMask,
 )
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Internal qualified as CI
import GI.Cairo.Render.Matrix (Matrix (..))
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main =
    defaultMain $
        testGroup
            "diagrams-canvas-json-cairo"
            [ codecTests
            , drawTests
            , rasterTests
            ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

-- | An L with a marker pixel at the top right, so any flip or transpose shows.
lShape :: [[Bool]]
lShape =
    [ [x < 3 || (x == 9 && y == 0) || y >= 5 | x <- [0 .. 10 :: Int]]
    | y <- [0 .. 6 :: Int]
    ]

lWidth, lHeight :: Int
lWidth = 11
lHeight = 7

packBits :: [Bool] -> BS.ByteString
packBits bits = BS.pack (go bits)
  where
    go [] = []
    go bs =
        let (byte, rest) = splitAt 8 bs
         in foldl (\acc (i, b) -> if b then setBit acc (7 - i) else acc) (0 :: Word8) (zip [0 ..] byte) : go rest

unpackBits :: Int -> BS.ByteString -> [Bool]
unpackBits w row = [testBit (BS.index row (x `div` 8)) (7 - x `mod` 8) | x <- [0 .. w - 1]]

lPng :: MaskPng
lPng = encodeMaskPng lWidth (map packBits lShape)

-- | 16 x 16 with only pixel (4, 4) set, which minification must not lose.
lonePng :: MaskPng
lonePng = encodeMaskPng 16 [packBits [x == 4 && y == 4 | x <- [0 .. 15 :: Int]] | y <- [0 .. 15 :: Int]]

decodeOrFail :: MaskPng -> IO (Int, Int, [BS.ByteString])
decodeOrFail png = either (assertFailure . ("decode failed: " <>)) pure (decodeMaskRows png)

setPixels :: Int -> [BS.ByteString] -> Set.Set (Int, Int)
setPixels w rows = Set.fromList [(x, y) | (y, row) <- zip [0 ..] rows, (x, True) <- zip [0 ..] (unpackBits w row)]

expectLeft :: String -> Either String a -> IO ()
expectLeft needle = \case
    Left err -> assertBool ("unexpected error: " <> err) (needle `isInfixOf` err)
    Right _ -> assertFailure ("expected a failure mentioning " <> show needle)

--------------------------------------------------------------------------------
-- Codec
--------------------------------------------------------------------------------

codecTests :: TestTree
codecTests =
    testGroup
        "MaskPng codec"
        [ testCase "L fixture round trips and passes the core profile check" $ do
            let MaskPng bytes = lPng
            either assertFailure (const (pure ())) (parseMaskPng bytes)
            (w, h, rows) <- decodeOrFail lPng
            (w, h) @?= (lWidth, lHeight)
            map (unpackBits w) rows @?= lShape
        , testCase "decoded A8 surface holds 255 for set pixels and re-encodes identically" $ do
            r <- withMaskSurface lPng $ \surf -> withSurfacePixels surf $ \stride dat -> do
                let !png' = encodeMaskPng lWidth (packMaskRows 128 stride dat 0 0 lWidth lHeight)
                    !a8 = BS.copy (BS.concat [BS.take lWidth (BS.drop (y * stride) dat) | y <- [0 .. lHeight - 1]])
                pure ([[BS.index a8 (y * lWidth + x) | x <- [0 .. lWidth - 1]] | y <- [0 .. lHeight - 1]], png')
            (a8, png') <- either assertFailure pure r
            a8 @?= map (map (\b -> if b then 255 else 0)) lShape
            png' @?= lPng
        , testCase "packing a region outside the buffer is an error, not an unchecked read" $ do
            let dat = BS.replicate (4 * 3) 255
                packs = [(1, 0, 4, 3), (0, 1, 4, 3), (-1, 0, 2, 2), (0, -1, 2, 2)]
            for_ packs $ \(x, y, w, h) -> do
                r <- try (evaluate (packMaskRows 128 4 dat x y w h))
                case r of
                    Left (_ :: ErrorCall) -> pure ()
                    Right _ -> assertFailure ("region " <> show (x, y, w, h) <> " was accepted")
            packMaskRows 128 4 dat 0 0 4 3 @?= replicate 3 (BS.singleton 0xF0)
        , testCase "payloads match the web package's browser test fixtures" $ do
            -- diagrams-canvas-json-web/test/fixture.ts
            maskPngBase64 lPng @?= "iVBORw0KGgoAAAANSUhEUgAAAAsAAAAHAQAAAAD2FYrwAAAAAnRSTlMAAHaTzTgAAAAWSURBVHicY3jgwPCAgYkBihj+PwCSAC3+A+iNmm1fAAAAAElFTkSuQmCC"
            maskPngBase64 lonePng @?= "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQAQAAAAA3iMLMAAAAAnRSTlMAAHaTzTgAAAAPSURBVHicY2BAAhwMBAEAAUgACex4Ik8AAAAASUVORK5CYII="
        , testCase "decoder undoes all five filter types" $ do
            let w = 37
                rows = [packBits [(x * 7 + y * 13) `mod` 5 < 2 || x == y | x <- [0 .. w - 1]] | y <- [0 .. 9 :: Int]]
                scan = BL.fromChunks (concat (zipWith3 forwardFilter (cycle [0 .. 4]) (zeroRow w : rows) rows))
            (w', h', rows') <- decodeOrFail (pngFromScanlines w (length rows) scan)
            (w', h') @?= (w, length rows)
            rows' @?= rows
        , testCase "a corrupted IDAT fails its CRC" $ do
            let MaskPng bytes = lPng
                i = BS.length bytes - 12 - 5
                bad = BS.take i bytes <> BS.singleton (BS.index bytes i + 1) <> BS.drop (i + 1) bytes
            expectLeft "CRC" (decodeMaskRows (MaskPng bad))
        , testCase "an unknown filter type is rejected" $
            expectLeft "filter type" (decodeMaskRows (pngFromScanlines 8 1 (BL.pack [5, 0xFF])))
        , testCase "a side over the texture limit is rejected before inflating" $
            expectLeft "at most 4096" (decodeMaskRows (pngFromScanlines 5000 1 (BL.pack [0])))
        , testCase "reading an unallocated surface throws rather than reading out of bounds" $ do
            r <- bracket (CI.imageSurfaceCreate C.FormatA8 (-1) 1) CI.surfaceDestroy $ \surf ->
                try (withSurfacePixels surf (\_ dat -> pure $! BS.length dat))
            case r of
                Left e -> assertBool (show e) ("unusable" `isInfixOf` show (e :: IOException))
                Right n -> assertFailure ("read " <> show n <> " bytes")
        , testCase "short and long image data are rejected" $ do
            expectLeft "shorter" (decodeMaskRows (pngFromScanlines 8 2 (BL.pack [0, 0xFF])))
            expectLeft "inflates past" (decodeMaskRows (pngFromScanlines 8 1 (BL.pack [0, 0xFF, 0, 0xFF])))
        ]

zeroRow :: Int -> BS.ByteString
zeroRow w = BS.replicate ((w + 7) `div` 8) 0

-- PNG forward filters with bpp = 1.
forwardFilter :: Word8 -> BS.ByteString -> BS.ByteString -> [BS.ByteString]
forwardFilter f prev row = [BS.singleton f, BS.pack [BS.index row i - predictor i | i <- [0 .. BS.length row - 1]]]
  where
    at bs i = if i < 0 then 0 else BS.index bs i
    predictor i =
        let a = at row (i - 1)
            b = at prev i
            c = at prev (i - 1)
         in case f of
                0 -> 0
                1 -> a
                2 -> b
                3 -> fromIntegral ((fromIntegral a + fromIntegral b :: Int) `div` 2)
                _ -> paeth a b c
    paeth a b c =
        let p = fromIntegral a + fromIntegral b - fromIntegral c :: Int
            pa = abs (p - fromIntegral a)
            pb = abs (p - fromIntegral b)
            pc = abs (p - fromIntegral c)
         in if pa <= pb && pa <= pc then a else if pb <= pc then b else c

--------------------------------------------------------------------------------
-- Drawing
--------------------------------------------------------------------------------

-- | ARGB32 render with the given user-to-device matrix; returns a pixel reader.
renderArgb :: Int -> Int -> Matrix -> [CanvasCmd] -> IO (Int -> Int -> (Word8, Word8, Word8, Word8))
renderArgb w h m cmds =
    withMaskSurfaces cmds $ \masks ->
        C.withImageSurface C.FormatARGB32 w h $ \surf -> do
            C.renderWith surf $ do
                C.setOperator C.OperatorClear
                C.paint
                C.setOperator C.OperatorOver
                C.setMatrix m
                runCommands masks 1 cmds
            C.surfaceFlush surf
            stride <- C.imageSurfaceGetStride surf
            dat <- C.imageSurfaceGetData surf
            -- Little-endian ARGB32 is B, G, R, A in memory; returned as (A, R, G, B).
            pure $ \x y ->
                let o = y * stride + 4 * x
                 in (BS.index dat (o + 3), BS.index dat (o + 2), BS.index dat (o + 1), BS.index dat o)

-- | The L at pitch 1 with its top left at (2, 9), drawn 10 device px per unit.
lCmd :: CanvasCmd
lCmd = CmdImageMask 0 128 255 1 2 2 13 9 lPng

-- | Device pixel inside raster pixel (i, j), at an offset within its 10 px cell.
cellPixel :: Int -> Int -> Int -> (Int, Int)
cellPixel off i j = (20 + 10 * i + off, 10 + 10 * j + off)

magnified :: Matrix
magnified = Matrix 10 0 0 (-10) 0 100

rect :: Double -> Double -> Double -> Double -> [CanvasCmd]
rect x0 y0 x1 y1 = [CmdBeginPath, CmdMoveTo x0 y0, CmdLineTo x1 y0, CmdLineTo x1 y1, CmdLineTo x0 y1, CmdClosePath]

drawTests :: TestTree
drawTests =
    testGroup
        "IM drawing"
        [ testCase "source-over places row 0 at y1 with crisp magnified pixels" $ do
            px <- renderArgb 150 100 magnified [lCmd]
            for_ (zip [0 ..] lShape) $ \(j, row) ->
                for_ (zip [0 ..] row) $ \(i, set) ->
                    for_ [1, 8] $ \off -> do
                        let (x, y) = cellPixel off i j
                        px x y @?= if set then (255, 0, 128, 255) else (0, 0, 0, 0)
        , testCase "destination-out erases only the set pixels" $ do
            px <-
                renderArgb 150 100 magnified $
                    rect 0 0 15 10 ++ [CmdFill 255 0 0 1, CmdSetGlobalCompositeOperation DestinationOut, lCmd]
            for_ (zip [0 ..] lShape) $ \(j, row) ->
                for_ (zip [0 ..] row) $ \(i, set) -> do
                    let (x, y) = cellPixel 5 i j
                    px x y @?= if set then (0, 0, 0, 0) else (255, 255, 0, 0)
        , testCase "a punch IM erases holes from every layer of a layered render" $ do
            let layer = MaskLayer Nothing (255, 0, 0, 1) (rect 0 0 15 10 ++ [CmdFill 255 255 255 1])
                mld = LayeredDiagram 15 10 (BBox 0 0 15 10) [layer, layer] [CmdSetGlobalCompositeOperation DestinationOut, lCmd] defaultJsonPrecision
                opts = defaultImageOptions{ioWidth = 150, ioHeight = 100, ioPadding = 1, ioBackground = BackgroundTransparent}
            tmp <- getTemporaryDirectory
            img <- bracket (openTempFile tmp "im-layered.png") (removeFile . fst) $ \(path, h) -> do
                hClose h
                renderLayeredDiagramTo FormatPNG path opts mld
                JP.readPng path >>= either assertFailure (pure . JP.convertRGBA8)
            for_ (zip [0 ..] lShape) $ \(j, row) ->
                for_ (zip [0 ..] row) $ \(i, set) -> do
                    let (x, y) = cellPixel 5 i j
                        JP.PixelRGBA8 _ _ _ a = JP.pixelAt img x y
                    a @?= if set then 0 else 255
        , testCase "a layer's own IM draws in a layered render" $ do
            let layer = MaskLayer Nothing (255, 0, 0, 1) [lCmd]
                mld = LayeredDiagram 15 10 (BBox 0 0 15 10) [layer] [] defaultJsonPrecision
                opts = defaultImageOptions{ioWidth = 150, ioHeight = 100, ioPadding = 1, ioBackground = BackgroundTransparent}
            tmp <- getTemporaryDirectory
            img <- bracket (openTempFile tmp "im-layer.png") (removeFile . fst) $ \(path, h) -> do
                hClose h
                renderLayeredDiagramTo FormatPNG path opts mld
                JP.readPng path >>= either assertFailure (pure . JP.convertRGBA8)
            for_ (zip [0 ..] lShape) $ \(j, row) ->
                for_ (zip [0 ..] row) $ \(i, set) -> do
                    let (x, y) = cellPixel 5 i j
                    JP.pixelAt img x y @?= if set then JP.PixelRGBA8 255 0 0 255 else JP.PixelRGBA8 0 0 0 0
        , testCase "minified rasters keep a lone pixel visible" $ do
            let cmd = CmdImageMask 0 0 0 1 0 0 16 16 lonePng
            px <- renderArgb 4 4 (Matrix 0.25 0 0 (-0.25) 0 4) [cmd]
            let (a, _, _, _) = px 1 1
            assertBool ("alpha " <> show a) (a > 0)
        ]

--------------------------------------------------------------------------------
-- Rasterisation
--------------------------------------------------------------------------------

circle :: Double -> Double -> Double -> [CanvasCmd]
circle cx cy r = [CmdMoveTo (cx + r) cy, CmdArc cx cy r 0 (2 * pi), CmdClosePath]

-- | Holes in the top left only and a slot near the bottom, so a flip shows.
punch :: [CanvasCmd]
punch =
    [CmdBeginPath]
        ++ concat
            [ circle (2 + 2.2 * fromIntegral i) (18 - 2.1 * fromIntegral j) (0.25 + 0.1 * fromIntegral (i + j))
            | i <- [0 .. 3 :: Int]
            , j <- [0 .. 3 :: Int]
            ]
        ++ [ CmdFill 0 0 0 1
           , CmdSetLineCap 1
           , CmdBeginPath
           , CmdMoveTo 3 2.5
           , CmdLineTo 15 4
           , CmdStroke 0 0 0 1 0.6
           ]

-- | Two holes and a thin trace, all in the left half of 'gridBounds'.
blobs :: [CanvasCmd]
blobs =
    [CmdBeginPath]
        ++ circle 1 1 0.7
        ++ circle 2.6 3.9 0.4
        ++ [CmdFill 0 0 0 1, CmdBeginPath, CmdMoveTo 0 0, CmdLineTo 3 4.5, CmdStroke 0 0 0 1 0.1]

gridBounds :: BBox
gridBounds = BBox (-0.123) (-0.077) 7.41 4.91

rasterOrFail :: MaskRasterSpec -> BBox -> [CanvasCmd] -> IO [MaskTile]
rasterOrFail spec bounds cmds = rasteriseMask spec bounds cmds >>= either (assertFailure . show) pure

-- | Pixels of all tiles in grid coordinates; overlapping tiles must agree.
tilePixels :: Double -> Int -> Int -> [MaskTile] -> IO (Set.Set (Int, Int))
tilePixels p col0 row1 tiles = do
    cells <- concat <$> traverse cellsOf tiles
    let merged = Map.fromListWith (\a b -> if a == b then a else error "tiles disagree on an overlap") cells
    pure (Map.keysSet (Map.filter id merged))
  where
    cellsOf (MaskTile (BBox x0 _ _ y1) png) = do
        (w, _, rows) <- decodeOrFail png
        let c = round (x0 / p) - col0
            r = row1 - round (y1 / p)
        pure [((c + x, r + y), b) | (y, row) <- zip [0 ..] rows, (x, b) <- zip [0 ..] (unpackBits w row)]

rasterTests :: TestTree
rasterTests =
    testGroup
        "rasteriseMask"
        [ testCase "picks the finest pitch in budget on an origin-anchored grid" $ do
            case planMaskGrid defaultMaskRasterSpec gridBounds of
                Right g -> (mgPitch g, mgCol0 g, mgRow1 g, mgWidth g, mgHeight g) @?= (0.025, -5, 197, 302, 201)
                Left err -> assertFailure (show err)
            fmap mgPitch (planMaskGrid defaultMaskRasterSpec{mrsMaxPixels = 20_000} gridBounds) @?= Right 0.05
            assertBool "nothing fits" (either (const True) (const False) (planMaskGrid defaultMaskRasterSpec{mrsMaxPixels = 100} gridBounds))
        , testCase "rejects pitches and tiles the wire cannot carry" $ do
            -- 1/3 has no exact 6 dp edges, so the decoder would see stretched pixels.
            assertBool "inexact pitch" (isLeft (planMaskGrid defaultMaskRasterSpec{mrsPitches = [1 / 3]} gridBounds))
            assertBool "0.033 is exact" (not (isLeft (planMaskGrid defaultMaskRasterSpec{mrsPitches = [0.033]} gridBounds)))
            assertBool "tile over 4096" (isLeft (planMaskGrid defaultMaskRasterSpec{mrsMaxTile = 4097} gridBounds))
        , testCase "raster punch matches the vector punch in a 512 px thumbnail" $ do
            tiles <- rasterOrFail defaultMaskRasterSpec (BBox 0 0 20 20) punch
            let base = rect 0 0 20 20 ++ [CmdFill 255 255 255 1, CmdSetGlobalCompositeOperation DestinationOut]
                thumb = Matrix 25.6 0 0 (-25.6) 0 512
            vec <- renderArgb 512 512 thumb (base ++ punch)
            ras <- renderArgb 512 512 thumb (base ++ maskTileCommands tiles)
            let holes px = Set.fromList [(x, y) | x <- [0 .. 511], y <- [0 .. 511], let (a, _, _, _) = px x y, a < 128]
                hv = holes vec
                hr = holes ras
                both = Set.size (Set.intersection hv hr)
                either' = Set.size (Set.union hv hr)
                holeIou = fromIntegral both / fromIntegral either' :: Double
                boardIou = fromIntegral (512 * 512 - either') / fromIntegral (512 * 512 - both) :: Double
            assertBool ("board IoU " <> show boardIou) (boardIou >= 0.99)
            assertBool ("hole IoU " <> show holeIou) (holeIou >= 0.95)
        , testCase "a 0.1 mm hole on a pixel corner at 50 um sets the four pixels around it" $ do
            -- Each pixel is only partly covered, so a high threshold would drop the hole.
            tiles <-
                rasterOrFail defaultMaskRasterSpec{mrsPitches = [0.05]} (BBox 0 0 2 2) $
                    [CmdBeginPath] ++ circle 1 1 0.05 ++ [CmdFill 0 0 0 1]
            case tiles of
                [MaskTile _ png] -> do
                    (w, _, rows) <- decodeOrFail png
                    setPixels w rows @?= Set.fromList [(19, 19), (20, 19), (19, 20), (20, 20)]
                _ -> assertFailure ("expected one tile, got " <> show (length tiles))
        , testCase "tiles reproduce the untiled raster and skip empty ones" $ do
            let spec = defaultMaskRasterSpec{mrsPitches = [0.05]}
            g <- either (assertFailure . show) pure (planMaskGrid spec gridBounds)
            whole <- rasterOrFail spec gridBounds blobs
            tiled <- rasterOrFail spec{mrsMaxTile = 40} gridBounds blobs
            length whole @?= 1
            -- Tiles start every 39 px, so neighbours share one row or column.
            for_ tiled $ \(MaskTile (BBox x0 _ _ y1) png) -> do
                (w, h, _) <- decodeOrFail png
                let c = round (x0 / 0.05) - mgCol0 g
                    r = mgRow1 g - round (y1 / 0.05)
                (c `mod` 39, r `mod` 39) @?= (0, 0)
                (w, h) @?= (min 40 (mgWidth g - c), min 40 (mgHeight g - r))
            -- 152 x 101 px in 40 px tiles sharing an edge is 4 x 3, content is left only.
            assertBool ("tile count " <> show (length tiled)) (length tiled > 1 && length tiled < 12)
            expected <- tilePixels 0.05 (mgCol0 g) (mgRow1 g) whole
            actual <- tilePixels 0.05 (mgCol0 g) (mgRow1 g) tiled
            assertBool "raster is not empty" (not (Set.null expected))
            actual @?= expected
        , testCase "25 um tiles decode after encoding at 2 dp coordinates" $ do
            tiles <- rasterOrFail defaultMaskRasterSpec{mrsMaxTile = 40} gridBounds blobs
            assertBool "has tiles" (length tiles > 1)
            let enc = encodeCmd defaultJsonPrecision{jpCoordinates = 2}
            for_ (maskTileCommands tiles) $ \cmd ->
                fmap enc (A.parseEither A.parseJSON (enc cmd)) @?= Right (enc cmd)
        , testCase "generated tiles pass the JSON square-pixel check" $ do
            tiles <- rasterOrFail defaultMaskRasterSpec{mrsPitches = [0.05], mrsMaxTile = 40} gridBounds blobs
            assertBool "has tiles" (length tiles > 1)
            let cd = CanvasDiagram 1 1 (BBox 0 0 1 1) (maskTileCommands tiles) defaultJsonPrecision
            case A.eitherDecode (A.encode cd) of
                Left err -> assertFailure err
                Right cd' -> [png | CmdImageMask _ _ _ _ _ _ _ _ png <- cdCommands cd'] @?= map mtPng tiles
        ]
