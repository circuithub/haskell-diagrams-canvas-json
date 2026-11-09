{-# LANGUAGE OverloadedStrings #-}

{- | Codec for the 'MaskPng' profile carried by @IM@ commands: 1-bit
greyscale, non-interlaced, with a @tRNS@ that makes grey 0 transparent.

Rows are packed MSB first, one bit per pixel, @(w + 7) \`div\` 8@ bytes per
row; a set bit is a covered pixel. The decoder targets an A8 cairo surface
(0 or 255) because A1 stores its bits LSB first in native-endian words.
-}
module Diagrams.Backend.CanvasJson.MaskPng (
    -- * Encoding
    packMaskRows,
    encodeMaskPng,
    withSurfacePixels,

    -- * Decoding
    MaskDecodeError (..),
    decodeMaskRows,
    withMaskSurface,

    -- * Low level
    pngFromScanlines,
) where

import Codec.Compression.Zlib qualified as Z
import Codec.Compression.Zlib.Internal qualified as ZI
import Control.Exception (Exception, bracket)
import Control.Monad (unless, when)
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Data.Bits (complement, setBit, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BI
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BU
import Data.Foldable (for_)
import Data.Word (Word32, Word8)
import Diagrams.Backend.CanvasJson (MaskPng (..), parseMaskPng)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Internal qualified as CI
import GI.Cairo.Render.Types (Surface (..))

--------------------------------------------------------------------------------
-- Encoding
--------------------------------------------------------------------------------

{- | Threshold the @w x h@ region at column @x@, row @y@ of an A8 pixel buffer
with the given row stride into packed rows. A pixel is set when its alpha is
at least the threshold. Calls 'error' when the region is outside the buffer,
since the rows are read unchecked and the buffer may be raw cairo memory.
-}
packMaskRows :: Word8 -> Int -> BS.ByteString -> Int -> Int -> Int -> Int -> [BS.ByteString]
packMaskRows thr stride dat x y w h
    | x < 0 || y < 0 || w < 0 || h < 0 || x + w > stride || (y + h) * stride > BS.length dat =
        error
            ( "packMaskRows: region "
                <> show (x, y, w, h)
                <> " is outside a buffer of stride "
                <> show stride
                <> " and length "
                <> show (BS.length dat)
            )
    | otherwise = [packRow (y + j) | j <- [0 .. h - 1]]
  where
    nb = rowBytes w
    packRow row = BI.unsafeCreate nb $ \p ->
        for_ [0 .. nb - 1] $ \k -> do
            let base = row * stride + x + 8 * k
                n = min 8 (w - 8 * k)
                byte =
                    foldl
                        (\acc i -> if BU.unsafeIndex dat (base + i) >= thr then setBit acc (7 - i) else acc)
                        (0 :: Word8)
                        [0 .. n - 1]
            pokeByteOff p k byte

{- | Encode packed rows of the given pixel width. Each row is filtered None or
Up, whichever has the smaller sum of absolute signed bytes (the libpng
heuristic), and deflated at level 6.
-}
encodeMaskPng :: Int -> [BS.ByteString] -> MaskPng
encodeMaskPng w rows = pngFromScanlines w (length rows) scanlines
  where
    zero = BS.replicate (rowBytes w) 0
    scanlines = BL.fromChunks (concat (zipWith filterRow (zero : rows) rows))
    filterRow prev row =
        let up = BS.packZipWith (-) row prev
         in if signedSum up < signedSum row then [upTag, up] else [noneTag, row]
    signedSum = BS.foldl' (\acc b -> acc + if b >= 128 then 256 - fromIntegral b else fromIntegral b) (0 :: Int)
    noneTag = BS.singleton 0
    upTag = BS.singleton 2

{- | Run the action on an image surface's row stride and pixel buffer, read in
place: 'C.imageSurfaceGetData' would copy the whole surface. The buffer is
only valid inside the action, so force whatever is built from it there.
Throws an 'IOException' when the surface failed to allocate.
-}
withSurfacePixels :: C.Surface -> (Int -> BS.ByteString -> IO a) -> IO a
withSurfacePixels surf@(Surface fp) act = withForeignPtr fp $ \_ -> do
    C.surfaceFlush surf
    status <- CI.surfaceStatus surf
    unless (status == C.StatusSuccess) $
        ioError (userError ("image surface is unusable: " <> show status))
    w <- C.imageSurfaceGetWidth surf
    h <- C.imageSurfaceGetHeight surf
    stride <- C.imageSurfaceGetStride surf
    base <- CI.imageSurfaceGetData surf
    unless (stride >= w && base /= nullPtr) $ ioError (userError "image surface has no pixel buffer")
    dat <- BU.unsafePackCStringLen (castPtr base, stride * h)
    act stride dat

{- | Assemble a profile PNG from already filtered scanlines (each prefixed
with its filter byte). Exposed so tests can exercise every filter type.
-}
pngFromScanlines :: Int -> Int -> BL.ByteString -> MaskPng
pngFromScanlines w h scanlines =
    MaskPng $!
        BS.concat
            [ pngSignature
            , chunk "IHDR" (BS.concat [be32 w, be32 h, BS.pack [1, 0, 0, 0, 0]])
            , chunk "tRNS" (BS.pack [0, 0])
            , chunk "IDAT" (BL.toStrict (Z.compressWith params scanlines))
            , chunk "IEND" BS.empty
            ]
  where
    params = Z.defaultCompressParams{Z.compressLevel = Z.compressionLevel 6}

chunk :: BS.ByteString -> BS.ByteString -> BS.ByteString
chunk ty body =
    BS.concat [be32 (BS.length body), ty, body, be32 (fromIntegral (crc32 (ty <> body)))]

be32 :: Int -> BS.ByteString
be32 n = BS.pack [fromIntegral (n `shiftR` s) | s <- [24, 16, 8, 0]]

pngSignature :: BS.ByteString
pngSignature = BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

rowBytes :: Int -> Int
rowBytes w = (w + 7) `div` 8

--------------------------------------------------------------------------------
-- CRC32 (ISO 3309, as PNG uses it)
--------------------------------------------------------------------------------

crcTable :: UArray Word8 Word32
crcTable = listArray (0, 255) [go (8 :: Int) (fromIntegral n) | n <- [0 .. 255 :: Int]]
  where
    go 0 !c = c
    go k !c = go (k - 1) (if testBit c 0 then 0xEDB88320 `xor` (c `shiftR` 1) else c `shiftR` 1)

crc32 :: BS.ByteString -> Word32
crc32 = complement . BS.foldl' step 0xFFFFFFFF
  where
    step c b = (crcTable `unsafeAt` fromIntegral ((c `xor` fromIntegral b) .&. 0xFF)) `xor` (c `shiftR` 8)

--------------------------------------------------------------------------------
-- Decoding
--------------------------------------------------------------------------------

-- | A payload outside the profile, raised when a render cannot draw an @IM@.
newtype MaskDecodeError = MaskDecodeError String
    deriving (Show)

instance Exception MaskDecodeError

{- | Decode to @(width, height, packed rows)@. Checks the profile (whose side
limit bounds the inflate) first, then every chunk CRC, the zlib stream and the exact inflated size, and undoes all five PNG
filter types.
-}
decodeMaskRows :: MaskPng -> Either String (Int, Int, [BS.ByteString])
decodeMaskRows (MaskPng bs) = do
    _ <- parseMaskPng bs
    chunks <- crcChunks (BS.drop 8 bs)
    (w, h) <- case chunks of
        ("IHDR", ihdr) : _ -> Right (readBe32 ihdr 0, readBe32 ihdr 4)
        _ -> Left "IM PNG IHDR must come first"
    let rb = rowBytes w
        expected = h * (rb + 1)
        idat = BL.fromChunks [body | ("IDAT", body) <- chunks]
    raw <- inflateExactly expected idat
    for_ [0 .. h - 1] $ \j ->
        let f = BU.unsafeIndex raw (j * (rb + 1))
         in when (f > 4) $ Left ("IM PNG row " <> show j <> " has unknown filter type " <> show f)
    let scan j = BS.take rb (BS.drop (j * (rb + 1) + 1) raw)
        filterOf j = BU.unsafeIndex raw (j * (rb + 1))
        rows = go (BS.replicate rb 0) 0
        go prev j
            | j >= h = []
            | otherwise = let !row = unfilter (filterOf j) prev (scan j) in row : go row (j + 1)
    pure (w, h, rows)

-- Chunk walk with CRC checks; the structure was already checked by parseMaskPng.
crcChunks :: BS.ByteString -> Either String [(BS.ByteString, BS.ByteString)]
crcChunks bs
    | BS.null bs = Right []
    | BS.length bs < 12 = Left "IM PNG chunk is truncated"
    | otherwise = do
        let len = readBe32 bs 0
            ty = BS.take 4 (BS.drop 4 bs)
            body = BS.take len (BS.drop 8 bs)
            crc = readBe32 bs (8 + len)
        when (BS.length bs < 12 + len) $ Left "IM PNG chunk is truncated"
        unless (fromIntegral crc == crc32 (ty <> body)) $
            Left ("IM PNG " <> show ty <> " chunk has a bad CRC")
        rest <- crcChunks (BS.drop (12 + len) bs)
        pure ((ty, body) : rest)

readBe32 :: BS.ByteString -> Int -> Int
readBe32 bs i = foldl (\acc k -> acc `shiftL` 8 .|. fromIntegral (BS.index bs (i + k))) 0 [0 .. 3]

-- Bounded so a hostile stream cannot inflate past the image size.
inflateExactly :: Int -> BL.ByteString -> Either String BS.ByteString
inflateExactly expected input = BS.concat <$> run 0
  where
    run =
        ZI.foldDecompressStreamWithInput
            ( \c k n ->
                let n' = n + BS.length c
                 in if n' > expected then Left "IM PNG image data inflates past its size" else (c :) <$> k n'
            )
            finish
            (\err _ -> Left ("IM PNG image data is not valid zlib: " <> show err))
            (ZI.decompressST ZI.zlibFormat ZI.defaultDecompressParams)
            input
    finish leftover n
        | not (BL.null leftover) = Left "IM PNG has data after the zlib stream"
        | n /= expected = Left "IM PNG image data is shorter than its size"
        | otherwise = Right []

-- bpp is 1 byte for 1-bit greyscale, so "left" is the previous byte.
unfilter :: Word8 -> BS.ByteString -> BS.ByteString -> BS.ByteString
unfilter 0 _ cur = cur
unfilter 2 prev cur = BS.packZipWith (+) cur prev
unfilter f prev cur = BI.unsafeCreate n $ \p ->
    for_ [0 .. n - 1] $ \i -> do
        a <- if i == 0 then pure 0 else peekByteOff p (i - 1) :: IO Word8
        let x = BU.unsafeIndex cur i
            b = BU.unsafeIndex prev i
            c = if i == 0 then 0 else BU.unsafeIndex prev (i - 1)
            pred' = case f of
                1 -> a
                3 -> fromIntegral ((fromIntegral a + fromIntegral b :: Int) `div` 2)
                _ -> paeth a b c
        pokeByteOff p i (x + pred')
  where
    n = BS.length cur

paeth :: Word8 -> Word8 -> Word8 -> Word8
paeth a b c
    | pa <= pb && pa <= pc = a
    | pb <= pc = b
    | otherwise = c
  where
    ia = fromIntegral a :: Int
    ib = fromIntegral b
    ic = fromIntegral c
    p = ia + ib - ic
    pa = abs (p - ia)
    pb = abs (p - ib)
    pc = abs (p - ic)

{- | Decode into an A8 surface (set pixels 255, others 0) that lives for the
action, so large rasters are freed promptly rather than by a finaliser.
-}
withMaskSurface :: MaskPng -> (C.Surface -> IO a) -> IO (Either String a)
withMaskSurface png act = case decodeMaskRows png of
    Left err -> pure (Left err)
    -- Sides are within 'maxMaskPngSide', far below cairo's limit.
    Right (w, h, rows) ->
        bracket (CI.imageSurfaceCreate C.FormatA8 w h) CI.surfaceDestroy $ \surf -> do
            status <- CI.surfaceStatus surf
            if status /= C.StatusSuccess
                then pure (Left ("IM PNG surface allocation failed: " <> show status))
                else do
                    writeRows surf w rows
                    Right <$> act surf

writeRows :: C.Surface -> Int -> [BS.ByteString] -> IO ()
writeRows surf@(Surface fp) w rows = withForeignPtr fp $ \_ -> do
    C.surfaceFlush surf
    stride <- C.imageSurfaceGetStride surf
    base <- castPtr <$> CI.imageSurfaceGetData surf :: IO (Ptr Word8)
    for_ (zip [0 ..] rows) $ \(j, row) -> do
        let rp = base `plusPtr` (j * stride)
        fillBytes rp 0 stride
        for_ [0 .. BS.length row - 1] $ \k -> do
            let byte = BU.unsafeIndex row k
            unless (byte == 0) $
                for_ [0 .. min 8 (w - 8 * k) - 1] $ \i ->
                    when (testBit byte (7 - i)) $ pokeByteOff rp (8 * k + i) (255 :: Word8)
    C.surfaceMarkDirty surf
