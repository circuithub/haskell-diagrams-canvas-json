{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.List (isInfixOf)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Diagrams.Backend.CanvasJson
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main =
    defaultMain $
        testGroup
            "diagrams-canvas-json"
            [maskPngTests, imageMaskJsonTests, imageMaskValidationTests, optimizeTests, encodingTests]

-- Profile fields of a fixture PNG; CRCs are zero as the core does not check them.
data Fixture = Fixture
    { fxWidth :: Int
    , fxHeight :: Int
    , fxDepth :: Word8
    , fxColourType :: Word8
    , fxInterlace :: Word8
    , fxPreData :: [(BS.ByteString, BS.ByteString)]
    , fxPostData :: [(BS.ByteString, BS.ByteString)]
    }

goodFixture :: Fixture
goodFixture = Fixture 4 2 1 0 0 [("tRNS", BS.pack [0, 0])] []

fixtureBytes :: Fixture -> BS.ByteString
fixtureBytes fx =
    BS.concat $
        BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
            : chunk "IHDR" (be32 (fxWidth fx) <> be32 (fxHeight fx) <> BS.pack [fxDepth fx, fxColourType fx, 0, 0, fxInterlace fx])
            : map (uncurry chunk) (fxPreData fx ++ [("IDAT", BS.pack [0x78, 0x01])] ++ fxPostData fx ++ [("IEND", "")])
  where
    chunk ty dat = be32 (BS.length dat) <> ty <> dat <> be32 0
    be32 :: Int -> BS.ByteString
    be32 n = BS.pack [fromIntegral (n `div` (256 ^ k) `mod` 256) | k <- [3, 2, 1, 0 :: Int]]

goodPng :: MaskPng
goodPng = either error id (parseMaskPng (fixtureBytes goodFixture))

-- 4x2 pixels of 0.05 mm.
goodCmd :: CanvasCmd
goodCmd = CmdImageMask 10 20 30 1 (-0.1) 0.05 0.1 0.15 goodPng

imJson :: [A.Value] -> A.Value
imJson = A.toJSON . (A.String "IM" :)

decodeCmd :: A.Value -> Either String CanvasCmd
decodeCmd = A.parseEither A.parseJSON

expectFailure :: String -> Either String CanvasCmd -> IO ()
expectFailure expected = \case
    Left err -> assertBool ("expected " <> show expected <> " in " <> show err) (expected `isInfixOf` err)
    Right cmd -> assertFailure ("expected failure, decoded " <> show cmd)

b64 :: BS.ByteString -> A.Value
b64 = A.String . TE.decodeLatin1 . B64.encode

rectAnd :: A.Value -> [A.Value]
rectAnd png = [A.Number 0, A.Number 0, A.Number 0, A.Number 1, A.Number (-0.1), A.Number 0.05, A.Number 0.1, A.Number 0.15, png]

maskPngTests :: TestTree
maskPngTests =
    testGroup
        "MaskPng"
        [ testCase "reads the IHDR size" $ (maskPngWidth goodPng, maskPngHeight goodPng) @?= (4, 2)
        , testCase "shows the size rather than the bytes" $
            show goodPng @?= "<MaskPng 4x2, " <> show (BS.length (fixtureBytes goodFixture)) <> " bytes>"
        ]

imageMaskJsonTests :: TestTree
imageMaskJsonTests =
    testGroup
        "IM JSON"
        [ testCase "encodes the colour like F, the rect at 6 dp and padded base64" $
            encodeCmd defaultJsonPrecision (CmdImageMask 10.4 20 30 0.456 (-0.1000004) 0.05 0.1 0.15 goodPng)
                @?= imJson
                    [ A.Number 10
                    , A.Number 20
                    , A.Number 30
                    , A.Number 0.46
                    , A.Number (-0.1)
                    , A.Number 0.05
                    , A.Number 0.1
                    , A.Number 0.15
                    , A.String (maskPngBase64 goodPng)
                    ]
        , testCase "base64 is standard and padded with no data: prefix" $
            maskPngBase64 goodPng @?= TE.decodeLatin1 (B64.encode (fixtureBytes goodFixture))
        , testCase "round trips through JSON" $ decodeCmd (A.toJSON goodCmd) @?= Right goodCmd
        , testCase "a 0.025 pitch tile survives 2 dp coordinates" $ do
            -- 7 x 2 pixels from -5.675; 2 dp would round the edges to -5.68 and -5.5.
            let png = either error id (parseMaskPng (fixtureBytes goodFixture{fxWidth = 7}))
                cmd = CmdImageMask 0 0 0 1 (-5.675) 0.025 (-5.5) 0.075 png
            decodeCmd (encodeCmd defaultJsonPrecision{jpCoordinates = 2} cmd) @?= Right cmd
        , testCase "round trips inside a diagram" $ do
            let cd = CanvasDiagram 100 100 (BBox 0 0 1 1) [CmdSave, goodCmd, CmdRestore] defaultJsonPrecision
            A.eitherDecode (A.encode cd) @?= Right cd
        ]

imageMaskValidationTests :: TestTree
imageMaskValidationTests =
    testGroup
        "IM validation"
        [ testCase "rejects a bad signature" $
            expectFailure "bad signature" (decodeCmd (imJson (rectAnd (b64 ("x" <> BS.drop 1 (fixtureBytes goodFixture))))))
        , testCase "rejects bit depth 8" $ rejectsFixture "bit depth must be 1" goodFixture{fxDepth = 8}
        , testCase "rejects colour type 3" $ rejectsFixture "colour type must be 0" goodFixture{fxColourType = 3}
        , testCase "rejects interlacing" $ rejectsFixture "must not be interlaced" goodFixture{fxInterlace = 1}
        , testCase "rejects a missing tRNS" $ rejectsFixture "must have a tRNS chunk" goodFixture{fxPreData = []}
        , testCase "rejects a tRNS after IDAT" $
            rejectsFixture "must have a tRNS chunk" goodFixture{fxPreData = [], fxPostData = fxPreData goodFixture}
        , testCase "rejects a tRNS keeping grey 0 opaque" $
            rejectsFixture "tRNS must make grey 0 transparent" goodFixture{fxPreData = [("tRNS", BS.pack [0, 1])]}
        , testCase "rejects a PLTE, which tinting would duplicate" $
            rejectsFixture "must not have a \"PLTE\" chunk" goodFixture{fxPreData = ("PLTE", BS.pack [0, 0, 0]) : fxPreData goodFixture}
        , testCase "rejects IDATs split by another chunk" $
            rejectsFixture "IDAT chunks must be consecutive" goodFixture{fxPostData = [("tEXt", "a\0b"), ("IDAT", BS.pack [0x78, 0x01])]}
        , testCase "rejects an unknown critical chunk" $
            rejectsFixture "must not have a \"ABCD\" chunk" goodFixture{fxPreData = ("ABCD", "") : fxPreData goodFixture}
        , testCase "rejects a second IHDR" $
            rejectsFixture "must have one IHDR chunk" goodFixture{fxPreData = ("IHDR", BS.replicate 13 0) : fxPreData goodFixture}
        , testCase "rejects a second tRNS" $
            rejectsFixture "must have one tRNS chunk" goodFixture{fxPostData = fxPreData goodFixture}
        , testCase "rejects an IEND with a body" $
            expectFailure "IEND must be empty" $
                decodeCmd (imJson (rectAnd (b64 (BS.dropEnd 12 (fixtureBytes goodFixture) <> BS.pack [0, 0, 0, 1] <> "IEND" <> BS.pack [0, 0, 0, 0, 0]))))
        , testCase "accepts ancillary chunks around the IDATs" $
            void (parseMaskPng (fixtureBytes goodFixture{fxPreData = ("gAMA", BS.pack [0, 1, 0x86, 0xA0]) : fxPreData goodFixture, fxPostData = [("IDAT", BS.pack [0x03, 0x00]), ("tEXt", "a\0b")]})) @?= Right ()
        , testCase "rejects a side over the texture limit before inflating" $
            rejectsFixture "sides must be at most 4096" goodFixture{fxWidth = 4097}
        , testCase "rejects a truncated PNG" $
            expectFailure "truncated" (decodeCmd (imJson (rectAnd (b64 (BS.dropEnd 3 (fixtureBytes goodFixture))))))
        , testCase "rejects a data: URI" $
            expectFailure "not standard padded base64" $
                decodeCmd (imJson (rectAnd (A.String ("data:image/png;base64," <> maskPngBase64 goodPng))))
        , testCase "rejects unpadded base64" $
            expectFailure "not standard padded base64" $
                decodeCmd (imJson (rectAnd (A.String (T.dropWhileEnd (== '=') (maskPngBase64 goodPng)))))
        , testCase "rejects non-square pixels" $
            expectFailure "pixels must be square" (decodeCmd (A.toJSON (CmdImageMask 0 0 0 1 0 0 0.2 0.2 goodPng)))
        , testCase "rejects an infinite edge" $
            -- Aeson reads 1e400 as Infinity, which passes a relative pitch check.
            expectFailure "must be finite" $
                decodeCmd (imJson (take 6 (rectAnd (b64 (fixtureBytes goodFixture))) ++ [A.Number 1e400] ++ drop 7 (rectAnd (b64 (fixtureBytes goodFixture)))))
        , testCase "rejects an empty rectangle" $
            expectFailure "x1 > x0 and y1 > y0" (decodeCmd (A.toJSON (CmdImageMask 0 0 0 1 0.2 0 0 0.1 goodPng)))
        , testCase "accepts pitches equal within 1e-9 relative" $ do
            let cmd = CmdImageMask 0 0 0 1 0 0 0.2 (0.1 * (1 + 1e-12)) goodPng
            void (decodeCmd (A.toJSON cmd)) @?= Right ()
        ]
  where
    rejectsFixture expected fx = expectFailure expected (decodeCmd (imJson (rectAnd (b64 (fixtureBytes fx)))))

optimizeTests :: TestTree
optimizeTests =
    testGroup
        "optimizeCommands"
        [ testCase "never hoists a GCO above an earlier IM" $ do
            let cmds =
                    [ CmdSave
                    , goodCmd
                    , CmdSetGlobalCompositeOperation DestinationOut
                    , CmdBeginPath
                    , CmdMoveTo 0 0
                    , CmdLineTo 1 0
                    , CmdLineTo 0 1
                    , CmdClosePath
                    , CmdFill 0 0 0 1
                    , CmdRestore
                    ]
            optimizeCommands cmds @?= cmds
        , testCase "merges punch groups keeping the GCO ahead of every IM" $ do
            let punch = [CmdSave, CmdSetGlobalCompositeOperation DestinationOut, goodCmd, CmdRestore]
            optimizeCommands (punch ++ punch)
                @?= [CmdSave, CmdSetGlobalCompositeOperation DestinationOut, goodCmd, goodCmd, CmdRestore]
        , testCase "drops a transparent IM" $
            optimizeCommands [CmdImageMask 0 0 0 0.0005 0 0 0.2 0.1 goodPng, goodCmd] @?= [goodCmd]
        ]

-- The viewers read the bytes, so the direct encoding must write those of the 'A.Value'.
encodingTests :: TestTree
encodingTests =
    testGroup
        "direct encoding"
        [ testCase "a diagram writes the bytes of its value" $
            A.encode diagram @?= A.encode (A.toJSON diagram)
        , testCase "a layered diagram writes the bytes of its value" $
            A.encode layered @?= A.encode (A.toJSON layered)
        , testCase "a layered diagram without a punch writes the bytes of its value" $
            A.encode layered{ldPunch = []} @?= A.encode (A.toJSON layered{ldPunch = []})
        ]
  where
    precision = defaultJsonPrecision{jpCoordinates = 6, jpDimensions = 6}
    diagram = CanvasDiagram 400 400 (BBox (-1.25) 0.5 3.125 4.0000004) everyCommand precision
    layered =
        LayeredDiagram
            { ldWidth = 4.375
            , ldHeight = 3.5
            , ldBounds = BBox 0 0 4.375 3.5
            , ldLayers = [MaskLayer (Just "copper") (218, 165, 32, 0.8) everyCommand, MaskLayer Nothing (0, 0, 0, 1) [CmdFillCurrent]]
            , ldPunch = [CmdSave, CmdSetGlobalCompositeOperation DestinationOut, goodCmd, CmdRestore]
            , ldPrecision = precision
            }
    everyCommand =
        [ CmdSave
        , CmdRestore
        , CmdTransform 1 0 0 (-1) 0.123456789 2
        , CmdBeginPath
        , CmdMoveTo 1.0000005 (-2.5)
        , CmdLineTo 3 4
        , CmdBezierTo 1 2 3 4 5 6
        , CmdQuadTo 1 2 3 4
        , CmdArc 1 2 0.5 0 (2 * pi)
        , CmdClosePath
        , CmdFill 0 0 0 1
        , CmdStroke 255 255 255 0.5 0.1
        , CmdStrokeView 1 2 3 0.25 2
        , CmdSetFillColor 10 20 30 1
        , CmdSetStrokeColor 10 20 30 1 0.2
        , CmdSetStrokeColorView 10 20 30 1 0.2
        , CmdFillCurrent
        , CmdStrokeCurrent
        , CmdSetLineCap 1
        , CmdSetLineJoin 2
        , CmdSetLineDash [0.1, 0.2]
        , CmdSetLineDashView [1, 2]
        , CmdFillText "R1 \"x\"" 1 2
        , CmdSetFont "16px sans-serif"
        , CmdSetGlobalCompositeOperation DestinationIn
        , goodCmd
        ]
