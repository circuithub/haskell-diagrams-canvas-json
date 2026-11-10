{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.Picture qualified as Picture
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector.Storable qualified as Storable
import Diagrams.Backend.CanvasJson (
    BBox (..),
    CanvasCmd (..),
    CanvasDiagram (..),
    CompositeOp (..),
    MaskPng (..),
    optimizeCommands,
 )
import Diagrams.Backend.CanvasJson.Cairo (Background (BackgroundTransparent), ImageFormat (FormatPNG), ImageOptions (..), defaultImageOptions, renderCanvasDiagramTo)
import Gerber.CanvasJson.Layer (LayerPolarity (..), layerBounds, layerCommands, parseGerberLayer)
import Gerber.Diagrams.CanvasJson (
    defaultRenderOptions,
    recolorCommands,
    renderGerberRaw,
    transformPolarity,
    transformPolarityInverted,
 )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

main :: IO ()
main =
    defaultMain $
        testGroup
            "gerber-diagrams-canvas-json"
            [ testGroup
                "image mask"
                [ testCase "inverted polarity punches a dark raster group" $
                    transformPolarityInverted [CmdSave, imageMask 0 0 0, CmdRestore]
                        @?= [CmdSave, CmdSetGlobalCompositeOperation DestinationOut, imageMask 0 0 0, CmdRestore]
                , testCase "normal polarity keeps a dark raster group as is" $
                    transformPolarity [CmdSave, imageMask 0 0 0, CmdRestore]
                        @?= [CmdSave, imageMask 0 0 0, CmdRestore]
                , testCase "normal polarity erases with a clear raster group" $
                    transformPolarity [CmdSave, imageMask 255 255 255, CmdRestore]
                        @?= [CmdSave, CmdSetGlobalCompositeOperation DestinationOut, imageMask 255 255 255, CmdRestore]
                , testCase "recolour rewrites the raster colour and keeps its placement" $
                    recolorCommands (10, 20, 30, 0.5) [imageMask 0 0 0]
                        @?= [CmdImageMask 10 20 30 0.5 (-1) (-2) 3 4 png]
                ]
            , layerTests
            ]

-- Validation is not under test here, so the payload need not be a real PNG.
png :: MaskPng
png = MaskPng (BS.pack [1, 2, 3])

imageMask :: Double -> Double -> Double -> CanvasCmd
imageMask r g b = CmdImageMask r g b 1 (-1) (-2) 3 4 png

layerTests :: TestTree
layerTests =
    testGroup
        "direct layer"
        [ testCase "draws every primitive as diagrams does" $ do
            layer <- parsed primitivesGerber
            reference <- either assertFailure pure (renderGerberRaw defaultRenderOptions primitivesGerber)
            differing <- pixelsDiffering reference (optimizeCommands (transformPolarity reference.cdCommands)) (layerCommands Normal Nothing layer)
            differing @?= 0
        , testCase "a block keeps the polarity of each of its shapes" $ do
            layer <- parsed mixedBlockGerber
            layerCommands Normal Nothing layer @?= darkDisc ++ clearDisc
            layerCommands Inverted Nothing layer @?= punch darkDisc
        , testCase "an arc with ends apart in one direction sweeps nothing" $ do
            layer <- parsed rayArcGerber
            [() | CmdBezierTo{} <- layerCommands Normal Nothing layer] @?= []
        , testCase "a zero width draw is in the bounds but not painted" $ do
            layer <- parsed zeroWidthGerber
            layerCommands Normal Nothing layer @?= []
            layerBounds layer @?= BBox 1 2 4 6
        , testCase "a block draw flashed scaled has its stroke scaled" $ do
            layer <- parsed scaledDrawGerber
            [width | CmdSetStrokeColor _ _ _ _ width <- layerCommands Normal Nothing layer] @?= [0.25]
        ]
  where
    parsed = either assertFailure pure . parseGerberLayer
    darkDisc = [CmdSave, CmdSetFillColor 0 0 0 1, CmdBeginPath, CmdMoveTo 3 2, CmdArc 2 2 1 0 (2 * pi), CmdClosePath, CmdFillCurrent, CmdRestore]
    clearDisc = [CmdSave, CmdSetFillColor 255 255 255 1, CmdSetGlobalCompositeOperation DestinationOut, CmdBeginPath, CmdMoveTo 2.5 2, CmdArc 2 2 0.5 0 (2 * pi), CmdClosePath, CmdFillCurrent, CmdRestore]
    punch = \case
        CmdSave : CmdSetFillColor{} : rest -> CmdSave : CmdSetFillColor 0 0 0 1 : CmdSetGlobalCompositeOperation DestinationOut : rest
        cmds -> cmds

-- Pixels of the two renders whose coverage differs by over half, drawn in the reference's bounds.
pixelsDiffering :: CanvasDiagram -> [CanvasCmd] -> [CanvasCmd] -> IO Int
pixelsDiffering reference expected actual =
    withSystemTempDirectory "layer" \directory -> do
        let options = defaultImageOptions{ioWidth = size, ioHeight = size, ioBackground = BackgroundTransparent}
            render name cmds = do
                renderCanvasDiagramTo FormatPNG (directory </> name) options reference{cdCommands = cmds}
                either assertFailure (pure . Picture.convertRGBA8) =<< Picture.readPng (directory </> name)
        expectedImage <- render "expected.png" expected
        actualImage <- render "actual.png" actual
        let alpha image i = fromIntegral (Storable.unsafeIndex (Picture.imageData image) (4 * i + 3)) :: Int
        pure (length [() | i <- [0 .. size * size - 1], abs (alpha expectedImage i - alpha actualImage i) > 127])
  where
    size = 1200

gerber :: [Text] -> Text
gerber body = Text.unlines (["%FSLAX26Y26*%", "%MOMM*%", "G75*"] ++ body ++ ["M02*"])

-- Every aperture, macro primitive, draw and region diagrams draws as the gerber specifies.
primitivesGerber :: Text
primitivesGerber =
    gerber
        [ "%AMALLON*"
        , "1,1,0.8,0.5,0.5,30*"
        , "20,1,0.2,-1,-1,1,0.5,10*"
        , "21,1,1.0,0.4,0,-1,15*"
        , "4,1,3,1,1,2,1,1.5,2,1,1,20*"
        , "5,1,6,0,0,1.2,10*"
        , "%"
        , "%AMCUT*"
        , "21,1,2.0,1.0,0,0,0*"
        , "1,0,0.6,0,0*"
        , "%"
        , "%AMTHERMAL*"
        , "7,0,0,1.0,0.5,0.15,45*"
        , "%"
        , "%AMMOIRE*"
        , "6,0,0,2.0,0.2,0.2,3,0.05,2.2,0*"
        , "%"
        , "%ADD10ALLON*%"
        , "%ADD11CUT*%"
        , "%ADD12THERMAL*%"
        , "%ADD13MOIRE*%"
        , "%ADD14O,2.0X1.0*%"
        , "%ADD15O,0.8X1.6*%"
        , "%ADD16O,1.2X1.2*%"
        , "%ADD17O,1.0X1.0X0.3*%"
        , "%ADD18R,1.5X1.0*%"
        , "%ADD19R,1.5X1.0X0.5*%"
        , "%ADD20C,1.2*%"
        , "%ADD21C,1.2X0.4*%"
        , "%ADD22P,1.6X6X15*%"
        , "%ADD23P,1.6X5X0X0.5*%"
        , "%ADD24C,0.3*%"
        , "%ADD25R,0.4X0.2*%"
        , "D10*"
        , "X3000000Y3000000D03*"
        , "%LR33*%"
        , "X7000000Y3000000D03*"
        , "%LMX*%"
        , "X11000000Y3000000D03*"
        , "%LMN*%"
        , "%LR0*%"
        , "D11*"
        , "X15000000Y3000000D03*"
        , "D12*"
        , "X18000000Y3000000D03*"
        , "D13*"
        , "X21000000Y3000000D03*"
        , "D14*"
        , "X3000000Y7000000D03*"
        , "D15*"
        , "X6000000Y7000000D03*"
        , "D16*"
        , "X8000000Y7000000D03*"
        , "D17*"
        , "X10000000Y7000000D03*"
        , "D18*"
        , "X12000000Y7000000D03*"
        , "D19*"
        , "X14500000Y7000000D03*"
        , "D20*"
        , "X17000000Y7000000D03*"
        , "D21*"
        , "X19000000Y7000000D03*"
        , "D22*"
        , "X21000000Y7000000D03*"
        , "D23*"
        , "X23000000Y7000000D03*"
        , "D24*"
        , "G01*"
        , "X1000000Y10000000D02*"
        , "X8000000Y12000000D01*"
        , "X10000000Y10000000D01*"
        , "G02*"
        , "X14000000Y10000000I2000000J0D01*"
        , "G03*"
        , "X18000000Y10000000I2000000J0D01*"
        , "X18000000Y10000000I1000000J0D01*"
        , "D25*"
        , "G01*"
        , "X20000000Y10000000D02*"
        , "X24000000Y12000000D01*"
        , "D20*"
        , "X3000000Y16000000D03*"
        , "%LPC*%"
        , "X3300000Y16000000D03*"
        , "%LPD*%"
        , "%ABD30*%"
        , "D18*"
        , "X0Y0D03*"
        , "D24*"
        , "X0Y0D02*"
        , "X1500000Y1000000D01*"
        , "G02*"
        , "X1500000Y-1000000I0J-1000000D01*"
        , "G01*"
        , "D10*"
        , "X-1000000Y1000000D03*"
        , "%AB*%"
        , "D30*"
        , "X8000000Y16000000D03*"
        , "%LMX*%"
        , "X13000000Y16000000D03*"
        , "%LMXY*%"
        , "%LR45*%"
        , "X18000000Y16000000D03*"
        , "%LMN*%"
        , "%LR0*%"
        , "%SRX3Y2I2.5J2.0*%"
        , "D19*"
        , "X1000000Y20000000D03*"
        , "D24*"
        , "X500000Y19500000D02*"
        , "X1500000Y20500000D01*"
        , "%SR*%"
        , -- a pour with a hole cut in: the hole's ends meet on one ray
          "G36*"
        , "X10000000Y19000000D02*"
        , "X16000000Y19000000D01*"
        , "X16000000Y23000000D01*"
        , "X13000000Y23000000D01*"
        , "X13000000Y22000000D01*"
        , "G03*"
        , "X13000000Y21980000I0J-1000000D01*"
        , "X13000000Y22000000I0J-980000D01*"
        , "G01*"
        , "X13000000Y23000000D01*"
        , "X10000000Y23000000D01*"
        , "X10000000Y19000000D01*"
        , "G37*"
        ]

mixedBlockGerber :: Text
mixedBlockGerber =
    gerber ["%ADD10C,2*%", "%ADD11C,1*%", "%ABD12*%", "D10*", "X0Y0D03*", "%LPC*%", "D11*", "X0Y0D03*", "%LPD*%", "%AB*%", "D12*", "X2000000Y2000000D03*"]

{- | Two arcs whose ends lie on one ray from the centre: one in directions equal
exactly, one in directions just apart by rounding, as just under a whole turn.
-}
rayArcGerber :: Text
rayArcGerber =
    gerber
        [ "G36*"
        , "X0Y-1000000D02*"
        , "X300000Y100000D01*"
        , "G03*"
        , "X900000Y300000I-300000J-100000D01*"
        , "G01*"
        , "X0Y1000000D01*"
        , "X0Y-1000000D01*"
        , "G37*"
        , "G36*"
        , "X2000000Y-1000000D02*"
        , "X4000000Y1000000D01*"
        , "G03*"
        , "X6000000Y2000000I-2000000J-1000000D01*"
        , "G01*"
        , "X2000000Y-1000000D01*"
        , "G37*"
        ]

zeroWidthGerber :: Text
zeroWidthGerber = gerber ["%ADD10C,0*%", "D10*", "X1000000Y2000000D02*", "X4000000Y6000000D01*"]

scaledDrawGerber :: Text
scaledDrawGerber = gerber ["%ADD10C,0.5*%", "%ABD11*%", "D10*", "X0Y0D02*", "X1000000Y0D01*", "%AB*%", "%LS0.5*%", "D11*", "X0Y0D03*"]
