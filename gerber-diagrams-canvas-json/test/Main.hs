module Main (main) where

import Data.ByteString qualified as BS
import Diagrams.Backend.CanvasJson (
    CanvasCmd (..),
    CompositeOp (..),
    MaskPng (..),
 )
import Gerber.Diagrams.CanvasJson (
    recolorCommands,
    transformPolarity,
    transformPolarityInverted,
 )
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

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
            ]

-- Validation is not under test here, so the payload need not be a real PNG.
png :: MaskPng
png = MaskPng (BS.pack [1, 2, 3])

imageMask :: Double -> Double -> Double -> CanvasCmd
imageMask r g b = CmdImageMask r g b 1 (-1) (-2) 3 4 png
