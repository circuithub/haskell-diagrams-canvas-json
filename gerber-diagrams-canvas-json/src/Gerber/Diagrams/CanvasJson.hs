{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Convert gerber files to canvas JSON diagrams.

Gerber layers are 1-bit images built by drawing shapes in positive (dark)
and negative (clear) polarity. This module renders them using
@diagrams-canvas-json@ and post-processes the command stream so that
clear-polarity shapes use @destination-out@ compositing to punch holes
to transparency.
-}
module Gerber.Diagrams.CanvasJson (
    -- * Rendering
    renderGerber,
    renderGerberRaw,
    renderGerberOutline,
    RenderOptions (..),
    defaultRenderOptions,

    -- * Compositing
    compositeLayers,
    clipToOutline,

    -- * Board rendering
    OutlineMode (..),
    BoardLayerSpec (..),
    BoardSpec (..),
    buildBoardLayerCommands,
    buildBoardDiagram,
    recolorCommands,
    syntheticOutline,

    -- * Gerber precision
    applyGerberPrecision,
    gerberDecimalPlaces,

    -- * Post-processing
    transformPolarity,
    transformPolarityInverted,
    outlineToFilled,
) where

import Control.Foldl qualified as L
import Data.Aeson (FromJSON (..), (.!=), (.:), (.:?))
import Data.Aeson qualified as A
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Diagrams.Backend.CanvasJson (
    BBox (..),
    CanvasCmd (..),
    CanvasDiagram (..),
    CanvasJson,
    CanvasJsonOptions (..),
    CompositeOp (..),
    JsonPrecision (..),
    LayeredDiagram (..),
    MaskLayer (..),
    defaultJsonPrecision,
    optimizeCommands,
    renderCanvasJson,
 )
import Diagrams.Prelude (Any, QDiagram, SizeSpec, V2, mkWidth)
import Gerber.Command qualified as Gerber
import Gerber.Diagrams (gerberToDiagram)
import Gerber.Evaluate (evaluate)
import Gerber.Format (Format (..))
import Gerber.Grammar (parseGerberPretty)
import Gerber.Unit qualified as Unit

-- | Options for rendering a gerber layer.
data RenderOptions = RenderOptions
    { roSize :: SizeSpec V2 Double
    -- ^ Canvas size specification (default: width 400)
    , roPrecision :: JsonPrecision
    -- ^ JSON serialization precision (default: 'defaultJsonPrecision')
    }

-- | Default render options: 400px wide, default precision.
defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions{roSize = mkWidth 400, roPrecision = defaultJsonPrecision}

{- | Parse a gerber file and render it to a 'CanvasDiagram' with
clear-polarity shapes using @destination-out@ compositing.
-}
renderGerber :: RenderOptions -> Text -> Either String CanvasDiagram
renderGerber opts src = do
    cd <- renderGerberRaw opts src
    Right cd{cdCommands = optimizeCommands $ transformPolarity (cdCommands cd)}

{- | Parse a gerber file and render it to a 'CanvasDiagram' without
any polarity post-processing. Useful when you want to apply a custom
transform (e.g. 'transformPolarityInverted') yourself.

The coordinate and dimension precision in the 'JsonPrecision' is
automatically set from the gerber format specification (FS command),
accounting for unit conversion to millimetres.
-}
renderGerberRaw :: RenderOptions -> Text -> Either String CanvasDiagram
renderGerberRaw opts src = do
    cmds <- parseGerberPretty src
    let precision = applyGerberPrecision cmds (roPrecision opts)
        diagram :: QDiagram CanvasJson V2 Double Any
        diagram = L.fold (evaluate gerberToDiagram) cmds
        cd = renderCanvasJson (CanvasJsonOptions (roSize opts) precision) diagram
    Right cd

{- | Extract coordinate precision from gerber commands and apply it to
the given 'JsonPrecision'.

Reads the FS (format specification) and MO (unit mode) commands to
determine how many decimal places are meaningful. When units are
inches, the ×25.4 mm conversion adds approximately 2 extra digits
of precision.
-}
applyGerberPrecision :: [Gerber.Command] -> JsonPrecision -> JsonPrecision
applyGerberPrecision cmds jp =
    case gerberDecimalPlaces cmds of
        Nothing -> jp
        Just dp -> jp{jpCoordinates = dp, jpDimensions = dp}

{- | Compute the number of decimal places needed in millimetres from
the gerber format specification.

Returns 'Nothing' if no FS command is found.
-}
gerberDecimalPlaces :: [Gerber.Command] -> Maybe Int
gerberDecimalPlaces cmds =
    case findFS cmds of
        Nothing -> Nothing
        Just (xFmt, _yFmt) ->
            let basePrecision = decimalPositions xFmt
             in Just $ case findUnit cmds of
                    Just Unit.IN -> basePrecision + 2
                    _ -> basePrecision
  where
    findFS [] = Nothing
    findFS (Gerber.FS _ xf yf : _) = Just (xf, yf)
    findFS (_ : rest) = findFS rest

    findUnit [] = Nothing
    findUnit (Gerber.MO u : _) = Just u
    findUnit (_ : rest) = findUnit rest

{- | Parse a gerber outline file and render it as a solid filled shape.

Outline gerbers draw the board edge as individual stroked line segments.
This function extracts the path coordinates from those segments and
produces a single closed, filled path instead.
-}
renderGerberOutline ::
    RenderOptions ->
    -- | Fill colour RGBA (0-255 for RGB, 0-1 for alpha)
    (Double, Double, Double, Double) ->
    Text ->
    Either String CanvasDiagram
renderGerberOutline opts rgba src = do
    cd <- renderGerberRaw opts src
    Right cd{cdCommands = outlineToFilled rgba (cdCommands cd)}

{- | Extract path segments from canvas commands, weld them into contours,
identify the board outline (largest contour) and any cutouts (smaller
contours), then produce a filled outline with cutouts punched out.

Outline gerbers may specify their segments out of order and may include
interior cutouts. This function uses a spatial-index-based welding
algorithm to efficiently join segments by endpoint proximity, then
splits the resulting contours into the board outline and cutouts.
-}
outlineToFilled :: (Double, Double, Double, Double) -> [CanvasCmd] -> [CanvasCmd]
outlineToFilled (fr, fg, fb, fa) cmds =
    let segments = extractPathSegments cmds
        welded = weldSegments segments
     in case splitOutlineCutouts welded of
            Nothing -> cmds -- no contours found, return original
            Just (outline, cutouts) ->
                fillContour (wcPoints outline)
                    ++ concatMap punchCutout cutouts
  where
    fillContour [] = []
    fillContour ((x, y) : pts) =
        [CmdBeginPath, CmdMoveTo x y]
            ++ map (uncurry CmdLineTo) pts
            ++ [CmdClosePath, CmdFill fr fg fb fa]

    punchCutout wc =
        [ CmdSave
        , CmdSetGlobalCompositeOperation DestinationOut
        ]
            ++ fillContour (wcPoints wc)
            ++ [CmdRestore]

{- | Extract path segments from the command stream.

Each @Save@\/@Restore@ group in an outline gerber represents one stroke
segment. This function collects the path coordinates from each group
into a separate segment list.
-}
extractPathSegments :: [CanvasCmd] -> [[(Double, Double)]]
extractPathSegments = go
  where
    go [] = []
    go (CmdSave : rest) =
        let (group, remaining) = extractGroup 1 [] rest
            pts = concatMap toPoint group
         in case pts of
                [] -> go remaining
                _ -> pts : go remaining
    go (_ : rest) = go rest

    toPoint :: CanvasCmd -> [(Double, Double)]
    toPoint (CmdMoveTo x y) = [(x, y)]
    toPoint (CmdLineTo x y) = [(x, y)]
    toPoint (CmdBezierTo _ _ _ _ x y) = [(x, y)]
    toPoint (CmdQuadTo _ _ x y) = [(x, y)]
    toPoint _ = []

--------------------------------------------------------------------------------
-- Contour welding
--------------------------------------------------------------------------------

-- | Tolerance for considering two endpoints as the same point.
weldTolerance :: Double
weldTolerance = 0.01

-- | A contour assembled from welded path segments.
data WeldContour = WeldContour
    { wcStart :: !(Double, Double)
    , wcEnd :: !(Double, Double)
    , wcPoints :: ![(Double, Double)]
    -- ^ All points from start to end, inclusive.
    }

-- | Reverse a contour's direction.
reverseWC :: WeldContour -> WeldContour
reverseWC wc =
    WeldContour
        { wcStart = wcEnd wc
        , wcEnd = wcStart wc
        , wcPoints = reverse (wcPoints wc)
        }

-- | Join two contours end-to-start (first's end meets second's start).
joinWC :: WeldContour -> WeldContour -> WeldContour
joinWC a b =
    WeldContour
        { wcStart = wcStart a
        , wcEnd = wcEnd b
        , wcPoints = wcPoints a ++ drop 1 (wcPoints b)
        }

-- | Reference to a contour endpoint in the spatial index.
data EndRef = EndRef
    { erContourId :: !Int
    , erIsStart :: !Bool
    }

-- | State for the contour welding algorithm.
data WeldState = WeldState
    { wsNextId :: !Int
    , wsContours :: !(IntMap WeldContour)
    , wsGrid :: !(Map.Map (Int, Int) [EndRef])
    -- ^ Spatial grid index: quantized coordinate -> endpoint references
    }

emptyWeldState :: WeldState
emptyWeldState = WeldState 0 IntMap.empty Map.empty

-- | Quantize a point to a grid cell key.
gridKey :: (Double, Double) -> (Int, Int)
gridKey (x, y) = (round (x / weldTolerance), round (y / weldTolerance))

-- | Grid cells that could contain a point within the weld tolerance.
gridNeighbors :: (Double, Double) -> [(Int, Int)]
gridNeighbors p =
    let (gx, gy) = gridKey p
     in [(gx + dx, gy + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]

-- | Check if two points are within the weld tolerance.
nearPoint :: (Double, Double) -> (Double, Double) -> Bool
nearPoint (x1, y1) (x2, y2) =
    abs (x1 - x2) <= weldTolerance && abs (y1 - y2) <= weldTolerance

-- | Add an endpoint reference to the spatial grid.
addToGrid :: Int -> Bool -> (Double, Double) -> Map.Map (Int, Int) [EndRef] -> Map.Map (Int, Int) [EndRef]
addToGrid cid isStart pt =
    Map.insertWith (++) (gridKey pt) [EndRef cid isStart]

-- | Remove all entries for a contour ID from the grid.
removeFromGrid :: Int -> Map.Map (Int, Int) [EndRef] -> Map.Map (Int, Int) [EndRef]
removeFromGrid cid = Map.mapMaybe $ \refs ->
    case filter (\r -> erContourId r /= cid) refs of
        [] -> Nothing
        rs -> Just rs

-- | Find a contour endpoint near the given point, excluding a specific contour.
findNearEndpoint :: WeldState -> Int -> (Double, Double) -> Maybe EndRef
findNearEndpoint ws excludeId pt =
    case candidates of
        [] -> Nothing
        (ref : _) -> Just ref
  where
    candidates =
        [ ref
        | cell <- gridNeighbors pt
        , ref <- Map.findWithDefault [] cell (wsGrid ws)
        , erContourId ref /= excludeId
        , let contour = wsContours ws IntMap.! erContourId ref
        , let ep = if erIsStart ref then wcStart contour else wcEnd contour
        , nearPoint pt ep
        ]

-- | Insert a new contour into the weld state.
insertWS :: WeldContour -> WeldState -> WeldState
insertWS wc ws =
    let cid = wsNextId ws
     in WeldState
            { wsNextId = cid + 1
            , wsContours = IntMap.insert cid wc (wsContours ws)
            , wsGrid =
                addToGrid cid True (wcStart wc) $
                    addToGrid cid False (wcEnd wc) $
                        wsGrid ws
            }

-- | Remove a contour from the weld state.
deleteWS :: Int -> WeldState -> WeldState
deleteWS cid ws =
    ws
        { wsContours = IntMap.delete cid (wsContours ws)
        , wsGrid = removeFromGrid cid (wsGrid ws)
        }

{- | Add a path segment to the weld state, joining it with existing
contours when endpoints match within the weld tolerance.

For each new segment, the algorithm checks whether its start or end
point is near any existing contour endpoint using the spatial grid
index (O(log n) per lookup). When matches are found, the segment is
joined to the matching contour(s); when both ends match different
contours, all three are merged into one.
-}
addSegment :: [(Double, Double)] -> WeldState -> WeldState
addSegment [] ws = ws
addSegment [_] ws = ws
addSegment pts@(segStart : _) ws =
    let seg = WeldContour segStart (last pts) pts
     in case findNearEndpoint ws (-1) (wcStart seg) of
            Just startRef ->
                let cid = erContourId startRef
                    existing = wsContours ws IntMap.! cid
                    ws' = deleteWS cid ws
                    -- Orient existing so matching endpoint is at its end
                    oriented =
                        if erIsStart startRef
                            then reverseWC existing
                            else existing
                    joined = joinWC oriented seg
                 in -- Check if joined contour's other end also matches something
                    case findNearEndpoint ws' (-1) (wcEnd joined) of
                        Just endRef ->
                            let cid2 = erContourId endRef
                                existing2 = wsContours ws' IntMap.! cid2
                                ws'' = deleteWS cid2 ws'
                                oriented2 =
                                    if erIsStart endRef
                                        then existing2
                                        else reverseWC existing2
                             in insertWS (joinWC joined oriented2) ws''
                        Nothing -> insertWS joined ws'
            Nothing ->
                case findNearEndpoint ws (-1) (wcEnd seg) of
                    Just endRef ->
                        let cid = erContourId endRef
                            existing = wsContours ws IntMap.! cid
                            ws' = deleteWS cid ws
                            oriented =
                                if erIsStart endRef
                                    then existing
                                    else reverseWC existing
                         in insertWS (joinWC seg oriented) ws'
                    Nothing -> insertWS seg ws

-- | Weld a list of path segments into contours using spatial indexing.
weldSegments :: [[(Double, Double)]] -> [WeldContour]
weldSegments segments =
    IntMap.elems . wsContours $ foldl' (flip addSegment) emptyWeldState segments

-- | Check if a contour is closed (start and end within tolerance).
isClosedContour :: WeldContour -> Bool
isClosedContour wc = nearPoint (wcStart wc) (wcEnd wc)

-- | Bounding box area of a contour (for size comparison).
contourBBoxArea :: WeldContour -> Double
contourBBoxArea wc =
    let xs = map fst (wcPoints wc)
        ys = map snd (wcPoints wc)
     in (maximum xs - minimum xs) * (maximum ys - minimum ys)

{- | Split welded contours into the board outline and cutouts.

The largest closed contour by bounding box area is taken as the board
outline. All other closed contours become cutouts.
-}
splitOutlineCutouts :: [WeldContour] -> Maybe (WeldContour, [WeldContour])
splitOutlineCutouts contours =
    case sortBy (flip compare `on` contourBBoxArea) (filter isClosedContour contours) of
        [] -> Nothing
        (outline : cutouts) -> Just (outline, cutouts)

{- | Composite a base layer with an inverted overlay layer.

The base layer is rendered normally (dark = opaque, clear = transparent).
The overlay layer is inverted: its dark shapes punch holes through the
base using @destination-out@, and its clear shapes are dropped.

The resulting diagram uses the union of both bounding boxes.
-}
compositeLayers ::
    -- | Base layer
    CanvasDiagram ->
    -- | Overlay layer (will be inverted)
    CanvasDiagram ->
    CanvasDiagram
compositeLayers base overlay =
    CanvasDiagram
        { cdWidth = max (cdWidth base) (cdWidth overlay)
        , cdHeight = max (cdHeight base) (cdHeight overlay)
        , cdBounds = unionBBox (cdBounds base) (cdBounds overlay)
        , cdCommands =
            transformPolarity (cdCommands base)
                ++ transformPolarityInverted (cdCommands overlay)
        , cdPrecision = cdPrecision base
        }

{- | Clip a content layer to an outline shape using @destination-in@.

The content layer is rendered first (with normal polarity processing).
Then the outline is drawn with @destination-in@ compositing, which
keeps only the content pixels that fall inside the outline shape and
makes everything outside transparent.

Use this to trim away parts of a gerber layer that extend beyond the
board outline (e.g. silkscreen drawn outside the board edge).
-}
clipToOutline ::
    -- | Content layer
    CanvasDiagram ->
    -- | Outline layer (will be filled and used as clip mask)
    CanvasDiagram ->
    CanvasDiagram
clipToOutline content outline =
    CanvasDiagram
        { cdWidth = max (cdWidth content) (cdWidth outline)
        , cdHeight = max (cdHeight content) (cdHeight outline)
        , cdBounds = unionBBox (cdBounds content) (cdBounds outline)
        , cdCommands =
            transformPolarity (cdCommands content)
                ++ [ CmdSave
                   , CmdSetGlobalCompositeOperation DestinationIn
                   ]
                ++ outlineToFilled (0, 0, 0, 1) (cdCommands outline)
                ++ [CmdRestore]
        , cdPrecision = cdPrecision content
        }

unionBBox :: BBox -> BBox -> BBox
unionBBox a b =
    BBox
        { bbMinX = min (bbMinX a) (bbMinX b)
        , bbMinY = min (bbMinY a) (bbMinY b)
        , bbMaxX = max (bbMaxX a) (bbMaxX b)
        , bbMaxY = max (bbMaxY a) (bbMaxY b)
        }

--------------------------------------------------------------------------------
-- Board composition
--------------------------------------------------------------------------------

-- | How the board outline interacts with a layer.
data OutlineMode
    = {- | Clip the layer to the outline shape using @destination-in@.
      Use for layers whose content extends beyond the board edge
      (e.g. copper, silkscreen).
      -}
      OutlineClip
    | {- | Fill the outline shape, then punch holes where the layer's dark
      shapes appear using @destination-out@. Use for layers that cover
      the board except at openings (e.g. solder mask).
      -}
      OutlineFill
    deriving (Show, Eq)

instance FromJSON OutlineMode where
    parseJSON = A.withText "OutlineMode" $ \case
        "clip" -> pure OutlineClip
        "fill" -> pure OutlineFill
        other -> fail $ "Unknown outlineMode: " <> T.unpack other

-- | Specification for a single layer in a board composition.
data BoardLayerSpec = BoardLayerSpec
    { blsName :: !(Maybe Text)
    -- ^ Optional human-readable layer name
    , blsColor :: !(Double, Double, Double, Double)
    -- ^ Layer colour RGBA (0-255 RGB, 0-1 alpha)
    , blsBase :: !FilePath
    -- ^ Base gerber file
    , blsOutlineMode :: !OutlineMode
    -- ^ How the board outline interacts with this layer
    }
    deriving (Show, Eq)

instance FromJSON BoardLayerSpec where
    parseJSON = A.withObject "BoardLayerSpec" $ \o -> do
        name <- o .:? "name"
        [r, g, b, a] <- o .: "color"
        base <- o .: "base"
        mode <- o .: "outlineMode"
        pure
            BoardLayerSpec
                { blsName = name
                , blsColor = (r, g, b, a)
                , blsBase = base
                , blsOutlineMode = mode
                }

{- | Board specification parsed from a JSON file.

The outline and through-layers (drills, cutouts) are specified once and
applied to every layer. Each layer specifies only its colour, base gerber,
and how the outline interacts with it.
-}
data BoardSpec = BoardSpec
    { bsOutline :: !(Maybe FilePath)
    {- ^ Board outline gerber. When 'Nothing', the outline is synthesized
    from the bounding box of all layers.
    -}
    , bsThroughLayers :: ![FilePath]
    -- ^ Gerber files that punch through all layers (drills, cutouts)
    , bsBaseColor :: !(Maybe (Double, Double, Double, Double))
    -- ^ Optional board substrate colour (filled outline below all layers)
    , bsLayers :: ![BoardLayerSpec]
    -- ^ Layers from bottom to top
    }
    deriving (Show, Eq)

instance FromJSON BoardSpec where
    parseJSON = A.withObject "BoardSpec" $ \o -> do
        outline <- o .:? "outline"
        through <- o .:? "throughLayers" .!= []
        baseColor <- o .:? "baseColor"
        layers <- o .: "layers"
        pure
            BoardSpec
                { bsOutline = outline
                , bsThroughLayers = through
                , bsBaseColor = baseColor
                , bsLayers = layers
                }

{- | Build the command list for a single board layer.

The outline mode determines how the layer interacts with the board outline:

* 'OutlineClip': The base gerber is rendered with normal polarity, then
  clipped to the outline via @destination-in@.

* 'OutlineFill': The outline is filled, then the base gerber's dark shapes
  punch holes via @destination-out@ (e.g. solder mask openings).

In both modes, through-layers (drills, cutouts) punch holes at the end.

All colours are replaced with the layer colour so the commands can be drawn
directly. Compositing operations depend only on alpha, so recolouring does
not affect their behaviour.
-}
buildBoardLayerCommands ::
    -- | Layer colour (RGBA)
    (Double, Double, Double, Double) ->
    -- | How the outline interacts with this layer
    OutlineMode ->
    -- | Base layer (raw, unprocessed)
    CanvasDiagram ->
    -- | Board outline (raw)
    CanvasDiagram ->
    -- | Through-layers (raw) — drills, cutouts
    [CanvasDiagram] ->
    [CanvasCmd]
buildBoardLayerCommands color mode base outline throughs =
    optimizeCommands $ recolorCommands color (baseCmds ++ throughCmds)
  where
    baseCmds = case mode of
        OutlineClip ->
            transformPolarity (cdCommands base)
                ++ [ CmdSave
                   , CmdSetGlobalCompositeOperation DestinationIn
                   ]
                ++ outlineToFilled (0, 0, 0, 1) (cdCommands outline)
                ++ [CmdRestore]
        OutlineFill ->
            outlineToFilled (0, 0, 0, 1) (cdCommands outline)
                ++ transformPolarityInverted (cdCommands base)

    throughCmds = concatMap (transformPolarityInverted . cdCommands) throughs

-- | Replace all fill and stroke colours with the given colour.
recolorCommands :: (Double, Double, Double, Double) -> [CanvasCmd] -> [CanvasCmd]
recolorCommands (cr, cg, cb, ca) = map rc
  where
    rc CmdFill{} = CmdFill cr cg cb ca
    rc (CmdStroke _ _ _ _ lw) = CmdStroke cr cg cb ca lw
    rc CmdSetFillColor{} = CmdSetFillColor cr cg cb ca
    rc (CmdSetStrokeColor _ _ _ _ lw) = CmdSetStrokeColor cr cg cb ca lw
    rc cmd = cmd

{- | Create a synthetic outline 'CanvasDiagram' from the union bounding box
of a list of diagrams. The outline is a simple filled rectangle matching
the union bounds. Use this when no outline gerber is available to derive
the board shape from the layer extents.
-}
syntheticOutline :: [CanvasDiagram] -> CanvasDiagram
syntheticOutline [] =
    CanvasDiagram
        { cdWidth = 0
        , cdHeight = 0
        , cdBounds = BBox 0 0 0 0
        , cdCommands = []
        , cdPrecision = defaultJsonPrecision
        }
syntheticOutline diagrams@(d : _) =
    let bounds = foldl1 unionBBox (map cdBounds diagrams)
        w = bbMaxX bounds - bbMinX bounds
        h = bbMaxY bounds - bbMinY bounds
        cmds =
            [ CmdBeginPath
            , CmdMoveTo (bbMinX bounds) (bbMinY bounds)
            , CmdLineTo (bbMaxX bounds) (bbMinY bounds)
            , CmdLineTo (bbMaxX bounds) (bbMaxY bounds)
            , CmdLineTo (bbMinX bounds) (bbMaxY bounds)
            , CmdClosePath
            , CmdFill 0 0 0 1
            ]
     in CanvasDiagram
            { cdWidth = w
            , cdHeight = h
            , cdBounds = bounds
            , cdCommands = cmds
            , cdPrecision = cdPrecision d
            }

{- | Assemble a board diagram from shared outline, through-layers, an
optional base colour, and per-layer specs paired with their pre-rendered
raw diagrams.

The base colour (if provided) is rendered as the bottom-most layer: the
outline filled with that colour, with through-layers punched out.
-}
buildBoardDiagram ::
    -- | Board outline (raw)
    CanvasDiagram ->
    -- | Through-layers (raw) — drills, cutouts
    [CanvasDiagram] ->
    -- | Optional board substrate colour (bottom-most layer)
    Maybe (Double, Double, Double, Double) ->
    -- | Per-layer specs paired with their base diagrams
    [(BoardLayerSpec, CanvasDiagram)] ->
    LayeredDiagram
buildBoardDiagram outline throughs mBaseColor entries =
    LayeredDiagram
        { ldWidth = maximum allWidths
        , ldHeight = maximum allHeights
        , ldBounds = foldl1 unionBBox allBounds
        , ldLayers = baseLayer ++ map buildOne entries
        , ldPunch = concatMap (transformPolarityInverted . cdCommands) throughs
        , ldPrecision = cdPrecision outline
        }
  where
    allDiagrams = outline : throughs ++ map snd entries
    allWidths = map cdWidth allDiagrams
    allHeights = map cdHeight allDiagrams
    allBounds = map cdBounds allDiagrams

    filledOutline color =
        optimizeCommands $
            recolorCommands color $
                outlineToFilled (0, 0, 0, 1) (cdCommands outline)

    baseLayer = case mBaseColor of
        Nothing -> []
        Just color ->
            [ MaskLayer
                { mlName = Just "substrate"
                , mlColor = color
                , mlCommands = filledOutline color
                }
            ]

    buildOne (spec, base) =
        MaskLayer
            { mlName = blsName spec
            , mlColor = blsColor spec
            , mlCommands = buildBoardLayerCommands (blsColor spec) (blsOutlineMode spec) base outline []
            }

--------------------------------------------------------------------------------
-- Polarity post-processing
--------------------------------------------------------------------------------

{- | Transform the command stream so that clear-polarity (white) drawing
operations use @destination-out@ compositing.

The @gerber-diagrams@ package renders dark-polarity shapes with black
fill\/stroke and clear-polarity shapes with white fill\/stroke (using
'defaultConfig'). This function walks the flat command list, finds
top-level @Save@\/@Restore@ groups that contain white fills or strokes,
and injects a @globalCompositeOperation = \"destination-out\"@ command
right after the @Save@.
-}
transformPolarity :: [CanvasCmd] -> [CanvasCmd]
transformPolarity = processGroups False

{- | Transform the command stream with inverted polarity: dark shapes
use @destination-out@ to punch holes, and clear shapes are dropped
entirely.

This is used when overlaying an inverted mask layer on top of a base
layer — the mask's material areas cut through the base.
-}
transformPolarityInverted :: [CanvasCmd] -> [CanvasCmd]
transformPolarityInverted = processGroups True

{- | Shared implementation for polarity transforms.

When @inverted@ is 'False' (normal): clear groups get @destination-out@,
dark groups are kept as-is.

When @inverted@ is 'True': dark groups get @destination-out@, clear
groups are dropped.
-}
processGroups :: Bool -> [CanvasCmd] -> [CanvasCmd]
processGroups inverted = go
  where
    go [] = []
    go (CmdSave : rest) =
        let (group, remaining) = extractGroup 1 [] rest
            hasClear = any isClearColour group
            hasDark = any isDarkColour group
         in if inverted
                then
                    if hasDark
                        then CmdSave : CmdSetGlobalCompositeOperation DestinationOut : group ++ go remaining
                        else go remaining -- drop clear groups entirely
                else
                    if hasClear
                        then CmdSave : CmdSetGlobalCompositeOperation DestinationOut : group ++ go remaining
                        else CmdSave : group ++ go remaining
    go (cmd : rest) = cmd : go rest

{- | Extract a balanced Save/Restore group. Returns the group contents
(including the closing Restore) and the remaining commands.
-}
extractGroup :: Int -> [CanvasCmd] -> [CanvasCmd] -> ([CanvasCmd], [CanvasCmd])
extractGroup 0 acc rest = (reverse acc, rest)
extractGroup _ acc [] = (reverse acc, [])
extractGroup depth acc (CmdSave : rest) =
    extractGroup (depth + 1) (CmdSave : acc) rest
extractGroup depth acc (CmdRestore : rest)
    | depth == 1 = (reverse (CmdRestore : acc), rest)
    | otherwise = extractGroup (depth - 1) (CmdRestore : acc) rest
extractGroup depth acc (cmd : rest) =
    extractGroup depth (cmd : acc) rest

-- | Check if a command uses the clear colour (white: ~255, ~255, ~255).
isClearColour :: CanvasCmd -> Bool
isClearColour (CmdFill r g b _a) = isWhite r g b
isClearColour (CmdStroke r g b _a _lw) = isWhite r g b
isClearColour _ = False

-- | Check if a command uses the dark colour (black: ~0, ~0, ~0).
isDarkColour :: CanvasCmd -> Bool
isDarkColour (CmdFill r g b _a) = isBlack r g b
isDarkColour (CmdStroke r g b _a _lw) = isBlack r g b
isDarkColour _ = False

isWhite :: Double -> Double -> Double -> Bool
isWhite r g b = close r 255 && close g 255 && close b 255

isBlack :: Double -> Double -> Double -> Bool
isBlack r g b = close r 0 && close g 0 && close b 0

close :: Double -> Double -> Bool
close x y = abs (x - y) < 0.5
