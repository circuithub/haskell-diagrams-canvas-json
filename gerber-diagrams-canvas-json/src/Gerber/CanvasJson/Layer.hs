{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Gerber layers drawn straight to canvas commands.

A gerber layer only ever paints with the dark or the clear polarity, with
fills and round or square ended strokes. Its drawing is kept as runs of
paths sharing a polarity and paint, in drawing order, and each command
stream a renderer needs is written from those runs in one pass, grouped as
'Diagrams.Backend.CanvasJson.optimizeCommands' would group them.

See Note [Canvas gerber layers].
-}
module Gerber.CanvasJson.Layer (
    -- * Layers
    GerberLayer,
    parseGerberLayer,
    layerBounds,
    layerDecimals,
    layerStrokeWidth,
    layerFrame,

    -- * Command streams
    LayerPolarity (..),
    layerCommands,
) where

import Control.Foldl qualified as L
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Vector.Strict qualified as Vector
import Data.Vector.Unboxed qualified as Unboxed
import Data.Word (Word8)
import Diagrams.Backend.CanvasJson (
    BBox (..),
    CanvasCmd (..),
    CanvasDiagram (..),
    CanvasJson,
    CanvasJsonOptions (..),
    CompositeOp (DestinationOut),
    JsonPrecision (..),
    defaultJsonPrecision,
    renderCanvasJson,
    sizeFromSpec,
 )
import Diagrams.Prelude (Any, QDiagram, mkWidth)
import Gerber.Diagrams.CanvasJson (RenderOptions (..))
import Gerber.ApertureDefinition (ApertureDefinition)
import Gerber.ApertureDefinition qualified as ApertureDefinition
import Gerber.Command qualified as Gerber
import Gerber.Diagrams (gerberToDiagram)
import Gerber.Evaluate (evaluate)
import Gerber.Evaluate qualified as Gerber
import Gerber.Evaluate.Edge qualified as Edge
import Gerber.Format (Format (..))
import Gerber.Grammar (foldGerber)
import Gerber.MacroDefinition qualified as MacroDefinition
import Gerber.Mirroring qualified as Mirroring
import Gerber.Polarity qualified as Polarity
import Gerber.Unit qualified as Unit
import Linear (V2 (..))
import Text.Megaparsec (errorBundlePretty)

{- Note [Canvas gerber layers]

A large copper layer has millions of shapes, so they are written as canvas
paths as they are evaluated rather than drawn as diagrams and rendered: no
diagram, command group per shape or merging pass is held.

Each path is packed into one vector of tags and one of coordinates, and
gerber transforms are applied to it as it is drawn. Arcs become the cubic
Béziers diagrams draws for them (quarter turns, then the remainder), and
whole anticlockwise circles become one arc command, as the canvas backend
sends them, so the layer looks as its diagram does.

Only shapes made with a path boolean go through diagrams still: apertures
with holes, and macros with thermals, moirés or primitives with exposure
off. They are rare, and drawing them through diagrams keeps them as they
were.
-}

-- | The layer as drawn so far, its newest run first; see Note [Canvas gerber layers].
newtype Layer = Layer [Run]

data Run = Run
    { dark :: !Bool
    , paint :: !Paint
    , paths :: ![Path]
    -- ^ Newest first while the layer is drawn.
    }

data Paint
    = Fill
    | -- | Width and canvas line cap.
      Stroke !Double !Int
    | -- | Drawn with a zero width aperture: in the bounds, as diagrams measures it, but not painted.
      Unpainted
    deriving (Eq)

{- | One canvas path. Each tag takes its coordinates in turn from 'coordinates':
move and line two, cubic six, circle four (centre, radius, start angle),
close none.
-}
data Path = Path
    { tags :: !(Unboxed.Vector Word8)
    , coordinates :: !(Unboxed.Vector Double)
    }

data Segment
    = MoveTo !Double !Double
    | LineTo !Double !Double
    | CubicTo !Double !Double !Double !Double !Double !Double
    | -- | A whole circle as its own closed subpath: centre, radius, start angle, anticlockwise.
      Circle !Double !Double !Double !Double !Bool
    | Close

instance Semigroup Layer where
    -- The evaluator draws newest on the left.
    Layer newer <> Layer older = Layer (joinRuns newer older)

instance Monoid Layer where
    mempty = Layer []

-- The newest run of the older drawing continues the oldest of the newer one when they paint alike.
joinRuns :: [Run] -> [Run] -> [Run]
joinRuns [] older = older
joinRuns [run] (previous : older)
    | run.dark == previous.dark && run.paint == previous.paint =
        let !joined = Run{dark = previous.dark, paint = previous.paint, paths = prependAll run.paths previous.paths}
         in joined : older
joinRuns (run : runs) older = let !rest = joinRuns runs older in run : rest

-- Strict in the spine, unlike '(++)', so runs joined while drawing leave no thunks.
{- HLINT ignore prependAll "Use foldr" -}
prependAll :: [a] -> [a] -> [a]
prependAll [] ys = ys
prependAll (x : xs) ys = let !rest = prependAll xs ys in x : rest

-- | A gerber layer ready to be written as canvas commands.
data GerberLayer = GerberLayer
    { bounds :: !BBox
    , decimals :: !(Maybe Int)
    , runs :: !(Vector.Vector FinishedRun)
    }

data FinishedRun = FinishedRun
    { dark :: !Bool
    , paint :: !Paint
    , paths :: !(Vector.Vector Path)
    }

{- | The bounds of the paths drawn, as diagrams measures them: stroke widths are
left out. An empty layer has empty bounds at the origin.
-}
layerBounds :: GerberLayer -> BBox
layerBounds layer = layer.bounds

{- | The decimal places of the layer's coordinates in millimetres, from its
format specification; two more for a layer in inches.
-}
layerDecimals :: GerberLayer -> Maybe Int
layerDecimals layer = layer.decimals

-- | The widest stroke, which the bounds leave out; zero without strokes.
layerStrokeWidth :: GerberLayer -> Double
layerStrokeWidth layer = maximum (0 : [width | FinishedRun{paint = Stroke width _} <- Vector.toList layer.runs])

{- | The layer's size, bounds and precision as a diagram without commands,
sized and with the precision 'Gerber.Diagrams.CanvasJson.renderGerberRaw' gives.
-}
layerFrame :: RenderOptions -> GerberLayer -> CanvasDiagram
layerFrame options layer =
    CanvasDiagram
        { cdWidth = width
        , cdHeight = height
        , cdBounds = layer.bounds
        , cdCommands = []
        , cdPrecision = maybe (roPrecision options) (\dp -> (roPrecision options){jpCoordinates = dp, jpDimensions = dp}) layer.decimals
        }
  where
    V2 width height = sizeFromSpec 400 (roSize options)

parseGerberLayer :: Text -> Either String GerberLayer
parseGerberLayer =
    either (Left . errorBundlePretty) Right
        . foldGerber (finishLayer <$> evaluate gerberToLayer <*> decimalsFold)

finishLayer :: Layer -> Maybe Int -> GerberLayer
finishLayer (Layer newestFirst) decimals =
    GerberLayer
        { bounds = case extent of
            Extent x0 y0 x1 y1 | x0 <= x1 -> BBox x0 y0 x1 y1
            _ -> BBox 0 0 0 0
        , decimals
        , runs = Vector.fromList (reverse (map finish newestFirst))
        }
  where
    finish run = FinishedRun{dark = run.dark, paint = run.paint, paths = Vector.fromList (reverse run.paths)}
    extent = foldl' (\acc run -> foldl' (\acc' path -> acc' <> pathExtent path) acc run.paths) noExtent newestFirst

decimalsFold :: L.Fold Gerber.Command (Maybe Int)
decimalsFold = places <$> L.find isFormat <*> L.find isUnit
  where
    isFormat = \case Gerber.FS{} -> True; _ -> False
    isUnit = \case Gerber.MO{} -> True; _ -> False
    places format unit = case format of
        Just (Gerber.FS _ xFormat _) -> Just $ case unit of
            Just (Gerber.MO Unit.IN) -> decimalPositions xFormat + 2
            _ -> decimalPositions xFormat
        _ -> Nothing

-- | Which polarity a layer's shapes paint with.
data LayerPolarity
    = -- | Dark shapes paint and clear shapes erase.
      Normal
    | -- | Dark shapes erase and clear shapes are left out, to punch the layer through another.
      Inverted
    deriving (Eq, Show)

{- | The layer as canvas commands, a group per run. Without a colour, dark is
black and clear is white.
-}
layerCommands :: LayerPolarity -> Maybe (Double, Double, Double, Double) -> GerberLayer -> [CanvasCmd]
layerCommands polarity colour layer = foldr runCommands [] layer.runs
  where
    runCommands run rest
        | run.paint == Unpainted = rest
        | polarity == Inverted && not run.dark = rest
        | otherwise =
            CmdSave
                : context run
                ++ [CmdSetGlobalCompositeOperation DestinationOut | erases run]
                ++ foldr (\path more -> CmdBeginPath : pathCommands path (draw run : more)) (CmdRestore : rest) run.paths

    erases run = case polarity of
        Normal -> not run.dark
        Inverted -> run.dark

    rgba run = case colour of
        Just c -> c
        Nothing -> if run.dark then (0, 0, 0, 1) else (255, 255, 255, 1)

    context run =
        let (r, g, b, a) = rgba run
         in case run.paint of
                Fill -> [CmdSetFillColor r g b a]
                Stroke width cap -> [CmdSetStrokeColor r g b a width, CmdSetLineCap cap]
                Unpainted -> []

    draw run = case run.paint of
        Stroke{} -> CmdStrokeCurrent
        _ -> CmdFillCurrent

pathCommands :: Path -> [CanvasCmd] -> [CanvasCmd]
pathCommands path rest = foldr segmentCommands rest (segments path)
  where
    segmentCommands segment more = case segment of
        MoveTo x y -> CmdMoveTo x y : more
        LineTo x y -> CmdLineTo x y : more
        CubicTo x1 y1 x2 y2 x y -> CmdBezierTo x1 y1 x2 y2 x y : more
        Circle cx cy r start anticlockwise
            | anticlockwise -> CmdMoveTo (cx + r * cos start) (cy + r * sin start) : CmdArc cx cy r start (start + 2 * pi) : CmdClosePath : more
            | otherwise -> foldr segmentCommands more (circleSegments cx cy r start False)
        Close -> CmdClosePath : more

--------------------------------------------------------------------------------
-- Paths

pack :: [Segment] -> Path
pack segments' =
    Path
        { tags = Unboxed.fromList (map tag segments')
        , coordinates = Unboxed.fromList (concatMap coords segments')
        }
  where
    tag = \case
        MoveTo{} -> 0
        LineTo{} -> 1
        CubicTo{} -> 2
        Circle _ _ _ _ True -> 3
        Circle _ _ _ _ False -> 4
        Close -> 5
    coords = \case
        MoveTo x y -> [x, y]
        LineTo x y -> [x, y]
        CubicTo x1 y1 x2 y2 x y -> [x1, y1, x2, y2, x, y]
        Circle cx cy r start _ -> [cx, cy, r, start]
        Close -> []

segments :: Path -> [Segment]
segments path = go 0 0
  where
    go !t !c
        | t >= Unboxed.length path.tags = []
        | otherwise = case Unboxed.unsafeIndex path.tags t of
            0 -> MoveTo (at c) (at (c + 1)) : go (t + 1) (c + 2)
            1 -> LineTo (at c) (at (c + 1)) : go (t + 1) (c + 2)
            2 -> CubicTo (at c) (at (c + 1)) (at (c + 2)) (at (c + 3)) (at (c + 4)) (at (c + 5)) : go (t + 1) (c + 6)
            3 -> Circle (at c) (at (c + 1)) (at (c + 2)) (at (c + 3)) True : go (t + 1) (c + 4)
            4 -> Circle (at c) (at (c + 1)) (at (c + 2)) (at (c + 3)) False : go (t + 1) (c + 4)
            _ -> Close : go (t + 1) c
    at = Unboxed.unsafeIndex path.coordinates

-- | A whole circle as a closed run of Béziers.
circleSegments :: Double -> Double -> Double -> Double -> Bool -> [Segment]
circleSegments cx cy r start anticlockwise =
    MoveTo (cx + r * cos start) (cy + r * sin start)
        : arcSegments cx cy r start (if anticlockwise then 2 * pi else -(2 * pi))
        ++ [Close]

{- | The Béziers diagrams draws for an arc from a start angle through a sweep,
anticlockwise when positive: quarter turns and then the remainder, leaving
out a remainder under 0.0001 radians.
-}
arcSegments :: Double -> Double -> Double -> Double -> Double -> [Segment]
arcSegments cx cy r start sweep
    | abs sweep < 0.0001 = []
    | otherwise =
        let step = if abs sweep <= pi / 2 then sweep else signum sweep * pi / 2
            end = start + step
            k = 4 / 3 * tan (step / 4)
            (x0, y0) = point start
            (x1, y1) = point end
         in CubicTo (x0 - k * r * sin start) (y0 + k * r * cos start) (x1 + k * r * sin end) (y1 - k * r * cos end) x1 y1
                : arcSegments cx cy r end (sweep - step)
  where
    point angle = (cx + r * cos angle, cy + r * sin angle)

-- | An arc too short to draw goes straight to its end, so the path still reaches it.
arcOrLine :: Double -> Double -> Double -> Double -> Double -> (Double, Double) -> [Segment]
arcOrLine cx cy r start sweep (ex, ey) = case arcSegments cx cy r start sweep of
    [] -> [LineTo ex ey]
    segments' -> segments'

-- | @x' = a x + b y + tx@ and @y' = c x + d y + ty@.
data Affine = Affine !Double !Double !Double !Double !Double !Double

transformPath :: Affine -> Path -> Path
transformPath (Affine a b c d tx ty) path = pack (concatMap move (segments path))
  where
    determinant = a * d - b * c
    factor = sqrt (abs determinant)
    apply x y = (a * x + b * y + tx, c * x + d * y + ty)
    move = \case
        MoveTo x y -> [uncurry MoveTo (apply x y)]
        LineTo x y -> [uncurry LineTo (apply x y)]
        CubicTo x1 y1 x2 y2 x y ->
            let (x1', y1') = apply x1 y1
                (x2', y2') = apply x2 y2
                (x', y') = apply x y
             in [CubicTo x1' y1' x2' y2' x' y']
        Circle cx cy r start anticlockwise ->
            let (cx', cy') = apply cx cy
                (sx, sy) = apply (cx + r * cos start) (cy + r * sin start)
             in [Circle cx' cy' (r * factor) (atan2 (sy - cy') (sx - cx')) (if determinant < 0 then not anticlockwise else anticlockwise)]
        Close -> [Close]

data Extent = Extent !Double !Double !Double !Double

instance Semigroup Extent where
    Extent a b c d <> Extent a' b' c' d' = Extent (min a a') (min b b') (max c c') (max d d')

-- | Covers nothing: its low corner is above its high one.
noExtent :: Extent
noExtent = Extent (1 / 0) (1 / 0) (-(1 / 0)) (-(1 / 0))

data Walk = Walk !Double !Double !Extent

pointExtent :: Double -> Double -> Extent
pointExtent x y = Extent x y x y

-- | Exact, as diagrams measures a path: Béziers by their extremes.
pathExtent :: Path -> Extent
pathExtent path = let Walk _ _ extent = foldl' step (Walk 0 0 noExtent) (segments path) in extent
  where
    step walk@(Walk px py extent) = \case
        MoveTo x y -> Walk x y (extent <> pointExtent x y)
        LineTo x y -> Walk x y (extent <> pointExtent px py <> pointExtent x y)
        CubicTo x1 y1 x2 y2 x y -> Walk x y (extent <> cubicExtent px x1 x2 x py y1 y2 y)
        Circle cx cy r _ _ -> Walk px py (extent <> Extent (cx - r) (cy - r) (cx + r) (cy + r))
        Close -> walk
    cubicExtent x0 x1 x2 x3 y0 y1 y2 y3 =
        let (lowX, highX) = cubicRange x0 x1 x2 x3
            (lowY, highY) = cubicRange y0 y1 y2 y3
         in Extent lowX lowY highX highY

-- The range of one coordinate of a cubic Bézier, at its ends and where its derivative is zero.
cubicRange :: Double -> Double -> Double -> Double -> (Double, Double)
cubicRange p0 p1 p2 p3 = (minimum values, maximum values)
  where
    values = p0 : p3 : [at t | t <- roots, t > 0, t < 1]
    at t = let u = 1 - t in u * u * u * p0 + 3 * u * u * t * p1 + 3 * u * t * t * p2 + t * t * t * p3
    -- The derivative divided by three is a t^2 + b t + c.
    a = -p0 + 3 * p1 - 3 * p2 + p3
    b = 2 * (p0 - 2 * p1 + p2)
    c = p1 - p0
    roots
        | abs a < 1e-12 = [-(c / b) | abs b >= 1e-12]
        | discriminant < 0 = []
        | otherwise = [(-b + sqrt discriminant) / (2 * a), (-b - sqrt discriminant) / (2 * a)]
    discriminant = b * b - 4 * a * c

--------------------------------------------------------------------------------
-- Evaluation

gerberToLayer :: Gerber.Evaluator Layer
gerberToLayer =
    Gerber.Evaluator
        { Gerber.line = \polarity aperture (x1, y1) (x2, y2) ->
            stroked polarity aperture [MoveTo x1 y1, LineTo x2 y2]
        , Gerber.arc = \polarity aperture (x0, y0) (cx, cy) (x1, y1) ->
            let radius = sqrt ((x0 - cx) ^ (2 :: Int) + (y0 - cy) ^ (2 :: Int))
                start = atan2 (y0 - cy) (x0 - cx)
                sweep = anticlockwiseSweep (x0, y0) (cx, cy) (x1, y1)
             in if nearZero radius
                    then mempty
                    else stroked polarity aperture (MoveTo x0 y0 : arcOrLine cx cy radius start sweep (x1, y1))
        , Gerber.flash = \polarity aperture -> case flashPaths aperture of
            Just paths' -> filled polarity paths'
            Nothing -> filled polarity (throughDiagrams (Gerber.flash gerberToDiagram polarity aperture))
        , Gerber.flashMacro = \polarity primitives -> case traverse macroPrimitive primitives of
            Just paths' -> filled polarity (concat paths')
            Nothing -> filled polarity (throughDiagrams (Gerber.flashMacro gerberToDiagram polarity primitives))
        , Gerber.fillRegion = \polarity start edges ->
            if null edges then mempty else filled polarity [pack (regionSegments start edges)]
        , Gerber.translate = \x y -> transform (Affine 1 0 0 1 x y)
        , Gerber.mirror = \case
            Mirroring.MirrorNone -> id
            Mirroring.MirrorX -> transform (Affine (-1) 0 0 1 0 0)
            Mirroring.MirrorY -> transform (Affine 1 0 0 (-1) 0 0)
            Mirroring.MirrorXY -> transform (Affine (-1) 0 0 (-1) 0 0)
        , Gerber.rotate = \degrees -> transform (rotation (degrees * pi / 180))
        , -- diagrams cannot scale by zero, so draws nothing
          Gerber.scale = \factor -> if factor == 0 then const mempty else transform (Affine factor 0 0 factor 0 0)
        }

filled :: Polarity.Polarity -> [Path] -> Layer
filled polarity = \case
    [] -> mempty
    paths' -> Layer [Run{dark = polarity == Polarity.Dark, paint = Fill, paths = reverse paths'}]

-- | Only circles and rectangles can be stroked, a rectangle as wide as it is with square ends.
stroked :: Polarity.Polarity -> ApertureDefinition -> [Segment] -> Layer
stroked polarity aperture segments' = case aperture of
    ApertureDefinition.Circle modifiers -> with modifiers.diameter 1
    ApertureDefinition.Rectangle modifiers -> with modifiers.width 2
    _ -> mempty
  where
    with width cap =
        let !path = pack segments'
         in Layer [Run{dark = polarity == Polarity.Dark, paint = if width > 0 then Stroke width cap else Unpainted, paths = [path]}]

transform :: Affine -> Layer -> Layer
transform affine@(Affine a b c d _ _) (Layer runs') = Layer (strictMap moveRun runs')
  where
    factor = sqrt (abs (a * d - b * c))
    moveRun run =
        Run
            { dark = run.dark
            , paint = case run.paint of
                Stroke width cap -> Stroke (width * factor) cap
                paint -> paint
            , paths = strictMap (transformPath affine) run.paths
            }

strictMap :: (a -> b) -> [a] -> [b]
strictMap f = \case
    [] -> []
    x : xs -> let !y = f x; !ys = strictMap f xs in y : ys

-- Right angles are snapped so boxes stay axis aligned.
rotation :: Double -> Affine
rotation radians = Affine cosine (-sine) sine cosine 0 0
  where
    cosine = snapped (cos radians)
    sine = snapped (sin radians)
    snapped value = if abs value < 1e-15 then 0 else value

rotated :: Double -> [Segment] -> Path
rotated degrees = transformPath (rotation (degrees * pi / 180)) . pack

nearZero :: Double -> Bool
nearZero x = abs x <= 1e-12

{- | The anticlockwise sweep from the start to the end direction, in [0, 2π), as
diagrams draws it; ends that meet make a whole turn.

Ends apart in directions within rounding of each other sweep nothing, rather
than nothing or a whole turn depending on the rounding.
-}
anticlockwiseSweep :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
anticlockwiseSweep (x0, y0) (cx, cy) (x1, y1)
    | nearZero (x0 - x1) && nearZero (y0 - y1) = 2 * pi
    | sweep < 1e-9 || sweep > 2 * pi - 1e-9 = 0
    | otherwise = sweep
  where
    difference = atan2 (y1 - cy) (x1 - cx) - atan2 (y0 - cy) (x0 - cx)
    sweep = if difference < 0 then difference + 2 * pi else difference

regionSegments :: (Double, Double) -> [Edge.Edge] -> [Segment]
regionSegments start@(startX, startY) edges = MoveTo startX startY : go start edges
  where
    go _ [] = [Close]
    go (px, py) (edge : rest) = case edge of
        Edge.Line (x, y) -> LineTo x y : go (x, y) rest
        Edge.ArcCCW centre end -> arcTo (px, py) centre end True ++ go end rest
        Edge.ArcCW centre end -> arcTo (px, py) centre end False ++ go end rest
    arcTo from@(fx, fy) centre@(cx, cy) end@(ex, ey) anticlockwise
        | nearZero radius = [LineTo ex ey]
        | otherwise = arcOrLine cx cy radius (atan2 (fy - cy) (fx - cx)) sweep end
      where
        radius = sqrt ((fx - cx) ^ (2 :: Int) + (fy - cy) ^ (2 :: Int))
        sweep
            | anticlockwise = anticlockwiseSweep from centre end
            | otherwise = negate (anticlockwiseSweep end centre from)

-- | A flash drawn without a path boolean; 'Nothing' for an aperture with a hole.
flashPaths :: ApertureDefinition -> Maybe [Path]
flashPaths = \case
    ApertureDefinition.Circle modifiers
        | Nothing <- modifiers.circleHoleDiameter -> Just (circle 0 0 (modifiers.diameter / 2) 0)
    ApertureDefinition.Rectangle modifiers
        | Nothing <- modifiers.rectangleHoleDiameter -> Just (rectangle modifiers.width modifiers.height)
    ApertureDefinition.Obround modifiers
        | Nothing <- modifiers.rectangleHoleDiameter -> Just (obround (abs modifiers.width) (abs modifiers.height))
    ApertureDefinition.Polygon modifiers
        | Nothing <- modifiers.polygonHoleDiameter ->
            Just (maybe id (\degrees -> map (transformPath (rotation (degrees * pi / 180)))) modifiers.rotation (regularPolygon modifiers.numberOfVertices (modifiers.outerDiameter / 2)))
    _ -> Nothing

circle :: Double -> Double -> Double -> Double -> [Path]
circle cx cy r start
    | nearZero r = []
    | otherwise = [pack [Circle cx cy (abs r) start True]]

-- | Centred on the origin; kept when it has no area, as diagrams keeps it.
rectangle :: Double -> Double -> [Path]
rectangle width height = [pack (polygonSegments [(-w, -h), (w, -h), (w, h), (-w, h)])]
  where
    w = abs width / 2
    h = abs height / 2

-- diagrams clamps the corner radius of the rounded rectangle to half the shorter side.
obround :: Double -> Double -> [Path]
obround width height
    | width == 0 || height == 0 = []
    | width == height = circle 0 0 (width / 2) 0
    | width > height =
        let r = height / 2
            x = (width - height) / 2
         in [pack ([MoveTo (-x) (-r), LineTo x (-r)] ++ arcSegments x 0 r (-(pi / 2)) pi ++ [LineTo (-x) r] ++ arcSegments (-x) 0 r (pi / 2) pi ++ [Close])]
    | otherwise =
        let r = width / 2
            y = (height - width) / 2
         in [pack ([MoveTo r (-y), LineTo r y] ++ arcSegments 0 y r 0 pi ++ [LineTo (-r) (-y)] ++ arcSegments 0 (-y) r pi pi ++ [Close])]

-- | Centred on the origin with a vertex on the positive x axis, as diagrams draws it.
regularPolygon :: Int -> Double -> [Path]
regularPolygon count radius
    | count < 3 || radius == 0 = []
    | otherwise = [pack (polygonSegments [(radius * cos (angle k), radius * sin (angle k)) | k <- [0 .. count - 1]])]
  where
    angle k = 2 * pi * fromIntegral k / fromIntegral count

polygonSegments :: [(Double, Double)] -> [Segment]
polygonSegments = \case
    [] -> []
    (x, y) : rest -> MoveTo x y : map (uncurry LineTo) rest ++ [Close]

{- | A macro primitive drawn without a path boolean: 'Nothing' for a thermal,
a moiré or a primitive with exposure off.
-}
macroPrimitive :: MacroDefinition.Primitive MacroDefinition.Exposure Double -> Maybe [Path]
macroPrimitive = \case
    MacroDefinition.Circle modifiers -> exposed modifiers.circleExposure $
        let V2 x y = modifiers.circleCenter
         in maybe id (\degrees -> map (transformPath (rotation (degrees * pi / 180)))) modifiers.circleRotation (circle x y (modifiers.diameter / 2) 0)
    MacroDefinition.VectorLine modifiers -> exposed modifiers.vectorLineExposure $
        let V2 sx sy = modifiers.start
            V2 ex ey = modifiers.end
            along = sqrt ((ex - sx) ^ (2 :: Int) + (ey - sy) ^ (2 :: Int))
            placed = rotation (modifiers.vectorLineRotation * pi / 180) `compose` Affine 1 0 0 1 ((sx + ex) / 2) ((sy + ey) / 2) `compose` rotation (atan2 (ey - sy) (ex - sx))
         in map (transformPath placed) (rectangle along modifiers.vectorLineWidth)
    MacroDefinition.CenterLine modifiers -> exposed modifiers.centerLineExposure $
        let V2 x y = modifiers.centerLineCenter
         in map (transformPath (rotation (modifiers.centerLineRotation * pi / 180) `compose` Affine 1 0 0 1 x y)) (rectangle modifiers.centerLineWidth modifiers.height)
    MacroDefinition.Outline modifiers -> exposed modifiers.outlineExposure
        [rotated modifiers.outlineRotation (polygonSegments [(x, y) | V2 x y <- NonEmpty.toList modifiers.vertices])]
    -- The centre is ignored as "Gerber.Diagrams" ignores it.
    MacroDefinition.Polygon modifiers -> exposed modifiers.polygonExposure $
        map (transformPath (rotation (modifiers.polygonRotation * pi / 180))) (regularPolygon modifiers.numVertices (modifiers.polygonDiameter / 2))
    MacroDefinition.Moire{} -> Nothing
    MacroDefinition.Thermal{} -> Nothing
  where
    exposed exposure paths' = case exposure of
        MacroDefinition.ExposureOn -> Just paths'
        MacroDefinition.ExposureOff -> Nothing

-- | The first is applied after the second.
compose :: Affine -> Affine -> Affine
compose (Affine a b c d tx ty) (Affine a' b' c' d' tx' ty') =
    Affine (a * a' + b * c') (a * b' + b * d') (c * a' + d * c') (c * b' + d * d') (a * tx' + b * ty' + tx) (c * tx' + d * ty' + ty)

-- | The paths diagrams draws for a shape, read back from its canvas commands.
throughDiagrams :: QDiagram CanvasJson V2 Double Any -> [Path]
throughDiagrams diagram = map pack (go [] (cdCommands (renderCanvasJson (CanvasJsonOptions (mkWidth 400) defaultJsonPrecision) diagram)))
  where
    go current = \case
        [] -> [reverse current | not (null current)]
        CmdBeginPath : rest -> [reverse current | not (null current)] ++ go [] rest
        CmdMoveTo _ _ : CmdArc cx cy r start _ : CmdClosePath : rest -> go (Circle cx cy r start True : current) rest
        CmdMoveTo x y : rest -> go (MoveTo x y : current) rest
        CmdLineTo x y : rest -> go (LineTo x y : current) rest
        CmdBezierTo x1 y1 x2 y2 x y : rest -> go (CubicTo x1 y1 x2 y2 x y : current) rest
        CmdClosePath : rest -> go (Close : current) rest
        _ : rest -> go current rest
