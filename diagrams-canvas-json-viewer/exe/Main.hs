{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, stdin)
import Web.Scotty (get, raw, scotty, setHeader)

--------------------------------------------------------------------------------
-- Command line
--------------------------------------------------------------------------------

-- | Layout modes describe how the JSON payload is rendered.
data Layout
    = -- | Single opaque CanvasDiagram JSON rendered as one black layer.
      LSingle
    | -- | LayeredDiagram JSON (expected to have @layers@ and @bounds@).
      LBoard
    | -- | Array of @{name, bounds, commands}@ rendered in an NxM grid.
      LGrid
    | -- | Array of @{name, bounds, commands}@ rendered as a toggleable stack.
      LStack
    deriving (Show, Eq)

-- | Which in-browser renderer to load.
data Renderer = RCanvas | RPixi deriving (Show, Eq)

-- | Parsed command-line options: layout, renderer, source, port, mirrorH, mirrorV.
data Opts = Opts !Layout !Renderer !(Maybe FilePath) !Int !Bool !Bool

commandParser :: ParserInfo Opts
commandParser =
    info
        (layouts <**> helper)
        ( fullDesc
            <> progDesc
                "Serve pre-rendered diagrams-canvas-json output in a browser. \
                \Reads JSON from FILE or stdin."
        )
  where
    layouts =
        subparser
            ( command "single" (info (mkOpts LSingle) (progDesc "View a single CanvasDiagram JSON"))
                <> command "board" (info (mkOpts LBoard) (progDesc "View a multi-layer board JSON"))
                <> command "grid" (info (mkOpts LGrid) (progDesc "View a layer array JSON in an NxM grid"))
                <> command "stack" (info (mkOpts LStack) (progDesc "View a layer array JSON as a toggleable stack"))
            )

    mkOpts layout =
        Opts layout
            <$> pixiFlag
            <*> optional (argument str (metavar "FILE" <> help "Path to JSON input (default: stdin; \"-\" also reads stdin)"))
            <*> portOpt
            <*> mirrorHFlag
            <*> mirrorVFlag
                <**> helper

    pixiFlag = flag RCanvas RPixi (long "pixi" <> help "Use the PixiJS WebGL renderer instead of Canvas 2D")
    portOpt = option auto (long "port" <> short 'p' <> value 3000 <> metavar "PORT" <> help "Port to serve on (default: 3000)")
    mirrorHFlag = switch (long "mirror-h" <> help "Mirror the view horizontally (flip left/right)")
    mirrorVFlag = switch (long "mirror-v" <> help "Mirror the view vertically (flip top/bottom)")

--------------------------------------------------------------------------------
-- Input
--------------------------------------------------------------------------------

{- | Read the viewer JSON payload, either from disk or stdin. Strict so that
the producer process can exit before we start serving.
-}
readInput :: Maybe FilePath -> IO BL.ByteString
readInput src =
    BL.fromStrict <$> case src of
        Nothing -> BS.hGetContents stdin
        Just "-" -> BS.hGetContents stdin
        Just path -> BS.readFile path

-- | Human-readable label for logs and the page H1.
sourceLabel :: Maybe FilePath -> T.Text
sourceLabel Nothing = "stdin"
sourceLabel (Just "-") = "stdin"
sourceLabel (Just path) = T.pack path

--------------------------------------------------------------------------------
-- JS bundle resolution
--------------------------------------------------------------------------------

-- | Name of the bundled Canvas 2D JS library file.
jsBundleFileName :: FilePath
jsBundleFileName = "diagrams-canvas-json-web.iife.js"

-- | Name of the bundled PixiJS library file.
pixiJsBundleFileName :: FilePath
pixiJsBundleFileName = "diagrams-canvas-json-web-pixi.iife.js"

{- | Resolve the path to the JS bundle for a given renderer.

Uses the @DIAGRAMS_CANVAS_JSON_WEB_DIR@ environment variable if set,
otherwise falls back to the default path relative to the project root.
-}
resolveBundlePath :: Renderer -> IO FilePath
resolveBundlePath renderer = do
    mDir <- lookupEnv "DIAGRAMS_CANVAS_JSON_WEB_DIR"
    let name = bundleFileName renderer
    pure $ case mDir of
        Just dir -> dir </> name
        Nothing -> "diagrams-canvas-json-web" </> "dist" </> name

bundleFileName :: Renderer -> FilePath
bundleFileName RCanvas = jsBundleFileName
bundleFileName RPixi = pixiJsBundleFileName

-- | Read the JS bundle, failing with a helpful message if not found.
readBundle :: Renderer -> IO BL.ByteString
readBundle renderer = do
    path <- resolveBundlePath renderer
    result <- try (BL.readFile path) :: IO (Either SomeException BL.ByteString)
    case result of
        Right bs -> pure bs
        Left _ -> do
            hPutStrLn stderr $ "Error: Could not read JS bundle at: " <> path
            hPutStrLn stderr "Set DIAGRAMS_CANVAS_JSON_WEB_DIR to the directory containing the built JS library."
            hPutStrLn stderr $
                "To build it: cd diagrams-canvas-json-web && npm run "
                    <> case renderer of
                        RCanvas -> "build:bundle"
                        RPixi -> "build:bundle-pixi"
            exitFailure

--------------------------------------------------------------------------------
-- Serving
--------------------------------------------------------------------------------

main :: IO ()
main = do
    Opts layout renderer src port mirrorH mirrorV <- execParser commandParser
    let label = sourceLabel src
    payload <- readInput src
    bundle <- readBundle renderer
    putStrLn $ "Serving " <> layoutName layout <> rendererSuffix renderer <> " viewer at http://localhost:" <> show port
    scotty port $ do
        get "/" $ do
            setHeader "Content-Type" "text/html; charset=utf-8"
            raw . BL.fromStrict . TE.encodeUtf8 $ viewerHtml label layout renderer mirrorH mirrorV

        get "/api/data" $ do
            setHeader "Content-Type" "application/json"
            raw payload

        case renderer of
            RCanvas -> get "/lib/diagrams-canvas-json-web.js" $ do
                setHeader "Content-Type" "application/javascript"
                raw bundle
            RPixi -> get "/lib/diagrams-canvas-json-web-pixi.js" $ do
                setHeader "Content-Type" "application/javascript"
                raw bundle

layoutName :: Layout -> String
layoutName LSingle = "single"
layoutName LBoard = "board"
layoutName LGrid = "grid"
layoutName LStack = "stack"

rendererSuffix :: Renderer -> String
rendererSuffix RCanvas = ""
rendererSuffix RPixi = " (PixiJS)"

--------------------------------------------------------------------------------
-- HTML scaffolding
--------------------------------------------------------------------------------

{- | URL at which the bundle is served (must match the @script@ src in the HTML
and the route wired up in 'main').
-}
bundleUrl :: Renderer -> T.Text
bundleUrl RCanvas = "/lib/diagrams-canvas-json-web.js"
bundleUrl RPixi = "/lib/diagrams-canvas-json-web-pixi.js"

{- | Expression that resolves to the create-viewer function. It is async
(see 'createViewerJs'), so callers prefix the call with @await@ inside an
@async@ function (our @main@ already is).
-}
createCall :: Renderer -> T.Text
createCall _ = "await createViewer"

{- | Defines @createViewer(opts)@, which decodes the layers' raster masks
before creating the viewer so they do not pop in on a later frame. The
viewer holds its layers' bitmaps itself, so the prepared handle is released.
-}
createViewerJs :: Renderer -> T.Text
createViewerJs renderer =
    T.unlines
        [ "async function createViewer(opts) {"
        , "  opts.rasters = await " <> ns <> ".prepareRasters(opts.layers);"
        , "  const viewer = " <> create <> "(opts);"
        , "  opts.rasters.release();"
        , "  return viewer;"
        , "}"
        ]
  where
    (ns, create) = case renderer of
        RCanvas -> ("DiagramsCanvasJson", "DiagramsCanvasJson.createViewer")
        RPixi -> ("DiagramsCanvasJsonPixi", "await DiagramsCanvasJsonPixi.createPixiViewer")

viewerHtml :: T.Text -> Layout -> Renderer -> Bool -> Bool -> T.Text
viewerHtml title layout renderer mirrorH mirrorV =
    T.unlines
        [ "<!DOCTYPE html>"
        , "<html><head>"
        , "<meta charset=\"utf-8\">"
        , "<title>" <> headerText <> "</title>"
        , "<style>"
        , viewerCss
        , "</style>"
        , "</head><body>"
        , "<h1>" <> headerText <> "</h1>"
        , "<div id=\"wrap\"></div>"
        , "<div id=\"error\"></div>"
        , "<script src=\"" <> bundleUrl renderer <> "\"></script>"
        , "<script>"
        , createViewerJs renderer
        , "async function main() {"
        , "  const resp = await fetch('/api/data');"
        , "  if (!resp.ok) throw new Error('HTTP ' + resp.status);"
        , "  const data = await resp.json();"
        , layoutBody layout renderer mirrorH mirrorV
        , "}"
        , "main().catch(e => {"
        , "  document.getElementById('error').textContent = 'Error: ' + e.message;"
        , "});"
        , "</script>"
        , "</body></html>"
        ]
  where
    headerText =
        layoutTitle layout
            <> " - "
            <> escapeHtml title
            <> case renderer of
                RCanvas -> ""
                RPixi -> " (PixiJS)"

layoutTitle :: Layout -> T.Text
layoutTitle LSingle = "Viewer"
layoutTitle LBoard = "Board Viewer"
layoutTitle LGrid = "Grid Viewer"
layoutTitle LStack = "Stack Viewer"

-- | Shared CSS for all viewer pages.
viewerCss :: T.Text
viewerCss =
    T.unlines
        [ "* { margin: 0; padding: 0; box-sizing: border-box; }"
        , "html, body { height: 100%; overflow: hidden; background: #fff;"
        , "  font-family: system-ui, sans-serif; color: #333; }"
        , "body { display: flex; flex-direction: column; }"
        , "h1 { font-size: 1rem; padding: 0.4rem 0.8rem; background: #f5f5f5;"
        , "  border-bottom: 1px solid #ddd; flex-shrink: 0; }"
        , "#wrap { flex: 1; position: relative; overflow: hidden; }"
        , "#error { color: #cc0000; position: absolute; top: 50%; left: 50%;"
        , "  transform: translate(-50%, -50%); }"
        ]

escapeHtml :: T.Text -> T.Text
escapeHtml = T.replace "&" "&amp;" . T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "\"" "&quot;"

{- | Emits the body of the @main()@ JS function: inspects @data@ and invokes
the create-viewer call appropriate for the given layout.
-}
layoutBody :: Layout -> Renderer -> Bool -> Bool -> T.Text
layoutBody LSingle renderer mh mv =
    T.unlines
        [ "  " <> createCall renderer <> "({"
        , "    container: document.getElementById('wrap'),"
        , "    bounds: data.bounds,"
        , "    layers: [{ color: [0,0,0,1], commands: data.commands }],"
        , mirrorOpts mh mv
        , "  });"
        ]
layoutBody LBoard renderer mh mv =
    T.unlines
        [ "  " <> createCall renderer <> "({"
        , "    container: document.getElementById('wrap'),"
        , "    bounds: data.bounds,"
        , "    layers: data.layers.map(function (l) {"
        , "      return Object.assign({}, l, { commands: l.commands.concat(data.punch || []) });"
        , "    }),"
        , mirrorOpts mh mv
        , "  });"
        ]
layoutBody LGrid renderer mh mv = gridLayoutJs (createCall renderer) mh mv
layoutBody LStack renderer mh mv = stackLayoutJs (createCall renderer) mh mv

-- | Emit JS object properties for mirror flags (empty string when both false).
mirrorOpts :: Bool -> Bool -> T.Text
mirrorOpts False False = ""
mirrorOpts mh mv =
    let parts = ["mirrorH: true" | mh] ++ ["mirrorV: true" | mv]
     in "    " <> T.intercalate ", " parts <> ","

{- | Shared JavaScript for computing the NxM grid layout and creating
the viewer with interleaved CommandLayer / MaskLayer / CustomLayer.
-}
gridLayoutJs :: T.Text -> Bool -> Bool -> T.Text
gridLayoutJs call mh mv =
    T.unlines
        [ "  var n = data.length;"
        , "  if (n === 0) return;"
        , ""
        , "  // Find max bounds across all layers for uniform cell sizing"
        , "  var maxW = 0, maxH = 0;"
        , "  for (var i = 0; i < n; i++) {"
        , "    var b = data[i].bounds;"
        , "    maxW = Math.max(maxW, b.maxX - b.minX);"
        , "    maxH = Math.max(maxH, b.maxY - b.minY);"
        , "  }"
        , ""
        , "  // Grid dimensions"
        , "  var cols = Math.ceil(Math.sqrt(n));"
        , "  var rows = Math.ceil(n / cols);"
        , ""
        , "  // Layout constants (diagram units)"
        , "  var pad = maxW * 0.08;"
        , "  var titleH = maxH * 0.08;"
        , "  var cellW = maxW + pad * 2;"
        , "  var cellH = maxH + titleH + pad * 2;"
        , "  var gap = maxW * 0.06;"
        , ""
        , "  var layers = [];"
        , "  var labels = [];"
        , ""
        , "  for (var i = 0; i < n; i++) {"
        , "    var row = Math.floor(i / cols);"
        , "    var col = i % cols;"
        , "    // Cell origin (bottom-left, Y-up)"
        , "    var cx = col * (cellW + gap);"
        , "    var cy = (rows - 1 - row) * (cellH + gap);"
        , ""
        , "    // White background (CommandLayer)"
        , "    layers.push({"
        , "      commands: ["
        , "        ['B'], ['M', cx, cy], ['L', cx + cellW, cy],"
        , "        ['L', cx + cellW, cy + cellH], ['L', cx, cy + cellH],"
        , "        ['Z'], ['F', 255, 255, 255, 1]"
        , "      ]"
        , "    });"
        , ""
        , "    // Gerber content (MaskLayer, translated to cell center)"
        , "    var b = data[i].bounds;"
        , "    var gcx = (b.minX + b.maxX) / 2;"
        , "    var gcy = (b.minY + b.maxY) / 2;"
        , "    var contentCx = cx + cellW / 2;"
        , "    var contentCy = cy + pad + (cellH - titleH - pad * 2) / 2;"
        , "    var ox = contentCx - gcx;"
        , "    var oy = contentCy - gcy;"
        , ""
        , "    layers.push({"
        , "      color: [0, 0, 0, 1],"
        , "      commands: [['S'], ['T', 1, 0, 0, 1, ox, oy]]"
        , "        .concat(data[i].commands)"
        , "        .concat([['R']])"
        , "    });"
        , ""
        , "    labels.push({ text: data[i].name,"
        , "      x: cx + cellW / 2, y: cy + cellH - pad * 0.5 });"
        , "  }"
        , ""
        , "  // Title labels (CustomLayer)"
        , "  var fontSize = titleH * 0.7;"
        , "  layers.push({"
        , "    render: function(ctx, scale) {"
        , "      for (var i = 0; i < labels.length; i++) {"
        , "        var lbl = labels[i];"
        , "        ctx.save();"
        , "        ctx.translate(lbl.x, lbl.y);"
        , "        ctx.scale(1, -1);"
        , "        ctx.font = fontSize + 'px system-ui, sans-serif';"
        , "        ctx.textAlign = 'center';"
        , "        ctx.textBaseline = 'top';"
        , "        ctx.fillStyle = '#333';"
        , "        ctx.fillText(lbl.text, 0, 0);"
        , "        ctx.restore();"
        , "      }"
        , "    }"
        , "  });"
        , ""
        , "  // Overall bounds"
        , "  var totalW = cols * cellW + (cols - 1) * gap;"
        , "  var totalH = rows * cellH + (rows - 1) * gap;"
        , "  var bounds = { minX: -gap, minY: -gap,"
        , "    maxX: totalW + gap, maxY: totalH + gap };"
        , ""
        , "  " <> call <> "({"
        , "    container: document.getElementById('wrap'),"
        , "    bounds: bounds,"
        , "    layers: layers,"
        , mirrorOpts mh mv
        , "  });"
        ]

-- | Shared JavaScript for the stack viewer with a toggleable legend of layers.
stackLayoutJs :: T.Text -> Bool -> Bool -> T.Text
stackLayoutJs call mh mv =
    T.unlines
        [ "  var n = data.length;"
        , "  if (n === 0) return;"
        , ""
        , "  // Distinct colors for layers (HSL wheel)"
        , "  var colors = [];"
        , "  for (var i = 0; i < n; i++) {"
        , "    var hue = (i * 360 / n) % 360;"
        , "    var rad = hue * Math.PI / 180;"
        , "    // Convert HSL(hue, 80%, 45%) to RGB 0-255"
        , "    var s = 0.8, l = 0.45;"
        , "    var c = (1 - Math.abs(2 * l - 1)) * s;"
        , "    var x = c * (1 - Math.abs((hue / 60) % 2 - 1));"
        , "    var m = l - c / 2;"
        , "    var r1, g1, b1;"
        , "    if (hue < 60) { r1=c; g1=x; b1=0; }"
        , "    else if (hue < 120) { r1=x; g1=c; b1=0; }"
        , "    else if (hue < 180) { r1=0; g1=c; b1=x; }"
        , "    else if (hue < 240) { r1=0; g1=x; b1=c; }"
        , "    else if (hue < 300) { r1=x; g1=0; b1=c; }"
        , "    else { r1=c; g1=0; b1=x; }"
        , "    colors.push(["
        , "      Math.round((r1 + m) * 255),"
        , "      Math.round((g1 + m) * 255),"
        , "      Math.round((b1 + m) * 255), 0.7]);"
        , "  }"
        , ""
        , "  // Union of all bounds"
        , "  var ub = { minX: Infinity, minY: Infinity, maxX: -Infinity, maxY: -Infinity };"
        , "  for (var i = 0; i < n; i++) {"
        , "    var b = data[i].bounds;"
        , "    ub.minX = Math.min(ub.minX, b.minX);"
        , "    ub.minY = Math.min(ub.minY, b.minY);"
        , "    ub.maxX = Math.max(ub.maxX, b.maxX);"
        , "    ub.maxY = Math.max(ub.maxY, b.maxY);"
        , "  }"
        , "  var contentW = ub.maxX - ub.minX;"
        , "  var contentH = ub.maxY - ub.minY;"
        , ""
        , "  // Legend layout to the left of the content"
        , "  var legendItemH = contentH * 0.04;"
        , "  var legendGap = legendItemH * 0.3;"
        , "  var legendW = contentW * 0.25;"
        , "  var legendPad = contentW * 0.03;"
        , "  var legendX = ub.minX - legendW - legendPad;"
        , "  // Space for show/hide buttons above legend items"
        , "  var btnH = legendItemH;"
        , "  var btnGap = legendGap;"
        , "  var btnRowH = btnH + btnGap * 2;"
        , "  // Center legend + buttons vertically"
        , "  var legendTotalH = n * legendItemH + (n - 1) * legendGap + btnRowH;"
        , "  var legendTopY = ub.minY + contentH / 2 + legendTotalH / 2;"
        , ""
        , "  // Build layers array and legend metadata"
        , "  var layers = [];"
        , "  var maskLayers = []; // references to the MaskLayer objects"
        , "  var legendItems = [];"
        , ""
        , "  // White background behind the gerber stack"
        , "  var pad = contentW * 0.02;"
        , "  layers.push({"
        , "    commands: ["
        , "      ['B'], ['M', ub.minX - pad, ub.minY - pad],"
        , "      ['L', ub.maxX + pad, ub.minY - pad],"
        , "      ['L', ub.maxX + pad, ub.maxY + pad],"
        , "      ['L', ub.minX - pad, ub.maxY + pad],"
        , "      ['Z'], ['F', 255, 255, 255, 1]"
        , "    ]"
        , "  });"
        , ""
        , "  for (var i = 0; i < n; i++) {"
        , "    var ml = {"
        , "      name: data[i].name,"
        , "      color: colors[i],"
        , "      commands: data[i].commands"
        , "    };"
        , "    layers.push(ml);"
        , "    maskLayers.push(ml);"
        , ""
        , "    var iy = legendTopY - btnRowH - i * (legendItemH + legendGap);"
        , "    legendItems.push({"
        , "      text: data[i].name,"
        , "      color: colors[i],"
        , "      x: legendX,"
        , "      y: iy,"
        , "      w: legendW,"
        , "      h: legendItemH,"
        , "      index: i"
        , "    });"
        , "  }"
        , ""
        , "  // Button positions (Y-up, at the top of the legend)"
        , "  var btnY = legendTopY;"
        , "  var btnW = (legendW - btnGap) / 2;"
        , "  var showAllBtn = { x: legendX, y: btnY, w: btnW, h: btnH };"
        , "  var hideAllBtn = { x: legendX + btnW + btnGap, y: btnY, w: btnW, h: btnH };"
        , ""
        , "  // Legend (CustomLayer)"
        , "  var fontSize = legendItemH * 0.65;"
        , "  var checkSize = legendItemH * 0.5;"
        , "  var btnFontSize = btnH * 0.55;"
        , "  layers.push({"
        , "    render: function(ctx, scale) {"
        , "      // Show All / Hide All buttons"
        , "      function drawBtn(btn, label) {"
        , "        ctx.save();"
        , "        ctx.translate(btn.x, btn.y);"
        , "        ctx.scale(1, -1);"
        , "        ctx.fillStyle = '#e8e8e8';"
        , "        ctx.fillRect(0, 0, btn.w, btn.h);"
        , "        ctx.strokeStyle = '#bbb';"
        , "        ctx.lineWidth = btn.h * 0.04;"
        , "        ctx.strokeRect(0, 0, btn.w, btn.h);"
        , "        ctx.fillStyle = '#333';"
        , "        ctx.font = btnFontSize + 'px system-ui, sans-serif';"
        , "        ctx.textAlign = 'center';"
        , "        ctx.textBaseline = 'middle';"
        , "        ctx.fillText(label, btn.w / 2, btn.h / 2);"
        , "        ctx.restore();"
        , "      }"
        , "      drawBtn(showAllBtn, 'Show All');"
        , "      drawBtn(hideAllBtn, 'Hide All');"
        , ""
        , "      for (var i = 0; i < legendItems.length; i++) {"
        , "        var it = legendItems[i];"
        , "        var vis = maskLayers[it.index].visible !== false;"
        , "        ctx.save();"
        , "        ctx.translate(it.x, it.y);"
        , "        ctx.scale(1, -1);"
        , ""
        , "        // Color swatch"
        , "        var c = it.color;"
        , "        ctx.fillStyle = 'rgba(' + c[0] + ',' + c[1] + ',' + c[2] + ',' + c[3] + ')';"
        , "        ctx.fillRect(0, 0, checkSize, checkSize);"
        , ""
        , "        // Check mark if visible"
        , "        if (vis) {"
        , "          ctx.strokeStyle = '#fff';"
        , "          ctx.lineWidth = checkSize * 0.15;"
        , "          ctx.lineCap = 'round';"
        , "          ctx.beginPath();"
        , "          ctx.moveTo(checkSize * 0.2, checkSize * 0.5);"
        , "          ctx.lineTo(checkSize * 0.45, checkSize * 0.75);"
        , "          ctx.lineTo(checkSize * 0.8, checkSize * 0.25);"
        , "          ctx.stroke();"
        , "        }"
        , ""
        , "        // Layer name"
        , "        ctx.fillStyle = vis ? '#333' : '#aaa';"
        , "        ctx.font = fontSize + 'px system-ui, sans-serif';"
        , "        ctx.textAlign = 'left';"
        , "        ctx.textBaseline = 'middle';"
        , "        ctx.fillText(it.text, checkSize + checkSize * 0.4, checkSize / 2);"
        , ""
        , "        ctx.restore();"
        , "      }"
        , "    }"
        , "  });"
        , ""
        , "  // Bounds: include legend area and buttons"
        , "  var bounds = {"
        , "    minX: legendX - legendPad,"
        , "    minY: Math.min(ub.minY - pad, legendTopY - legendTotalH - legendPad),"
        , "    maxX: ub.maxX + pad,"
        , "    maxY: Math.max(ub.maxY + pad, legendTopY + legendPad)"
        , "  };"
        , ""
        , "  var viewer = " <> call <> "({"
        , "    container: document.getElementById('wrap'),"
        , "    bounds: bounds,"
        , "    layers: layers,"
        , mirrorOpts mh mv
        , "  });"
        , ""
        , "  // Click handler for legend toggle"
        , "  document.getElementById('wrap').addEventListener('click', function(e) {"
        , "    var rect = e.currentTarget.getBoundingClientRect();"
        , "    var t = viewer.getTransform ? viewer.getTransform() : (viewer.then ? null : null);"
        , "    if (!t) return;"
        , "    // Convert click from CSS pixels to diagram space"
        , "    var sx = t.mirrorH ? -t.scale : t.scale;"
        , "    var sy = t.mirrorV ? t.scale : -t.scale;"
        , "    var dx = (e.clientX - rect.left - t.tx) / sx;"
        , "    var dy = (e.clientY - rect.top - t.ty) / sy;"
        , ""
        , "    // Hit test: Show All / Hide All buttons"
        , "    function hitBtn(btn) {"
        , "      return dx >= btn.x && dx <= btn.x + btn.w &&"
        , "             dy <= btn.y && dy >= btn.y - btn.h;"
        , "    }"
        , "    if (hitBtn(showAllBtn)) {"
        , "      for (var j = 0; j < maskLayers.length; j++) maskLayers[j].visible = true;"
        , "      viewer.render();"
        , "      return;"
        , "    }"
        , "    if (hitBtn(hideAllBtn)) {"
        , "      for (var j = 0; j < maskLayers.length; j++) maskLayers[j].visible = false;"
        , "      viewer.render();"
        , "      return;"
        , "    }"
        , ""
        , "    // Hit test: legend items"
        , "    for (var i = 0; i < legendItems.length; i++) {"
        , "      var it = legendItems[i];"
        , "      if (dx >= it.x && dx <= it.x + it.w &&"
        , "          dy <= it.y && dy >= it.y - it.h) {"
        , "        var ml = maskLayers[it.index];"
        , "        ml.visible = ml.visible === false ? true : false;"
        , "        viewer.render();"
        , "        break;"
        , "      }"
        , "    }"
        , "  });"
        ]
