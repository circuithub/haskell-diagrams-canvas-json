/**
 * PixiJS rendering backend for diagrams-canvas-json-web
 *
 * Import from "diagrams-canvas-json-web/pixi" to use the PixiJS renderer.
 * Requires pixi.js as a dependency.
 */

export type {
  CanvasDiagram,
  CanvasCommand,
  Color,
  BBox,
  ImageMaskCommand,
} from "./types.js";
export {
  isCommandLayer,
  isCustomLayer,
  isMaskLayer,
  punchedLayers,
} from "./viewer.js";
export type {
  CommandLayer,
  CustomLayer,
  CustomLayerRenderer,
  LayeredDiagram,
  MaskLayer,
  ViewerLayer,
} from "./viewer.js";
export {
  executeCommandsPixi,
  renderDiagramPixi,
  createPixiApp,
  toMaskCommands,
} from "./renderer-pixi.js";
export type { RenderPixiOptions, RasterTextures } from "./renderer-pixi.js";
export { createRasterTextures } from "./renderer-pixi.js";
export {
  drawImageMask,
  preparePixiRasters as prepareRasters,
} from "./raster.js";
export type { RasterCache, RasterSource } from "./raster.js";
export { createPixiViewer } from "./viewer-pixi.js";
export type { PixiViewer, PixiViewerOptions } from "./viewer-pixi.js";
