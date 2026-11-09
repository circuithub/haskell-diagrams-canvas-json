/**
 * diagrams-canvas-json-web
 *
 * TypeScript library for rendering diagrams-canvas-json output using HTML Canvas
 */

export type {
  CanvasDiagram,
  CanvasCommand,
  Color,
  BBox,
  ImageMaskCommand,
} from "./types.js";
export { executeCommands, renderDiagram } from "./renderer.js";
export type { ExecuteOptions, RenderOptions } from "./renderer.js";
export { drawImageMask, prepareRasters, retainRasters } from "./raster.js";
export type { RasterCache, RasterSource } from "./raster.js";
export {
  createViewer,
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
  Viewer,
  ViewerLayer,
  ViewerOptions,
} from "./viewer.js";
