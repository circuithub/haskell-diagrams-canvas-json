import type { BBox, CanvasCommand } from "./types.js";
import { executeCommands } from "./renderer.js";
import { retainRasters, sameLayerCommands } from "./raster.js";
import type { RasterCache } from "./raster.js";

/** A layer with pre-colored canvas commands rendered as-is */
export interface CommandLayer {
  name?: string;
  visible?: boolean;
  commands: CanvasCommand[];
}

/**
 * A mask layer: commands define a white-on-transparent shape, tinted with color.
 * In the Canvas 2D viewer the commands are rendered directly (colors are pre-baked
 * by the backend). In the PixiJS viewer the commands are rendered as a mask texture
 * and the sprite is tinted with the layer color.
 */
export interface MaskLayer {
  name?: string;
  visible?: boolean;
  color: [number, number, number, number];
  commands: CanvasCommand[];
}

/** Multi-layer diagram structure from the backend */
export interface LayeredDiagram {
  width: number;
  height: number;
  bounds: BBox;
  layers: MaskLayer[];
  /** Drawn after every layer's own commands, kept once rather than in each layer */
  punch?: CanvasCommand[];
}

/** The layers with the diagram's punch drawn into each, as they are composited */
export function punchedLayers(diagram: LayeredDiagram): MaskLayer[] {
  const punch = diagram.punch;
  if (!punch || punch.length === 0) return diagram.layers;
  return diagram.layers.map((layer) => ({
    ...layer,
    commands: layer.commands.concat(punch),
  }));
}

/**
 * Callback for rendering a custom canvas layer.
 * Called during each render pass with the canvas context already set up
 * with the current pan/zoom transform (scale, 0, 0, -scale, tx, ty).
 *
 * @param ctx - The 2D rendering context with pan/zoom transform applied
 * @param scale - Current zoom scale (useful for adjusting line widths)
 */
export type CustomLayerRenderer = (
  ctx: CanvasRenderingContext2D,
  scale: number,
) => void;

/** Configuration for a custom canvas layer */
export interface CustomLayer {
  name?: string;
  visible?: boolean;
  /** Render callback invoked each frame */
  render: CustomLayerRenderer;
}

/** A viewer layer — command, mask, or custom */
export type ViewerLayer = CommandLayer | MaskLayer | CustomLayer;

/** Type guard: is this a MaskLayer? (has both color and commands) */
export function isMaskLayer(layer: ViewerLayer): layer is MaskLayer {
  return "commands" in layer && "color" in layer;
}

/** Type guard: is this a CommandLayer? (has commands but no color) */
export function isCommandLayer(layer: ViewerLayer): layer is CommandLayer {
  return "commands" in layer && !("color" in layer);
}

/** Type guard: is this a CustomLayer? (has render callback) */
export function isCustomLayer(layer: ViewerLayer): layer is CustomLayer {
  return "render" in layer;
}

/** Type guard: does this layer have commands? (CommandLayer or MaskLayer) */
function hasCommands(layer: ViewerLayer): layer is CommandLayer | MaskLayer {
  return "commands" in layer;
}

/** Options for creating a pan-zoom viewer */
export interface ViewerOptions {
  /** Container element that the viewer will fill */
  container: HTMLElement;

  /** Diagram bounds for computing the fit transform */
  bounds: BBox;

  /**
   * Background CSS value for the container.
   * Defaults to a checkerboard pattern that shows transparency.
   * Set to a color string like '#ffffff' for a solid background,
   * or null to inherit the container's existing background.
   */
  background?: string | null;

  /** Padding factor for fitting diagram (0-1, default: 0.9 = 10% padding) */
  padding?: number;

  /** Layers to render (bottom to top), command and custom layers interleaved */
  layers?: ViewerLayer[];

  /** Mirror the image horizontally (flip left/right). Default: false. */
  mirrorH?: boolean;

  /** Mirror the image vertically (flip top/bottom). Default: false. */
  mirrorV?: boolean;

  /**
   * Bitmaps from {@link prepareRasters}. IM commands not prepared are decoded
   * lazily and appear on a later frame.
   */
  rasters?: RasterCache;
}

/** Handle returned by createViewer for controlling the viewer */
export interface Viewer {
  /** Force a re-render */
  render(): void;

  /** Reset the view to fit the diagram in the viewport */
  fitToViewport(): void;

  /** Clean up event listeners and DOM elements */
  destroy(): void;

  /** Update the layers and re-render */
  setLayers(layers: ViewerLayer[]): void;

  /** Get the current view transform state */
  getTransform(): {
    scale: number;
    tx: number;
    ty: number;
    mirrorH: boolean;
    mirrorV: boolean;
  };

  /** Set the view transform state and re-render */
  setTransform(scale: number, tx: number, ty: number): void;
}

const DEFAULT_CHECKERBOARD =
  "conic-gradient(#d5d0e6 90deg, #cde0f0 90deg 180deg, #d5d0e6 180deg 270deg, #cde0f0 270deg) 0 0 / 20px 20px";

/**
 * Create a pan-zoom viewer inside a container element.
 *
 * The viewer creates stacked canvas elements for each layer, all sharing
 * the same pan/zoom transform. Command layers and custom layers can be
 * freely interleaved. Mouse wheel zooms anchored at the cursor, and
 * mouse drag pans the view.
 *
 * @example
 * ```ts
 * // Basic usage with a multi-layer diagram
 * const viewer = createViewer({
 *   container: document.getElementById('viewer')!,
 *   bounds: diagram.bounds,
 *   layers: punchedLayers(diagram),
 * });
 *
 * // With interleaved custom layer
 * const [substrate, copper] = punchedLayers(diagram);
 * const viewer = createViewer({
 *   container: document.getElementById('viewer')!,
 *   bounds: diagram.bounds,
 *   layers: [
 *     substrate,
 *     { render: (ctx, scale) => { ctx.fillStyle = 'red'; ctx.fillRect(0, 0, 10, 10); } },
 *     copper,
 *   ],
 * });
 * ```
 */
export function createViewer(options: ViewerOptions): Viewer {
  const { container, bounds, padding = 0.9 } = options;
  const mirrorH = options.mirrorH ?? false;
  const mirrorV = options.mirrorV ?? false;

  let layers: ViewerLayer[] = options.layers ?? [];
  const rasters = options.rasters;
  // Held for the current layers, so their bitmaps close when replaced.
  let heldRasters = retainRasters(layers);
  let destroyed = false;
  let rerenderQueued = false;

  function onRasterReady(): void {
    if (rerenderQueued || destroyed) return;
    rerenderQueued = true;
    requestAnimationFrame(() => {
      rerenderQueued = false;
      if (!destroyed) render();
    });
  }

  // Apply background
  if (options.background !== null) {
    container.style.background = options.background ?? DEFAULT_CHECKERBOARD;
  }

  // Ensure container is positioned for absolute canvas children
  const pos = getComputedStyle(container).position;
  if (pos === "static") {
    container.style.position = "relative";
  }
  container.style.overflow = "hidden";
  container.style.cursor = "grab";

  // Diagram bounds center and size
  const dcx = (bounds.minX + bounds.maxX) / 2;
  const dcy = (bounds.minY + bounds.maxY) / 2;
  const dw = bounds.maxX - bounds.minX;
  const dh = bounds.maxY - bounds.minY;

  // View transform state
  let scale = 1;
  let tx = 0;
  let ty = 0;

  // Drag state
  let dragging = false;
  let lastX = 0;
  let lastY = 0;

  // Canvas pool: we create/remove canvases as layers change
  let canvases: HTMLCanvasElement[] = [];

  function ensureCanvasCount(count: number): void {
    while (canvases.length < count) {
      const c = document.createElement("canvas");
      c.style.position = "absolute";
      c.style.top = "0";
      c.style.left = "0";
      c.style.pointerEvents = "none";
      container.appendChild(c);
      canvases.push(c);
    }
    while (canvases.length > count) {
      const c = canvases.pop()!;
      container.removeChild(c);
    }
  }

  function fitToViewport(): void {
    const w = container.clientWidth;
    const h = container.clientHeight;
    scale =
      dw > 0 && dh > 0 ? Math.min((w * padding) / dw, (h * padding) / dh) : 100;
    const sx = mirrorH ? -scale : scale;
    const sy = mirrorV ? scale : -scale;
    tx = w / 2 - dcx * sx;
    ty = h / 2 - dcy * sy;
  }

  function render(): void {
    const pr = window.devicePixelRatio || 1;
    const w = container.clientWidth;
    const h = container.clientHeight;
    const pw = w * pr;
    const ph = h * pr;

    ensureCanvasCount(layers.length);

    for (let i = 0; i < layers.length; i++) {
      const layer = layers[i];
      const c = canvases[i];
      if (layer.visible === false) {
        c.style.display = "none";
        continue;
      }
      c.style.display = "";
      c.width = pw;
      c.height = ph;
      c.style.width = w + "px";
      c.style.height = h + "px";
      const ctx = c.getContext("2d")!;
      ctx.setTransform(pr, 0, 0, pr, 0, 0);
      ctx.clearRect(0, 0, w, h);
      ctx.save();
      const sx = mirrorH ? -scale : scale;
      const sy = mirrorV ? scale : -scale;
      ctx.transform(sx, 0, 0, sy, tx, ty);
      if (hasCommands(layer)) {
        executeCommands(ctx, layer.commands, scale, {
          rasters,
          onRasterReady,
          mask: isMaskLayer(layer),
        });
      } else {
        layer.render(ctx, scale);
      }
      ctx.restore();

      // Tint MaskLayers: replace opaque pixels with the layer color
      if (isMaskLayer(layer)) {
        const [r, g, b, a] = layer.color;
        ctx.save();
        ctx.setTransform(1, 0, 0, 1, 0, 0);
        ctx.globalCompositeOperation = "source-in";
        ctx.fillStyle = `rgba(${r},${g},${b},${a})`;
        ctx.fillRect(0, 0, pw, ph);
        ctx.restore();
      }
    }
  }

  // Zoom on scroll, anchored at mouse position
  function onWheel(e: WheelEvent): void {
    e.preventDefault();
    const rect = container.getBoundingClientRect();
    const mx = e.clientX - rect.left;
    const my = e.clientY - rect.top;
    const factor = e.deltaY < 0 ? 1.15 : 1 / 1.15;
    const newScale = scale * factor;
    tx = mx - (mx - tx) * (newScale / scale);
    ty = my - (my - ty) * (newScale / scale);
    scale = newScale;
    render();
  }

  function onMouseDown(e: MouseEvent): void {
    dragging = true;
    lastX = e.clientX;
    lastY = e.clientY;
    container.style.cursor = "grabbing";
  }

  function onMouseMove(e: MouseEvent): void {
    if (!dragging) return;
    tx += e.clientX - lastX;
    ty += e.clientY - lastY;
    lastX = e.clientX;
    lastY = e.clientY;
    render();
  }

  function onMouseUp(): void {
    dragging = false;
    container.style.cursor = "grab";
  }

  function onResize(): void {
    fitToViewport();
    render();
  }

  // Attach event listeners
  container.addEventListener("wheel", onWheel, { passive: false });
  container.addEventListener("mousedown", onMouseDown);
  window.addEventListener("mousemove", onMouseMove);
  window.addEventListener("mouseup", onMouseUp);
  window.addEventListener("resize", onResize);

  // Initial fit and render
  fitToViewport();
  render();

  return {
    render,
    fitToViewport,

    destroy(): void {
      destroyed = true;
      heldRasters.release();
      container.removeEventListener("wheel", onWheel);
      container.removeEventListener("mousedown", onMouseDown);
      window.removeEventListener("mousemove", onMouseMove);
      window.removeEventListener("mouseup", onMouseUp);
      window.removeEventListener("resize", onResize);
      for (const c of canvases) {
        container.removeChild(c);
      }
      canvases = [];
    },

    setLayers(newLayers: ViewerLayer[]): void {
      // Rescanning every command on a visibility or colour change is costly.
      if (!sameLayerCommands(layers, newLayers)) {
        const previous = heldRasters;
        heldRasters = retainRasters(newLayers);
        previous.release();
      }
      layers = newLayers;
      render();
    },

    getTransform() {
      return { scale, tx, ty, mirrorH, mirrorV };
    },

    setTransform(s: number, x: number, y: number): void {
      scale = s;
      tx = x;
      ty = y;
      render();
    },
  };
}
