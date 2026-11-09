import type { CanvasDiagram, CanvasCommand, Color, BBox } from "./types.js";
import { drawImageMask } from "./raster.js";
import type { RasterCache } from "./raster.js";

/** Convert RGBA values (0-255 for RGB, 0-1 for alpha) to CSS string */
function rgbaToCss(r: number, g: number, b: number, a: number): string {
  return `rgba(${Math.round(r)}, ${Math.round(g)}, ${Math.round(b)}, ${a})`;
}

/** Convert line cap number to canvas string */
function lineCapToString(cap: number): CanvasLineCap {
  switch (cap) {
    case 1:
      return "round";
    case 2:
      return "square";
    default:
      return "butt";
  }
}

/** Convert line join number to canvas string */
function lineJoinToString(join: number): CanvasLineJoin {
  switch (join) {
    case 1:
      return "round";
    case 2:
      return "bevel";
    default:
      return "miter";
  }
}

/**
 * Calculate transform to fit diagram bounds into canvas with padding
 * Returns transform parameters and scale factor
 */
export function calculateFitTransform(
  canvasWidth: number,
  canvasHeight: number,
  bounds: BBox,
  padding: number = 0.9,
  mirrorH: boolean = false,
  mirrorV: boolean = false,
): {
  a: number;
  b: number;
  c: number;
  d: number;
  e: number;
  f: number;
  scale: number;
} {
  const { minX, minY, maxX, maxY } = bounds;
  const diagramWidth = maxX - minX;
  const diagramHeight = maxY - minY;

  // Handle empty or zero-size diagrams
  if (diagramWidth <= 0 || diagramHeight <= 0) {
    return {
      a: 100,
      b: 0,
      c: 0,
      d: -100,
      e: canvasWidth / 2,
      f: canvasHeight / 2,
      scale: 100,
    };
  }

  // Calculate scale to fit with padding (preserving aspect ratio)
  const scaleX = (canvasWidth * padding) / diagramWidth;
  const scaleY = (canvasHeight * padding) / diagramHeight;
  const scale = Math.min(scaleX, scaleY);

  // Calculate center of diagram
  const centerX = (minX + maxX) / 2;
  const centerY = (minY + maxY) / 2;

  // Transform: scale, flip Y, optionally mirror, then translate to center
  // Canvas transform matrix: [a, b, c, d, e, f]
  // x' = a*x + c*y + e
  // y' = b*x + d*y + f
  const sx = mirrorH ? -scale : scale;
  const sy = mirrorV ? scale : -scale; // -scale is the default Y-flip
  const a = sx;
  const b = 0;
  const c = 0;
  const d = sy;
  const e = canvasWidth / 2 - centerX * sx;
  const f = canvasHeight / 2 - centerY * sy;

  return { a, b, c, d, e, f, scale };
}

/** Options for {@link executeCommands} */
export interface ExecuteOptions {
  /** Bitmaps from {@link prepareRasters}; missing ones are decoded lazily */
  rasters?: RasterCache;
  /** Called when a lazily decoded IM bitmap becomes ready, to render again */
  onRasterReady?: () => void;
  /** The output is tinted afterwards (mask layer), so IM colours are unused */
  mask?: boolean;
}

/**
 * Execute a single canvas command
 * @param scale - The current transform scale, used for view-relative line widths (KV/KSV commands)
 */
function executeCommand(
  ctx: CanvasRenderingContext2D,
  cmd: CanvasCommand,
  scale: number,
  opts: ExecuteOptions,
): void {
  const opcode = cmd[0];

  switch (opcode) {
    // State management
    case "S":
      ctx.save();
      break;
    case "R":
      ctx.restore();
      break;

    // Transformation
    case "T": {
      const [, a, b, c, d, e, f] = cmd;
      ctx.transform(a, b, c, d, e, f);
      break;
    }

    // Path commands
    case "B":
      ctx.beginPath();
      break;
    case "M": {
      const [, x, y] = cmd;
      ctx.moveTo(x, y);
      break;
    }
    case "L": {
      const [, x, y] = cmd;
      ctx.lineTo(x, y);
      break;
    }
    case "C": {
      const [, cx1, cy1, cx2, cy2, x, y] = cmd;
      ctx.bezierCurveTo(cx1, cy1, cx2, cy2, x, y);
      break;
    }
    case "Q": {
      const [, cx, cy, x, y] = cmd;
      ctx.quadraticCurveTo(cx, cy, x, y);
      break;
    }
    case "A": {
      const [, cx, cy, r, startAngle, endAngle] = cmd;
      ctx.arc(cx, cy, r, startAngle, endAngle);
      break;
    }
    case "Z":
      ctx.closePath();
      break;

    // Fill
    case "F": {
      const [, r, g, b, a] = cmd;
      ctx.fillStyle = rgbaToCss(r, g, b, a);
      ctx.fill();
      break;
    }

    // Stroke (coordinate-space line width — scales with the diagram)
    case "K": {
      const [, r, g, b, a, lineWidth] = cmd;
      ctx.strokeStyle = rgbaToCss(r, g, b, a);
      ctx.lineWidth = lineWidth;
      ctx.stroke();
      break;
    }

    // Stroke (view-relative line width — constant visual width)
    case "KV": {
      const [, r, g, b, a, lineWidth] = cmd;
      ctx.strokeStyle = rgbaToCss(r, g, b, a);
      ctx.lineWidth = lineWidth / scale;
      ctx.stroke();
      break;
    }

    // Set fill color (without filling)
    case "FS": {
      const [, r, g, b, a] = cmd;
      ctx.fillStyle = rgbaToCss(r, g, b, a);
      break;
    }

    // Set stroke color + line width (coordinate-space, without stroking)
    case "KS": {
      const [, r, g, b, a, lineWidth] = cmd;
      ctx.strokeStyle = rgbaToCss(r, g, b, a);
      ctx.lineWidth = lineWidth;
      break;
    }

    // Set stroke color + line width (view-relative, without stroking)
    case "KSV": {
      const [, r, g, b, a, lineWidth] = cmd;
      ctx.strokeStyle = rgbaToCss(r, g, b, a);
      ctx.lineWidth = lineWidth / scale;
      break;
    }

    // Fill using current fillStyle
    case "f":
      ctx.fill();
      break;

    // Stroke using current strokeStyle/lineWidth
    case "k":
      ctx.stroke();
      break;

    // Line style
    case "LC": {
      const [, cap] = cmd;
      ctx.lineCap = lineCapToString(cap);
      break;
    }
    case "LJ": {
      const [, join] = cmd;
      ctx.lineJoin = lineJoinToString(join);
      break;
    }
    // Line dash (coordinate-space, scales with diagram)
    case "LD": {
      const dashes = cmd.slice(1) as number[];
      ctx.setLineDash(dashes);
      break;
    }
    // Line dash (view-relative, constant visual size)
    case "LDV": {
      const dashes = cmd.slice(1) as number[];
      ctx.setLineDash(dashes.map((d) => d / scale));
      break;
    }

    // Text
    case "FT": {
      const [, text, x, y] = cmd;
      ctx.fillText(text, x, y);
      break;
    }
    case "SF": {
      const [, font] = cmd;
      ctx.font = font;
      break;
    }

    // Canvas state
    case "GCO": {
      const [, operation] = cmd;
      ctx.globalCompositeOperation = operation as GlobalCompositeOperation;
      break;
    }

    case "IM":
      drawImageMask(ctx, cmd, opts);
      break;

    default:
      console.warn(`Unknown canvas command: ${opcode}`);
  }
}

/**
 * Execute a list of canvas commands
 * @param scale - The current transform scale, used for view-relative line widths (KV/KSV commands)
 */
export function executeCommands(
  ctx: CanvasRenderingContext2D,
  commands: CanvasCommand[],
  scale: number,
  opts: ExecuteOptions = {},
): void {
  for (const cmd of commands) {
    executeCommand(ctx, cmd, scale, opts);
  }
}

/** Options for rendering a diagram */
export interface RenderOptions {
  /** Whether to clear the canvas before rendering (default: true) */
  clear?: boolean;
  /** Background color to fill before rendering (optional) */
  backgroundColor?: Color;
  /** Scale factor for high-DPI displays (default: window.devicePixelRatio) */
  pixelRatio?: number;
  /** Padding factor for fitting diagram (0-1, default: 0.9 = 10% padding) */
  padding?: number;
  /** Mirror the image horizontally (flip left/right). Default: false. */
  mirrorH?: boolean;
  /** Mirror the image vertically (flip top/bottom). Default: false. */
  mirrorV?: boolean;
  /**
   * Bitmaps from {@link prepareRasters}. Without them IM commands draw on a
   * later frame, once decoded, by rendering the whole canvas again; pass
   * them when drawing over the result.
   */
  rasters?: RasterCache;
}

// The latest render of each canvas, so a late redraw cannot replace it.
const latestRender = new WeakMap<HTMLCanvasElement, object>();

/**
 * Render a canvas diagram (command-based format) to an HTML canvas element
 */
export function renderDiagram(
  canvas: HTMLCanvasElement,
  diagram: CanvasDiagram,
  options: RenderOptions = {},
): void {
  const ctx = canvas.getContext("2d");
  if (!ctx) {
    throw new Error("Could not get 2D rendering context from canvas");
  }

  const pixelRatio = options.pixelRatio ?? window.devicePixelRatio ?? 1;
  const padding = options.padding ?? 0.9;

  // Set canvas size accounting for pixel ratio
  canvas.width = diagram.width * pixelRatio;
  canvas.height = diagram.height * pixelRatio;
  canvas.style.width = `${diagram.width}px`;
  canvas.style.height = `${diagram.height}px`;

  // Scale context for pixel ratio
  ctx.scale(pixelRatio, pixelRatio);

  // Clear canvas if requested
  if (options.clear !== false) {
    ctx.clearRect(0, 0, diagram.width, diagram.height);
  }

  // Fill background if specified
  if (options.backgroundColor) {
    const { r, g, b, a } = options.backgroundColor;
    ctx.fillStyle = rgbaToCss(r, g, b, a);
    ctx.fillRect(0, 0, diagram.width, diagram.height);
  }

  // Apply transform to fit diagram to canvas
  const transform = calculateFitTransform(
    diagram.width,
    diagram.height,
    diagram.bounds,
    padding,
    options.mirrorH,
    options.mirrorV,
  );
  ctx.save();
  ctx.transform(
    transform.a,
    transform.b,
    transform.c,
    transform.d,
    transform.e,
    transform.f,
  );

  const token = {};
  latestRender.set(canvas, token);
  let redrawQueued = false;
  executeCommands(ctx, diagram.commands, transform.scale, {
    rasters: options.rasters,
    onRasterReady: () => {
      if (redrawQueued) return;
      redrawQueued = true;
      requestAnimationFrame(() => {
        if (latestRender.get(canvas) === token)
          renderDiagram(canvas, diagram, options);
      });
    },
  });

  ctx.restore();
}
