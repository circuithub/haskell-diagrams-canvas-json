import {
  Application,
  Container,
  Graphics,
  GraphicsContext,
  ImageSource,
  Matrix,
  Text,
  Texture,
} from "pixi.js";
import type {
  CanvasDiagram,
  CanvasCommand,
  Color,
  CompositeOp,
  ImageMaskCommand,
} from "./types.js";
import { calculateFitTransform } from "./renderer.js";
import { holdRaster, rasterFor } from "./raster.js";

/** Drawing state tracked manually (Canvas 2D has this built-in) */
interface DrawState {
  fillColor: { r: number; g: number; b: number; a: number };
  strokeColor: { r: number; g: number; b: number; a: number };
  lineWidth: number;
  lineWidthViewRelative: boolean;
  lineCap: "butt" | "round" | "square";
  lineJoin: "miter" | "round" | "bevel";
  font: string;
  blendMode: string;
}

function cloneState(s: DrawState): DrawState {
  return {
    fillColor: { ...s.fillColor },
    strokeColor: { ...s.strokeColor },
    lineWidth: s.lineWidth,
    lineWidthViewRelative: s.lineWidthViewRelative,
    lineCap: s.lineCap,
    lineJoin: s.lineJoin,
    font: s.font,
    blendMode: s.blendMode,
  };
}

function defaultState(): DrawState {
  return {
    fillColor: { r: 0, g: 0, b: 0, a: 1 },
    strokeColor: { r: 0, g: 0, b: 0, a: 1 },
    lineWidth: 1,
    lineWidthViewRelative: false,
    lineCap: "butt",
    lineJoin: "miter",
    font: "10px sans-serif",
    blendMode: "normal",
  };
}

/** Convert RGBA (0-255 for RGB) to a hex number for PixiJS */
export function rgbaToHex(r: number, g: number, b: number): number {
  return (
    ((Math.round(r) & 0xff) << 16) |
    ((Math.round(g) & 0xff) << 8) |
    (Math.round(b) & 0xff)
  );
}

/** Convert line cap number to string */
function lineCapToString(cap: number): "butt" | "round" | "square" {
  switch (cap) {
    case 1:
      return "round";
    case 2:
      return "square";
    default:
      return "butt";
  }
}

/** Convert line join number to string */
function lineJoinToString(join: number): "miter" | "round" | "bevel" {
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
 * Map the {@link CompositeOp} emitted by the Haskell backend to a PixiJS
 * blend mode. `destination-in` is not a PixiJS blend mode; it is handled
 * at the viewer level (see `splitAtDestinationIn` in `viewer-pixi.ts`),
 * so encountering it inside a per-layer command stream falls back to
 * `normal` rendering.
 */
function compositeOpToBlendMode(op: CompositeOp): string {
  switch (op) {
    case "source-over":
      return "normal";
    case "destination-out":
      return "erase";
    case "destination-in":
      return "normal";
  }
}

/**
 * Emit a circular arc as bezier segments onto a GraphicsContext.
 * Splits into segments of at most PI/2, using the standard cubic bezier
 * approximation for each segment.
 */
function emitArc(
  ctx: GraphicsContext,
  cx: number,
  cy: number,
  r: number,
  startAngle: number,
  endAngle: number,
): void {
  const sweep = endAngle - startAngle;
  if (sweep === 0) return;

  const absSweep = Math.abs(sweep);
  const segCount = Math.ceil(absSweep / (Math.PI / 2));
  const segSweep = sweep / segCount;

  ctx.moveTo(cx + r * Math.cos(startAngle), cy + r * Math.sin(startAngle));

  for (let i = 0; i < segCount; i++) {
    const a1 = startAngle + i * segSweep;
    const a2 = a1 + segSweep;
    const alpha = (4 / 3) * Math.tan(segSweep / 4);

    const cos1 = Math.cos(a1);
    const sin1 = Math.sin(a1);
    const cos2 = Math.cos(a2);
    const sin2 = Math.sin(a2);

    ctx.bezierCurveTo(
      cx + r * (cos1 - alpha * sin1),
      cy + r * (sin1 + alpha * cos1),
      cx + r * (cos2 + alpha * sin2),
      cy + r * (sin2 - alpha * cos2),
      cx + r * cos2,
      cy + r * sin2,
      0.99,
    );
  }
}

/**
 * GPU textures for IM payloads, owned by whoever renders with them.
 *
 * Textures hold the untinted payload; the IM colour is applied as the quad
 * tint, so every colour and the mask pass share one texture per payload.
 */
export interface RasterTextures {
  /** The texture for `cmd`, or undefined while its bitmap decodes */
  get(cmd: ImageMaskCommand): Texture | undefined;
  /** Destroy textures not requested by `get` since the previous sweep */
  sweep(): void;
  destroy(): void;
}

/**
 * Destroy a texture without letting its death notify shared PixiJS state.
 *
 * See Note [Viewer teardown is page-local] in viewer-pixi.ts.
 */
export function destroyTextureQuietly(texture: Texture): void {
  const source = texture.source;
  // Unload while still live: "unload" is the only path that deletes the GL
  // texture, and its "change" merely dirties a shared BindGroup.
  source.unload();
  source.style?.removeAllListeners();
  // Keep "destroy": the renderer frees a render texture's framebuffer on it.
  source.removeAllListeners("change");
  texture.destroy(true);
}

/**
 * @param onReady - called when a bitmap requested by `get` finishes decoding,
 *   so the caller can rebuild the scenes that skipped it
 */
export function createRasterTextures(onReady?: () => void): RasterTextures {
  // Each texture holds its bitmap open, which the ImageSource re-uploads from.
  const textures = new Map<string, { texture: Texture; unhold: () => void }>();
  let used = new Set<string>();
  const drop = (t: { texture: Texture; unhold: () => void }) => {
    destroyTextureQuietly(t.texture);
    t.unhold();
  };
  return {
    get(cmd) {
      const payload = cmd[9];
      used.add(payload);
      const existing = textures.get(payload);
      if (existing) return existing.texture;
      const bitmap = rasterFor(cmd, true, onReady);
      if (!bitmap) return undefined;
      // Crisp pixels when magnified; mipmaps (where the GPU supports them for
      // this size) keep small holes visible when minified.
      const texture = new Texture({
        source: new ImageSource({
          resource: bitmap,
          autoGenerateMipmaps: true,
          magFilter: "nearest",
          minFilter: "linear",
          mipmapFilter: "linear",
        }),
      });
      textures.set(payload, { texture, unhold: holdRaster(cmd, true) });
      return texture;
    },
    sweep() {
      for (const [payload, t] of textures) {
        if (!used.has(payload)) {
          drop(t);
          textures.delete(payload);
        }
      }
      used = new Set();
    },
    destroy() {
      for (const t of textures.values()) drop(t);
      textures.clear();
      used = new Set();
    },
  };
}

/**
 * Execute a list of canvas commands, populating a PixiJS Container.
 *
 * Uses a single GraphicsContext per blend-mode group, accumulating all
 * path/fill/stroke operations. This dramatically reduces draw calls compared
 * to creating a new Graphics object per fill/stroke.
 *
 * Transforms are handled via GraphicsContext.save/restore/setTransform.
 * A new Graphics object is only created when the blend mode changes.
 *
 * @param parent - The Container to add display objects to
 * @param commands - The command stream to interpret
 * @param scale - The current transform scale, used for view-relative line widths
 * @param textures - textures for IM commands; without them IM is skipped
 */
export function executeCommandsPixi(
  parent: Container,
  commands: CanvasCommand[],
  scale: number,
  textures?: RasterTextures,
): void {
  let state = defaultState();
  const stateStack: DrawState[] = [];

  // Current GraphicsContext and its Graphics wrapper — one per blend-mode group
  let ctx: GraphicsContext | null = null;

  // Track the current transform so we can re-apply it after flushing
  let currentTransform = new Matrix();
  const transformStack: Matrix[] = [];

  /** Ensure we have a GraphicsContext for the current blend mode. */
  function ensureGfx(): GraphicsContext {
    if (!ctx) {
      ctx = new GraphicsContext();
      const gfx = new Graphics(ctx);
      gfx.blendMode = state.blendMode as never;
      parent.addChild(gfx);
      // Apply the current accumulated transform to the fresh context
      ctx.setTransform(currentTransform);
    }
    return ctx;
  }

  /** Flush the current context and start a new one (on blend mode change). */
  function flushGfx(): void {
    ctx = null;
  }

  /** Return the current context if one exists (helper for TypeScript narrowing). */
  function currentCtx(): GraphicsContext | null {
    return ctx;
  }

  for (const cmd of commands) {
    const opcode = cmd[0];

    switch (opcode) {
      // State management
      case "S": {
        currentCtx()?.save();
        transformStack.push(currentTransform.clone());
        stateStack.push(cloneState(state));
        break;
      }
      case "R": {
        const savedState = stateStack.pop();
        const savedTransform = transformStack.pop();
        if (savedState && savedTransform) {
          if (savedState.blendMode !== state.blendMode) {
            flushGfx();
          } else {
            currentCtx()?.restore();
          }
          state = savedState;
          currentTransform = savedTransform;
        }
        break;
      }

      // Transform
      case "T": {
        const [, a, b, c, d, e, f] = cmd;
        const m = new Matrix(a, b, c, d, e, f);
        currentTransform = currentTransform.append(m);
        currentCtx()?.setTransform(currentTransform);
        break;
      }

      // Path commands
      case "B": {
        const c = ensureGfx();
        c.beginPath();
        break;
      }
      case "M": {
        const c = ensureGfx();
        const [, x, y] = cmd;
        c.moveTo(x, y);
        break;
      }
      case "L": {
        const c = ensureGfx();
        const [, x, y] = cmd;
        c.lineTo(x, y);
        break;
      }
      case "C": {
        const c = ensureGfx();
        const [, cx1, cy1, cx2, cy2, x, y] = cmd;
        c.bezierCurveTo(cx1, cy1, cx2, cy2, x, y, 0.99);
        break;
      }
      case "Q": {
        const c = ensureGfx();
        const [, cx, cy, x, y] = cmd;
        c.quadraticCurveTo(cx, cy, x, y, 0.99);
        break;
      }
      case "A": {
        const c = ensureGfx();
        const [, acx, acy, ar, startAngle, endAngle] = cmd;
        emitArc(c, acx, acy, ar, startAngle, endAngle);
        break;
      }
      case "Z": {
        const c = ensureGfx();
        c.closePath();
        break;
      }

      // Fill with color
      case "F": {
        const c = ensureGfx();
        const [, r, g, b, a] = cmd;
        c.fill({ color: rgbaToHex(r, g, b), alpha: a });
        break;
      }

      // Stroke (coordinate-space line width)
      case "K": {
        const c = ensureGfx();
        const [, r, g, b, a, lineWidth] = cmd;
        c.stroke({
          color: rgbaToHex(r, g, b),
          alpha: a,
          width: lineWidth,
          cap: state.lineCap,
          join: state.lineJoin,
        });
        break;
      }

      // Stroke (view-relative line width)
      case "KV": {
        const c = ensureGfx();
        const [, r, g, b, a, lineWidth] = cmd;
        c.stroke({
          color: rgbaToHex(r, g, b),
          alpha: a,
          width: lineWidth / scale,
          cap: state.lineCap,
          join: state.lineJoin,
        });
        break;
      }

      // Set fill color only
      case "FS": {
        const [, r, g, b, a] = cmd;
        state.fillColor = { r, g, b, a };
        break;
      }

      // Set stroke style (coordinate-space)
      case "KS": {
        const [, r, g, b, a, lineWidth] = cmd;
        state.strokeColor = { r, g, b, a };
        state.lineWidth = lineWidth;
        state.lineWidthViewRelative = false;
        break;
      }

      // Set stroke style (view-relative)
      case "KSV": {
        const [, r, g, b, a, lineWidth] = cmd;
        state.strokeColor = { r, g, b, a };
        state.lineWidth = lineWidth;
        state.lineWidthViewRelative = true;
        break;
      }

      // Fill using current style
      case "f": {
        const c = ensureGfx();
        const { r, g, b, a } = state.fillColor;
        c.fill({ color: rgbaToHex(r, g, b), alpha: a });
        break;
      }

      // Stroke using current style
      case "k": {
        const c = ensureGfx();
        const { r, g, b, a } = state.strokeColor;
        const w = state.lineWidthViewRelative
          ? state.lineWidth / scale
          : state.lineWidth;
        c.stroke({
          color: rgbaToHex(r, g, b),
          alpha: a,
          width: w,
          cap: state.lineCap,
          join: state.lineJoin,
        });
        break;
      }

      // Line style
      case "LC": {
        const [, cap] = cmd;
        state.lineCap = lineCapToString(cap);
        break;
      }
      case "LJ": {
        const [, join] = cmd;
        state.lineJoin = lineJoinToString(join);
        break;
      }
      case "LD":
        // Line dash not supported in PixiJS Graphics
        break;
      case "LDV":
        // Line dash not supported in PixiJS Graphics
        break;

      // Text — requires its own display object
      case "FT": {
        const [, text, x, y] = cmd;
        const t = new Text({ text, style: { fontFamily: state.font } });
        const pt = currentTransform.apply({ x, y });
        t.x = pt.x;
        t.y = pt.y;
        t.scale.y = -1; // Counter the Y-axis flip
        const { r, g, b, a } = state.fillColor;
        t.style.fill = rgbaToHex(r, g, b);
        t.alpha = a;
        t.blendMode = state.blendMode as never;
        parent.addChild(t);
        break;
      }
      case "SF": {
        const [, font] = cmd;
        state.font = font;
        break;
      }

      // Composite operation — flush Graphics, next shapes get new blend mode
      case "GCO": {
        const [, operation] = cmd;
        const newMode = compositeOpToBlendMode(operation);
        if (newMode !== state.blendMode) {
          flushGfx();
          state.blendMode = newMode;
        }
        break;
      }

      // Raster mask — a textured quad in the current blend-mode group
      case "IM": {
        const tex = textures?.get(cmd);
        if (!tex) break;
        const c = ensureGfx();
        const [, r, g, b, a, x0, , x1, y1] = cmd;
        const w = tex.frame.width;
        const h = tex.frame.height;
        const p = (x1 - x0) / w;
        // Flip through the transform: the quad builder drops negative sizes.
        const m = currentTransform
          .clone()
          .append(new Matrix(p, 0, 0, -p, x0, y1));
        c.save();
        c.setTransform(m);
        c.setFillStyle({ alpha: a });
        c.texture(tex, rgbaToHex(r, g, b), 0, 0, w, h);
        c.restore();
        break;
      }

      default:
        console.warn(`Unknown canvas command: ${opcode}`);
    }
  }
}

/** Options for rendering a diagram with PixiJS */
export interface RenderPixiOptions {
  /** Background color (optional) */
  backgroundColor?: Color;
  /** Padding factor for fitting diagram (0-1, default: 0.9 = 10% padding) */
  padding?: number;
  /** Mirror the image horizontally (flip left/right). Default: false. */
  mirrorH?: boolean;
  /** Mirror the image vertically (flip top/bottom). Default: false. */
  mirrorV?: boolean;
  /** Textures for IM commands, owned by the caller; without them IM is skipped */
  textures?: RasterTextures;
}

/**
 * Render a canvas diagram to a PixiJS Application.
 * The Application should already be initialized.
 */
export function renderDiagramPixi(
  app: Application,
  diagram: CanvasDiagram,
  options: RenderPixiOptions = {},
): void {
  const padding = options.padding ?? 0.9;

  // Clear existing stage children
  app.stage.removeChildren();

  // Set background
  if (options.backgroundColor) {
    const { r, g, b, a } = options.backgroundColor;
    app.renderer.background.color = rgbaToHex(r, g, b);
    app.renderer.background.alpha = a;
  }

  // Calculate fit transform
  const transform = calculateFitTransform(
    diagram.width,
    diagram.height,
    diagram.bounds,
    padding,
    options.mirrorH,
    options.mirrorV,
  );

  // Create root container with the fit transform
  const root = new Container();
  root.setFromMatrix(
    new Matrix(
      transform.a,
      transform.b,
      transform.c,
      transform.d,
      transform.e,
      transform.f,
    ),
  );
  app.stage.addChild(root);

  executeCommandsPixi(
    root,
    diagram.commands,
    transform.scale,
    options.textures,
  );
}

/**
 * Transform a command stream so all shapes render as white on transparent.
 *
 * Used by the mask-texture viewer: each layer is rendered white-on-transparent
 * to a RenderTexture, then displayed via a tinted Sprite.
 *
 * - All fill/stroke/IM color commands get their RGBA replaced with white (255,255,255,1)
 * - GCO commands pass through unchanged (PixiJS erase handles destination-out)
 * - All other commands pass through unchanged
 */
export function toMaskCommands(commands: CanvasCommand[]): CanvasCommand[] {
  const result: CanvasCommand[] = [];
  for (const cmd of commands) {
    switch (cmd[0]) {
      case "F":
        result.push(["F", 255, 255, 255, 1]);
        break;
      case "K":
        result.push(["K", 255, 255, 255, 1, cmd[5]]);
        break;
      case "KV":
        result.push(["KV", 255, 255, 255, 1, cmd[5]]);
        break;
      case "FS":
        result.push(["FS", 255, 255, 255, 1]);
        break;
      case "KS":
        result.push(["KS", 255, 255, 255, 1, cmd[5]]);
        break;
      case "KSV":
        result.push(["KSV", 255, 255, 255, 1, cmd[5]]);
        break;
      case "IM":
        result.push([
          "IM",
          255,
          255,
          255,
          1,
          cmd[5],
          cmd[6],
          cmd[7],
          cmd[8],
          cmd[9],
        ]);
        break;
      default:
        result.push(cmd);
    }
  }
  return result;
}

/**
 * Create and initialize a PixiJS Application for rendering diagrams.
 * Convenience function that creates an app sized to the diagram.
 */
export async function createPixiApp(
  diagram: CanvasDiagram,
  options: RenderPixiOptions & { pixelRatio?: number } = {},
): Promise<Application> {
  const pixelRatio = options.pixelRatio ?? window.devicePixelRatio ?? 1;
  const app = new Application();
  await app.init({
    width: diagram.width,
    height: diagram.height,
    resolution: pixelRatio,
    autoDensity: true,
    antialias: true,
    background: options.backgroundColor
      ? rgbaToHex(
          options.backgroundColor.r,
          options.backgroundColor.g,
          options.backgroundColor.b,
        )
      : undefined,
    backgroundAlpha: options.backgroundColor?.a ?? 0,
  });
  return app;
}
