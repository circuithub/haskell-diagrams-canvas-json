// Pixel probes of IM drawing in a real browser, run by run.ts. Results are
// written to #results as JSON for the runner to read back.
import { Application, Container, RenderTexture } from "pixi.js";
import { executeCommands } from "../../src/lib/renderer.js";
import { prepareRasters } from "../../src/lib/raster.js";
import {
  createRasterTextures,
  destroyTextureQuietly,
  executeCommandsPixi,
  toMaskCommands,
} from "../../src/lib/renderer-pixi.js";
import type { CanvasCommand, ImageMaskCommand } from "../../src/lib/types.js";
import {
  L_FIXTURE_BASE64,
  L_HEIGHT,
  L_WIDTH,
  LONE_PIXEL_BASE64,
  lIsSet,
} from "../fixture.js";

type Rgba = [number, number, number, number];
type Probe = (x: number, y: number) => Rgba;

interface Result {
  name: string;
  ok: boolean;
  detail?: string;
}

// The L at pitch 1 with its top left at (2, 9), drawn 10 device px per unit
// with Y up, as in the cairo tests.
const lCmd: ImageMaskCommand = [
  "IM",
  0,
  128,
  255,
  1,
  2,
  2,
  2 + L_WIDTH,
  2 + L_HEIGHT,
  L_FIXTURE_BASE64,
];
const lone: ImageMaskCommand = [
  "IM",
  0,
  0,
  0,
  1,
  0,
  0,
  16,
  16,
  LONE_PIXEL_BASE64,
];

const board: CanvasCommand[] = [
  ["B"],
  ["M", 0, 0],
  ["L", 15, 0],
  ["L", 15, 10],
  ["L", 0, 10],
  ["Z"],
  ["F", 255, 0, 0, 1],
];

/** Device pixel at `off` within the 10 px cell of raster pixel (i, j) */
function cell(off: number, i: number, j: number): [number, number] {
  return [20 + 10 * i + off, 10 + 10 * j + off];
}

function near(a: Rgba, b: Rgba): boolean {
  return a.every((v, k) => Math.abs(v - b[k]) <= 2);
}

/** Compare every L cell at the offsets; returns the first mismatch */
function checkL(
  px: Probe,
  offsets: number[],
  expected: (set: boolean) => Rgba,
): string | undefined {
  for (let j = 0; j < L_HEIGHT; j++)
    for (let i = 0; i < L_WIDTH; i++)
      for (const off of offsets) {
        const [x, y] = cell(off, i, j);
        const want = expected(lIsSet(i, j));
        const got = px(x, y);
        if (!near(got, want))
          return `raster (${i},${j}) device (${x},${y}): got ${got} want ${want}`;
      }
  return undefined;
}

function canvas2d(
  w: number,
  h: number,
  m: [number, number, number, number, number, number],
  draw: (ctx: CanvasRenderingContext2D) => void,
): Probe {
  const canvas = document.createElement("canvas");
  canvas.width = w;
  canvas.height = h;
  const ctx = canvas.getContext("2d")!;
  ctx.setTransform(...m);
  draw(ctx);
  const data = ctx.getImageData(0, 0, w, h).data;
  return (x, y) => {
    const o = 4 * (y * w + x);
    return [data[o], data[o + 1], data[o + 2], data[o + 3]];
  };
}

const magnified: [number, number, number, number, number, number] = [
  10, 0, 0, -10, 0, 100,
];
const quarter: [number, number, number, number, number, number] = [
  0.25, 0, 0, -0.25, 0, 4,
];

async function canvasTests(): Promise<Result[]> {
  const halfAlpha = [
    "IM",
    0,
    128,
    255,
    0.5,
    ...lCmd.slice(5),
  ] as ImageMaskCommand;
  const punch: CanvasCommand[] = [...board, ["GCO", "destination-out"], lCmd];
  // Prepared as the viewers do, so the variants chosen here must be the ones
  // drawn: tinted for colour, plain for mask layers and destination-out.
  const rasters = await prepareRasters([
    { commands: [lCmd, lone, halfAlpha, ...punch] },
    { commands: [lCmd], color: [255, 255, 255, 1] },
  ]);
  const results: Result[] = [];
  const check = (name: string, detail: string | undefined) =>
    results.push({ name, ok: detail === undefined, detail });

  check(
    "canvas2d: row 0 at y1, tinted, crisp when magnified",
    checkL(
      canvas2d(150, 100, magnified, (ctx) =>
        executeCommands(ctx, [lCmd], 10, { rasters }),
      ),
      [1, 8],
      (set) => (set ? [0, 128, 255, 255] : [0, 0, 0, 0]),
    ),
  );
  check(
    "canvas2d: alpha 0.5 tint",
    checkL(
      canvas2d(150, 100, magnified, (ctx) =>
        executeCommands(ctx, [halfAlpha], 10, { rasters }),
      ),
      [5],
      (set) => (set ? [0, 128, 255, 128] : [0, 0, 0, 0]),
    ),
  );
  check(
    "canvas2d: mask layers draw the untinted payload",
    checkL(
      canvas2d(150, 100, magnified, (ctx) =>
        executeCommands(ctx, [lCmd], 10, { rasters, mask: true }),
      ),
      [5],
      (set) => (set ? [255, 255, 255, 255] : [0, 0, 0, 0]),
    ),
  );
  check(
    "canvas2d: destination-out erases only the set pixels",
    checkL(
      canvas2d(150, 100, magnified, (ctx) =>
        executeCommands(ctx, punch, 10, { rasters }),
      ),
      [5],
      (set) => (set ? [0, 0, 0, 0] : [255, 0, 0, 255]),
    ),
  );
  const smooth = canvas2d(4, 4, quarter, (ctx) =>
    executeCommands(ctx, [lone], 0.25, { rasters }),
  )(1, 1)[3];
  // Control: nearest sampling at 4x minification misses the pixel.
  const nearest = canvas2d(4, 4, quarter, (ctx) => {
    ctx.transform(1, 0, 0, -1, 0, 16);
    ctx.imageSmoothingEnabled = false;
    ctx.drawImage(rasters.lookup(lone, false)!, 0, 0);
  })(1, 1)[3];
  check(
    "canvas2d: smoothing when minified keeps a lone pixel",
    smooth > 0 && nearest === 0
      ? undefined
      : `alpha ${smooth}, nearest control ${nearest}`,
  );
  rasters.release();
  return results;
}

async function pixiTests(): Promise<Result[]> {
  const app = new Application();
  await app.init({
    width: 150,
    height: 100,
    preference: "webgl",
    backgroundAlpha: 0,
  });
  const rasters = await prepareRasters([lCmd, lone], true);
  const textures = createRasterTextures();

  function render(
    w: number,
    h: number,
    s: number,
    cmds: CanvasCommand[],
  ): Probe {
    const scene = new Container();
    executeCommandsPixi(scene, cmds, s, textures);
    scene.position.set(0, h);
    scene.scale.set(s, -s);
    const target = RenderTexture.create({ width: w, height: h, resolution: 1 });
    app.renderer.render({
      container: scene,
      target,
      clear: true,
      clearColor: [0, 0, 0, 0],
    });
    const { pixels, width } = app.renderer.extract.pixels(target);
    scene.destroy({ children: true });
    target.destroy(true);
    return (x, y) => {
      const o = 4 * (y * width + x);
      return [pixels[o], pixels[o + 1], pixels[o + 2], pixels[o + 3]];
    };
  }

  const results: Result[] = [];
  const check = (name: string, detail: string | undefined) =>
    results.push({ name, ok: detail === undefined, detail });
  check(
    "pixi: row 0 at y1, tinted, crisp when magnified",
    checkL(render(150, 100, 10, [lCmd]), [1, 8], (set) =>
      set ? [0, 128, 255, 255] : [0, 0, 0, 0],
    ),
  );
  check(
    "pixi: mask commands erase only the set pixels",
    checkL(
      render(
        150,
        100,
        10,
        toMaskCommands([...board, ["GCO", "destination-out"], lCmd]),
      ),
      [5],
      (set) => (set ? [0, 0, 0, 0] : [255, 255, 255, 255]),
    ),
  );
  const alpha = render(4, 4, 0.25, [lone])(1, 1)[3];
  check(
    "pixi: mipmaps when minified keep a lone pixel",
    alpha > 0 ? undefined : `alpha ${alpha}`,
  );
  // Viewer rebuilds free layer textures mid-life; the renderer must drop the
  // render target with them, and later renders must still work.
  const targets = (
    app.renderer.renderTarget as unknown as {
      _renderSurfaceToRenderTargetHash: Map<unknown, unknown>;
    }
  )._renderSurfaceToRenderTargetHash;
  const layer = RenderTexture.create({ width: 8, height: 8, resolution: 1 });
  app.renderer.render({ container: new Container(), target: layer });
  const held = targets.size;
  destroyTextureQuietly(layer);
  check(
    "pixi: a quietly destroyed render texture frees its render target",
    targets.size === held - 1
      ? checkL(render(150, 100, 10, [lCmd]), [1, 8], (set) =>
          set ? [0, 128, 255, 255] : [0, 0, 0, 0],
        )
      : `render targets ${held} -> ${targets.size}`,
  );
  textures.destroy();
  rasters.release();
  app.destroy();
  return results;
}

async function main(): Promise<void> {
  const out = document.getElementById("results")!;
  let results: Result[];
  try {
    results = [...(await canvasTests()), ...(await pixiTests())];
  } catch (err) {
    results = [{ name: "harness", ok: false, detail: String(err) }];
  }
  out.textContent = JSON.stringify(results);
  out.dataset.done = "true";
}

void main();
