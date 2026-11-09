import type {
  CanvasCommand,
  CanvasDiagram,
  ImageMaskCommand,
} from "./types.js";
import type { LayeredDiagram, ViewerLayer } from "./viewer.js";

/** Anything whose IM commands {@link prepareRasters} can decode up front */
export type RasterSource =
  | CanvasCommand[]
  | CanvasDiagram
  | LayeredDiagram
  | ViewerLayer[];

/**
 * A hold on decoded IM bitmaps, which are shared page-wide by payload.
 *
 * A bitmap is closed once every handle holding it is released, which frees
 * its memory without waiting for garbage collection. Bitmaps no handle ever
 * held (lazy decodes) are left to the garbage collector.
 */
export interface RasterCache {
  /**
   * The bitmap for `cmd`. `plain` selects the untinted payload (opaque white
   * where set), usable whenever the IM colour does not reach the output.
   */
  lookup(cmd: ImageMaskCommand, plain: boolean): ImageBitmap | undefined;
  /**
   * Drop this handle's hold. Later draws of a closed bitmap decode it again.
   * Calling it twice is harmless.
   */
  release(): void;
}

interface Decode {
  readonly entry: PayloadEntry;
  readonly key: string;
  bitmap: ImageBitmap | null;
  failed: boolean;
  promise: Promise<void>;
  waiters: Set<() => void>;
  /** Live handles holding this decode */
  refs: number;
  closed: boolean;
}

interface PayloadEntry {
  readonly payload: string;
  /** "plain" or the tint key */
  readonly variants: Map<string, Decode>;
}

// Entries live as long as some command that uses them; the payload map only
// finds an entry again for copied commands (toMaskCommands, layer rebuilds).
const byCommand = new WeakMap<CanvasCommand, PayloadEntry>();
const byPayload = new Map<string, WeakRef<PayloadEntry>>();
const payloadRegistry = new FinalizationRegistry<string>((payload) => {
  const ref = byPayload.get(payload);
  if (ref && ref.deref() === undefined) byPayload.delete(payload);
});

function entryFor(
  cmd: ImageMaskCommand,
  create: boolean,
): PayloadEntry | undefined {
  let entry = byCommand.get(cmd);
  if (entry) return entry;
  const payload = cmd[9];
  entry = byPayload.get(payload)?.deref();
  if (!entry) {
    if (!create) return undefined;
    entry = { payload, variants: new Map() };
    byPayload.set(payload, new WeakRef(entry));
    payloadRegistry.register(entry, payload);
  }
  byCommand.set(cmd, entry);
  return entry;
}

/** Tint key for `cmd`, or "plain" when the untinted payload already is its colour */
function variantKey(cmd: ImageMaskCommand, plain: boolean): string {
  const [, r, g, b, a] = cmd;
  const r8 = Math.round(r);
  const g8 = Math.round(g);
  const b8 = Math.round(b);
  const a8 = Math.round(a * 255);
  if (plain || (r8 === 255 && g8 === 255 && b8 === 255 && a8 === 255))
    return "plain";
  return `${r8},${g8},${b8},${a8}`;
}

/**
 * Whether drawing `cmd` can use the untinted payload: the colour is discarded
 * by a later source-in tint (`mask`) or by a destination-* operation. Alpha
 * still matters in both, so only opaque IMs qualify.
 */
export function colourIsIrrelevant(
  cmd: ImageMaskCommand,
  mask: boolean,
  gco: string,
): boolean {
  return cmd[4] >= 1 && (mask || gco !== "source-over");
}

function decodeBase64(text: string): Uint8Array {
  const fromBase64 = (
    Uint8Array as unknown as { fromBase64?: (s: string) => Uint8Array }
  ).fromBase64;
  if (fromBase64) return fromBase64(text);
  const bin = atob(text);
  const out = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
  return out;
}

const crcTable = (() => {
  const t = new Uint32Array(256);
  for (let n = 0; n < 256; n++) {
    let c = n;
    for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1;
    t[n] = c >>> 0;
  }
  return t;
})();

function crc32(bytes: Uint8Array, start: number, end: number): number {
  let c = 0xffffffff;
  for (let i = start; i < end; i++)
    c = crcTable[(c ^ bytes[i]) & 0xff] ^ (c >>> 8);
  return (c ^ 0xffffffff) >>> 0;
}

function chunk(type: string, data: Uint8Array): Uint8Array {
  const out = new Uint8Array(12 + data.length);
  const view = new DataView(out.buffer);
  view.setUint32(0, data.length);
  for (let i = 0; i < 4; i++) out[4 + i] = type.charCodeAt(i);
  out.set(data, 8);
  view.setUint32(8 + data.length, crc32(out, 4, 8 + data.length));
  return out;
}

/** Chunks whose layout depends on the colour type, replaced or dropped by the tint */
const colourTypeChunks = new Set(["tRNS", "PLTE", "bKGD", "sBIT", "hIST"]);

/**
 * Re-type a 1-bit grey+tRNS PNG as 1-bit indexed with palette
 * [transparent, rgba]. The IDAT bits mean the same in both, so colouring
 * needs no pixel work or scratch canvas. Chunks laid out for greyscale are
 * dropped.
 */
export function tintMaskPng(
  png: Uint8Array,
  r: number,
  g: number,
  b: number,
  a: number,
): Uint8Array {
  const view = new DataView(png.buffer, png.byteOffset, png.byteLength);
  const parts: Uint8Array[] = [png.subarray(0, 8)];
  let inserted = false;
  for (let off = 8; off + 12 <= png.length; ) {
    const len = view.getUint32(off);
    const end = off + 12 + len;
    const type = String.fromCharCode(...png.subarray(off + 4, off + 8));
    if (type === "IHDR") {
      const data = png.slice(off + 8, off + 8 + len);
      data[9] = 3;
      parts.push(chunk("IHDR", data));
    } else if (colourTypeChunks.has(type)) {
      // Laid out for greyscale, and a second PLTE is a fatal decode error.
    } else if (type === "IDAT" && !inserted) {
      // PLTE and tRNS must precede IDAT and follow any colour-space chunks.
      const clamp = (v: number) => Math.max(0, Math.min(255, Math.round(v)));
      parts.push(
        chunk("PLTE", Uint8Array.of(0, 0, 0, clamp(r), clamp(g), clamp(b))),
      );
      parts.push(chunk("tRNS", Uint8Array.of(0, clamp(a * 255))));
      parts.push(png.subarray(off, end));
      inserted = true;
    } else {
      parts.push(png.subarray(off, end));
    }
    off = end;
  }
  const out = new Uint8Array(parts.reduce((n, p) => n + p.length, 0));
  let pos = 0;
  for (const p of parts) {
    out.set(p, pos);
    pos += p.length;
  }
  return out;
}

const warned = new Set<string>();

function startDecode(
  entry: PayloadEntry,
  key: string,
  cmd: ImageMaskCommand,
): Decode {
  const existing = entry.variants.get(key);
  if (existing) return existing;
  const decode: Decode = {
    entry,
    key,
    bitmap: null,
    failed: false,
    promise: Promise.resolve(),
    waiters: new Set(),
    refs: 0,
    closed: false,
  };
  decode.promise = (async () => {
    try {
      let bytes = decodeBase64(entry.payload);
      if (key !== "plain") {
        const [, r, g, b, a] = cmd;
        bytes = tintMaskPng(bytes, r, g, b, a);
      }
      const bitmap = await createImageBitmap(
        new Blob([bytes as Uint8Array<ArrayBuffer>], { type: "image/png" }),
        { premultiplyAlpha: "premultiply", colorSpaceConversion: "none" },
      );
      if (decode.closed) bitmap.close();
      else decode.bitmap = bitmap;
    } catch (err) {
      decode.failed = true;
      const msg = err instanceof Error ? err.message : String(err);
      if (!warned.has(msg)) {
        warned.add(msg);
        console.warn(`IM payload failed to decode: ${msg}`);
      }
    }
    const waiters = [...decode.waiters];
    decode.waiters.clear();
    for (const w of waiters) w();
  })();
  entry.variants.set(key, decode);
  return decode;
}

function hold(decode: Decode): void {
  decode.refs++;
}

function unhold(decode: Decode): void {
  if (--decode.refs > 0) return;
  decode.closed = true;
  decode.bitmap?.close();
  decode.bitmap = null;
  if (decode.entry.variants.get(decode.key) === decode)
    decode.entry.variants.delete(decode.key);
}

/**
 * Hold the decode of `cmd` until the returned function is called, so its
 * bitmap stays open while something such as a GPU texture refers to it.
 */
export function holdRaster(cmd: ImageMaskCommand, plain: boolean): () => void {
  const decode = startDecode(entryFor(cmd, true)!, variantKey(cmd, plain), cmd);
  hold(decode);
  let held = true;
  return () => {
    if (held) unhold(decode);
    held = false;
  };
}

/**
 * The bitmap for `cmd`, starting its decode when missing. `onReady` runs
 * once that decode settles, so a synchronous renderer can draw again.
 */
export function rasterFor(
  cmd: ImageMaskCommand,
  plain: boolean,
  onReady?: () => void,
): ImageBitmap | undefined {
  const entry = entryFor(cmd, true)!;
  const decode = startDecode(entry, variantKey(cmd, plain), cmd);
  if (decode.bitmap) return decode.bitmap;
  if (!decode.failed && onReady) decode.waiters.add(onReady);
  return undefined;
}

function lookup(
  cmd: ImageMaskCommand,
  plain: boolean,
): ImageBitmap | undefined {
  return (
    entryFor(cmd, false)?.variants.get(variantKey(cmd, plain))?.bitmap ??
    undefined
  );
}

function collect(
  commands: CanvasCommand[],
  mask: boolean,
  plainOnly: boolean,
  retain: Set<Decode>,
): void {
  const gcoStack: string[] = [];
  let gco = "source-over";
  for (const cmd of commands) {
    switch (cmd[0]) {
      case "S":
        gcoStack.push(gco);
        break;
      case "R":
        gco = gcoStack.pop() ?? gco;
        break;
      case "GCO":
        gco = cmd[1];
        break;
      case "IM": {
        const plain = plainOnly || colourIsIrrelevant(cmd, mask, gco);
        const entry = entryFor(cmd, true)!;
        retain.add(startDecode(entry, variantKey(cmd, plain), cmd));
        break;
      }
    }
  }
}

/**
 * Start decoding every IM payload in `source` and hold the results until the
 * handle is released; {@link prepareRasters} also waits for them.
 */
export function retainRasters(
  source: RasterSource,
  plainOnly = false,
): RasterCache & { settled: Promise<void> } {
  const retain = new Set<Decode>();
  if (Array.isArray(source)) {
    if (source.length > 0 && !Array.isArray(source[0])) {
      for (const layer of source as ViewerLayer[])
        if ("commands" in layer)
          collect(layer.commands, "color" in layer, plainOnly, retain);
    } else {
      collect(source as CanvasCommand[], false, plainOnly, retain);
    }
  } else if ("layers" in source) {
    for (const layer of source.layers)
      collect(layer.commands, true, plainOnly, retain);
    if (source.punch) collect(source.punch, true, plainOnly, retain);
  } else {
    collect(source.commands, false, plainOnly, retain);
  }
  for (const decode of retain) hold(decode);
  let held = true;
  return {
    lookup,
    release() {
      if (held) for (const decode of retain) unhold(decode);
      held = false;
    },
    settled: Promise.all([...retain].map((d) => d.promise)).then(() => {}),
  };
}

/**
 * Whether two layer lists draw the same command arrays as the same kind of
 * layer, so their holds match: mask layers hold the tinted variants.
 */
export function sameLayerCommands(a: ViewerLayer[], b: ViewerLayer[]): boolean {
  return (
    a.length === b.length &&
    a.every((l, i) => {
      const m = b[i];
      return "commands" in l
        ? "commands" in m &&
            l.commands === m.commands &&
            "color" in l === "color" in m
        : !("commands" in m);
    })
  );
}

/**
 * Decode every IM payload in `source` once, ahead of a synchronous render.
 * A layered diagram's layers and punch are treated as mask layers, whose
 * colour comes from the tint. Decode failures are logged, not thrown.
 * Release the handle once nothing draws with it, so the bitmaps are closed.
 *
 * @param plainOnly - decode only untinted payloads (the PixiJS renderer
 *   tints on the GPU)
 */
export async function prepareRasters(
  source: RasterSource,
  plainOnly = false,
): Promise<RasterCache> {
  const { lookup, release, settled } = retainRasters(source, plainOnly);
  await settled;
  return { lookup, release };
}

/**
 * Draw an IM command: its W x H bitmap mapped onto [x0,x1] x [y0,y1] with PNG
 * row 0 at y1, under the current transform and composite operation.
 * Returns false when the bitmap is still decoding (`onRasterReady` fires
 * once it is ready) or failed to decode.
 *
 * @param mask - the output is tinted afterwards, so the IM colour is unused
 */
export function drawImageMask(
  ctx: CanvasRenderingContext2D,
  cmd: ImageMaskCommand,
  opts: {
    mask?: boolean;
    rasters?: RasterCache;
    onRasterReady?: () => void;
  } = {},
): boolean {
  const plain = colourIsIrrelevant(
    cmd,
    opts.mask ?? false,
    ctx.globalCompositeOperation,
  );
  const bmp =
    opts.rasters?.lookup(cmd, plain) ??
    rasterFor(cmd, plain, opts.onRasterReady);
  if (!bmp) return false;
  const [, , , , , x0, , x1, y1] = cmd;
  const p = (x1 - x0) / bmp.width;
  const m = ctx.getTransform();
  // Device px per raster px: crisp squares when magnified; smoothing when
  // minified keeps sub-pixel holes from aliasing away.
  const k = Math.sqrt(Math.abs(m.a * m.d - m.b * m.c)) * p;
  ctx.save();
  ctx.transform(p, 0, 0, -p, x0, y1);
  ctx.imageSmoothingEnabled = k < 1;
  ctx.imageSmoothingQuality = "high";
  ctx.drawImage(bmp, 0, 0);
  ctx.restore();
  return true;
}

/** {@link prepareRasters} for the PixiJS renderer, which only needs untinted payloads */
export function preparePixiRasters(source: RasterSource): Promise<RasterCache> {
  return prepareRasters(source, true);
}
