import { afterAll, beforeAll, describe, expect, test } from "bun:test";
import { crc32 } from "node:zlib";
import {
  holdRaster,
  prepareRasters,
  rasterFor,
  retainRasters,
  sameLayerCommands,
  tintMaskPng,
} from "../src/lib/raster.js";
import type { ImageMaskCommand } from "../src/lib/types.js";
import { L_FIXTURE_BASE64 } from "./fixture.js";

interface Chunk {
  type: string;
  data: Uint8Array;
  crc: number;
}

function chunks(png: Uint8Array): Chunk[] {
  const view = new DataView(png.buffer, png.byteOffset, png.byteLength);
  const out: Chunk[] = [];
  for (let off = 8; off < png.length; ) {
    const len = view.getUint32(off);
    out.push({
      type: String.fromCharCode(...png.subarray(off + 4, off + 8)),
      data: png.subarray(off + 8, off + 8 + len),
      crc: view.getUint32(off + 8 + len),
    });
    off += 12 + len;
  }
  return out;
}

function chunk(type: string, data: number[]): Uint8Array {
  const body = Uint8Array.of(...[...type].map((c) => c.charCodeAt(0)), ...data);
  const out = new Uint8Array(4 + body.length + 4);
  const view = new DataView(out.buffer);
  view.setUint32(0, data.length);
  out.set(body, 4);
  view.setUint32(4 + body.length, crc32(body));
  return out;
}

const lPng = Uint8Array.from(atob(L_FIXTURE_BASE64), (c) => c.charCodeAt(0));

/** The fixture with extra chunks spliced in before its IDAT */
function withChunksBeforeIdat(extra: Uint8Array[]): Uint8Array {
  const idat =
    8 +
    chunks(lPng)
      .slice(0, 2)
      .reduce((n, c) => n + 12 + c.data.length, 0);
  const parts = [lPng.subarray(0, idat), ...extra, lPng.subarray(idat)];
  const out = new Uint8Array(parts.reduce((n, p) => n + p.length, 0));
  let pos = 0;
  for (const p of parts) {
    out.set(p, pos);
    pos += p.length;
  }
  return out;
}

describe("tintMaskPng", () => {
  test("re-types the fixture as indexed with a [transparent, colour] palette", () => {
    const out = chunks(tintMaskPng(lPng, 10, 200.4, 300, 0.5));
    expect(out.map((c) => c.type)).toEqual([
      "IHDR",
      "PLTE",
      "tRNS",
      "IDAT",
      "IEND",
    ]);
    const ihdr = out[0].data;
    expect([...ihdr.subarray(8, 13)]).toEqual([1, 3, 0, 0, 0]);
    expect([...out[1].data]).toEqual([0, 0, 0, 10, 200, 255]);
    expect([...out[2].data]).toEqual([0, 128]);
    expect(out[3].data).toEqual(chunks(lPng)[2].data);
    for (const c of out) {
      const body = Uint8Array.of(
        ...[...c.type].map((ch) => ch.charCodeAt(0)),
        ...c.data,
      );
      expect(c.crc).toBe(crc32(body));
    }
  });

  test("drops chunks laid out for greyscale, so there is one PLTE", () => {
    const input = withChunksBeforeIdat([
      chunk("PLTE", [1, 2, 3]),
      chunk("bKGD", [0, 0]),
      chunk("sBIT", [1]),
    ]);
    const types = chunks(tintMaskPng(input, 0, 0, 0, 1)).map((c) => c.type);
    expect(types).toEqual(["IHDR", "PLTE", "tRNS", "IDAT", "IEND"]);
  });
});

// Bun has no image decoder, so count opens and closes with a stand-in.
const realCreateImageBitmap = globalThis.createImageBitmap;
let open = 0;
let closed = 0;

beforeAll(() => {
  globalThis.createImageBitmap = (async () => {
    open++;
    return {
      width: 11,
      height: 7,
      close: () => {
        closed++;
      },
    };
  }) as unknown as typeof createImageBitmap;
});

afterAll(() => {
  globalThis.createImageBitmap = realCreateImageBitmap;
});

function imCmd(payload: string, rgb = 0): ImageMaskCommand {
  return ["IM", rgb, rgb, rgb, 1, 0, 0, 11, 7, payload];
}

// A distinct payload per test, as the cache is page-wide.
let payloadSeq = 0;
function freshPayload(): string {
  payloadSeq++;
  return btoa(String.fromCharCode(...lPng, payloadSeq & 255, payloadSeq >> 8));
}

describe("bitmap lifetime", () => {
  test("releasing the only handle closes the bitmap and a later draw decodes again", async () => {
    const cmd = imCmd(freshPayload());
    const before = { open, closed };
    const cache = await prepareRasters([cmd]);
    expect(cache.lookup(cmd, false)).toBeDefined();
    cache.release();
    cache.release();
    expect(closed - before.closed).toBe(1);
    expect(cache.lookup(cmd, false)).toBeUndefined();
    expect(rasterFor(cmd, false)).toBeUndefined();
    expect(open - before.open).toBe(2);
  });

  test("a bitmap stays open until every holder releases it", async () => {
    const cmd = imCmd(freshPayload());
    const before = closed;
    const a = await prepareRasters([cmd]);
    const b = await prepareRasters([[...cmd] as ImageMaskCommand]);
    const unhold = holdRaster(cmd, false);
    a.release();
    b.release();
    expect(closed - before).toBe(0);
    expect(a.lookup(cmd, false)).toBeDefined();
    unhold();
    expect(closed - before).toBe(1);
  });

  test("a release during decode closes the bitmap when it arrives", async () => {
    const cmd = imCmd(freshPayload());
    const before = closed;
    const handle = retainRasters([cmd]);
    handle.release();
    await handle.settled;
    expect(closed - before).toBe(1);
    expect(handle.lookup(cmd, false)).toBeUndefined();
  });

  test("tint variants are held and closed separately", async () => {
    const payload = freshPayload();
    const black = imCmd(payload, 0);
    const grey = imCmd(payload, 128);
    const before = closed;
    const a = await prepareRasters([black]);
    const b = await prepareRasters([grey]);
    a.release();
    expect(closed - before).toBe(1);
    expect(b.lookup(grey, false)).toBeDefined();
    b.release();
    expect(closed - before).toBe(2);
  });
});

describe("sameLayerCommands", () => {
  // Mask layers hold the tinted variants, so a kind change must swap holds.
  test("a command layer turned mask layer with the same commands differs", () => {
    const commands = [imCmd(L_FIXTURE_BASE64)];
    const color: [number, number, number, number] = [255, 0, 0, 1];
    expect(sameLayerCommands([{ commands }], [{ commands }])).toBe(true);
    expect(sameLayerCommands([{ commands }], [{ color, commands }])).toBe(
      false,
    );
    expect(sameLayerCommands([{ color, commands }], [{ commands }])).toBe(
      false,
    );
  });
});
