/**
 * Payloads encoded by the cairo package's MaskPng.encodeMaskPng; its test
 * suite pins these exact strings, so both sides test the same bytes.
 */

/** 11 x 7 px L with a marker at the top right, so a flip or transpose shows */
export const L_FIXTURE_BASE64 =
  "iVBORw0KGgoAAAANSUhEUgAAAAsAAAAHAQAAAAD2FYrwAAAAAnRSTlMAAHaTzTgAAAAWSURBVHicY3jgwPCAgYkBihj+PwCSAC3+A+iNmm1fAAAAAElFTkSuQmCC";

export const L_WIDTH = 11;
export const L_HEIGHT = 7;

/** Pixel (x, y) of the L, with PNG row 0 at the top */
export function lIsSet(x: number, y: number): boolean {
  return x < 3 || y >= 5 || (x === 9 && y === 0);
}

/** 16 x 16 px with only pixel (4, 4) set */
export const LONE_PIXEL_BASE64 =
  "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQAQAAAAA3iMLMAAAAAnRSTlMAAHaTzTgAAAAPSURBVHicY2BAAhwMBAEAAUgACex4Ik8AAAAASUVORK5CYII=";
