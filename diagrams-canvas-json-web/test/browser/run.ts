// Bundle main.ts, load it in headless Chrome and report its pixel probes.
// Chrome is `google-chrome` on PATH unless CHROME names another binary; WebGL
// runs on SwiftShader, so no GPU is needed.
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { build } from "vite";

const dir = mkdtempSync(join(tmpdir(), "im-browser-"));
try {
  await build({
    configFile: false,
    logLevel: "warn",
    build: {
      lib: {
        entry: resolve(import.meta.dir, "main.ts"),
        name: "ImBrowserTest",
        formats: ["iife"],
        fileName: () => "test.js",
      },
      outDir: dir,
      emptyOutDir: false,
    },
  });
  writeFileSync(
    join(dir, "index.html"),
    '<!doctype html><pre id="results"></pre><script src="test.js"></script>',
  );
  const proc = Bun.spawnSync(
    [
      process.env.CHROME ?? "google-chrome",
      "--headless=new",
      "--no-first-run",
      `--user-data-dir=${join(dir, "profile")}`,
      "--use-angle=swiftshader",
      "--enable-unsafe-swiftshader",
      "--virtual-time-budget=30000",
      "--dump-dom",
      `file://${join(dir, "index.html")}`,
    ],
    { stderr: "pipe" },
  );
  const dom = proc.stdout.toString();
  const match = /<pre id="results" data-done="true">([^<]*)<\/pre>/.exec(dom);
  if (!match) {
    console.error(proc.stderr.toString());
    console.error(dom);
    throw new Error("the browser test page did not finish");
  }
  const text = match[1].replace(/&quot;/g, '"').replace(/&amp;/g, "&");
  const results = JSON.parse(text) as {
    name: string;
    ok: boolean;
    detail?: string;
  }[];
  for (const r of results)
    console.log(
      `${r.ok ? "ok  " : "FAIL"} ${r.name}${r.detail ? `: ${r.detail}` : ""}`,
    );
  const failed = results.filter((r) => !r.ok).length;
  console.log(`${results.length - failed} passed, ${failed} failed`);
  process.exitCode = failed > 0 ? 1 : 0;
} finally {
  rmSync(dir, { recursive: true, force: true });
}
