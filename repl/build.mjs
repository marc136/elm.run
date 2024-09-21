import * as esbuild from "esbuild";
import { wasmLoader } from "esbuild-plugin-wasm";

await esbuild.build({
  entryPoints: ["src/ulm-repl.ts"],
  outdir: "../www/dist/repl",
  format: "esm",
  outExtension: { ".js": ".mjs" },
  bundle: true,
  splitting: true,
  minify: true,
  sourcemap: "external",
  plugins: [
    wasmLoader({
      // (Default) Deferred mode copies the WASM binary to the output directory,
      // and then `fetch()`s it at runtime. This is the default mode.
      mode: "deferred",
    }),
  ],
});
