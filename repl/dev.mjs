import * as esbuild from "esbuild";
import { wasmLoader } from "esbuild-plugin-wasm";

let ctx = await esbuild.context({
  entryPoints: ["src/ulm-repl.ts"],
  outdir: "../www/dist/repl",
  format: "esm",
  outExtension: { ".js": ".mjs" },
  bundle: true,
  splitting: true,
  sourcemap: "external",
  plugins: [
    wasmLoader({
      // (Default) Deferred mode copies the WASM binary to the output directory,
      // and then `fetch()`s it at runtime. This is the default mode.
      mode: "deferred",
    }),
  ],
});

await ctx.watch();
const { host, port } = await ctx.serve({
  servedir: "../www/dist",
  port: 1212,
});
console.log(`Serving on: http://${host}:${port}/`);
