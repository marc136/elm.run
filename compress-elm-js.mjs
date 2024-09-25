import * as swc from "@swc/core";
import * as fs from "node:fs/promises";

/* Source: https://github.com/lydell/elm-minification-benchmarks */

const pureFuncs = [ "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"]; // prettier-ignore

async function minify(code) {
  return (
    await swc.minify(code, {
      compress: {
        pure_funcs: pureFuncs,
        pure_getters: true,
        unsafe_comps: true,
        unsafe: true,
      },
      mangle: {
        reserved: pureFuncs,
      },
    })
  ).code;
}

for (const arg of process.argv.slice(2)) {
  console.log("Compressing", arg);
  const before = await fs.readFile(arg, { encoding: "utf8" });
  const after = await minify(before);
  const diff = (after.length / before.length) * 100;
  await fs.writeFile(arg, after, { encoding: "utf8" });
  console.log(
    `Compressed ${arg} to ${diff.toFixed(1)}% (${(after.length / 1000).toFixed(2)}K)`,
  );
}
