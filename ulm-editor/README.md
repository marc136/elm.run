This editor is a combination of the editor on https://elm-lang.org/try and my earlier explorations, the latest one lives in `../www/src/dev.ts` and `src/ulm-editor.ts` is mostly a copy from that dev.ts split into multiple files.

The editor is used on `../www/dist/editor.html`

If something is no longer working that was working before, compare to the `../www/dist/make.html` variant and the `dev.ts` file.

---

For `../www/dist/editor.html` to work, you first need to build the codemirror files (for now I use the same as the https://elm-lang.org/try editor) into `../www/dist/editor/`, you can build these like this: `cd ../try-editor && ./build.sh dev`

Then you need to build the js parts by running `deno run dev.mjs` (which also starts hot-reload esbuild server).

And then you also need to build the Elm program with `npx elm-watch hot`

---

## DEV

```sh
# keep building the compiler
cd ../ulm-compiler-wasm
./init.sh
./dev.sh

# And start the esbuild server and elm-watch
npm run dev
```

## TODO

Use a web worker for compilation, see https://web.dev/articles/webassembly-performance-patterns-for-web-apps#good_task_runs_in_web_worker_and_loads_and_compiles_only_once

After the first editor release, I want to get back to implementing the full `Ulm.Make.elm` (see commit 6afc00a) and compile multiple user-created Elm files.

---

## Thoughts about the behavior

When editing text, I want to see the amount of problems instantly (throttled/debounced).

But I want to also have the option to turn off this instant verification, because it might be frightening to students.

When editing text, and the program compiles successfully, I don't want to switch to it immediately but rather press a button to show the new program (because I might lose state).
