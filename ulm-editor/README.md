This editor is a combination of the editor on https://elm-lang.org/try and my earlier explorations, the latest one lives in `../www/src/dev.ts` and `src/ulm-editor.ts` is mostly a copy from that dev.ts split into multiple files.

The editor is used on `../www/dist/editor.html`

If something is no longer working that was working before, compare to the `../www/dist/make.html` variant and the `dev.ts` file.

---

For `../www/dist/editor.html` to work, you first need to build the codemirror files (for now I use the same as the https://elm-lang.org/try editor) into `../www/dist/editor/`, you can build these like this: `cd ../try-editor && ./build.sh dev`

Then you need to build the js parts by running `deno run dev.mjs` (which also starts hot-reload esbuild server).

And then you also need to build the Elm program with `npx elm-watch hot`
