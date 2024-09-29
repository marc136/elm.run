# README elm.run

This repository contains the source for https://elm.run (and some binary files).

**Word of caution:**
This repo is not cleaned up. It contains a lot of abandoned or postponed explorations that I don't want to delete yet.
If I make enough progress for a sane public release, I will also clean it up properly for the public.

**The relevant directories are:**

1. `./repl/` contains code for https://elm.run/repl
2. `./elm-compiler-wasm/` is the official elm compiler (git submodule)
3. `./ulm-compiler-wasm/` contains my own code to build the Elm compiler as a WASM module. It directly uses the Elm compiler's source code, and only contains a few wasm-specific files. I intend to split the compiler and the repl into two builds to reduce the file size of the repl.wasm (to more easily use it in the tour).
4. `./www/dist/` is deployed to https://elm.run/

The rest are experimentation files that most likely noone else will find useful. There might also be vital files missing because of my .gitignore rules.

Some files might contain references to other experiments, my ulm compiler fork will not be open sourced because I think it makes more sense to invest time into [elm-dev](https://github.com/mdgriffith/elm-dev), [zokka](https://github.com/Zokka-Dev/zokka-compiler/) or [lamdera](https://github.com/lamdera/compiler).

## Running Elm compiler in the browser

In the last weeks, two full rewrites of the Elm compiler in Elm itself have surfaced. So I will maybe discontinue [elm-compiler-wasm](https://github.com/marc136/elm.run/tree/main/ulm-compiler-wasm) and instead switch to one of them for my REPL and editor.

1. https://github.com/pithub/elm-compiler-in-elm-ui
2. https://github.com/guida-lang/compiler
