# Ulm Compiler Wasm

This directory contains Haskell code to compile the Elm compiler to WebAssembly.\
It is not the actual Ulm compiler, just shares the name. It contains no extra features on top of the Elm compiler.

## Development

It requires the submodule `../elm-compiler-wasm` which is a fork of https://github.com/elm/compiler with the most minimal changes required to build for wams32-wasi.

Get the [GHC compiler with wasm backend](https://gitlab.haskell.org/ghc/ghc-wasm-meta). I used nix for that, see the `./init.sh` file for the current revision.

Then run `./build-wasm.sh` to build the `ulm.wasm` and `repl.wasm` used by other projects in this repository.

# TODO

Introduce a state monad to read the dependency artifacts only once.

Use a web worker for compilation, see https://web.dev/articles/webassembly-performance-patterns-for-web-apps#good_task_runs_in_web_worker_and_loads_and_compiles_only_once

Add a replacement for elm-compiler/builder/src/Stuff.hs `withRegistryLock`.
The missing lock might be the reason why installing many dependencies at once can fail.
See my experiments inside Ulm.Details with `acquireLock`.

Handle installation of new packages.

Using `Reporting.Doc` to turn the errors into human-readable formatted JSON adds 2MiB to the wasm bundle size. I want to instead return a direct representation of the actual error, and then transform it in the viewer to the same or similar readable data.
This will also allow to show the errors in [different languages](https://github.com/katjam/local-elm).
