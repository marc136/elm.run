# README

After the experiments in 

1. ../build-wasm-with-cabal/
2. ../elm-js-compiler/
3. ../mdgriffith_elm-dev/ext-ulm/
4. ../mdgriffith_elm-dev/ext-ulm/welmo

I will start again fresh from a clean slate.

I will clone the official Elm compiler and try to keep the file size minimal for now.  
The extensions (except for the `Show` implementations and helpers from Mario) won't be part of the repo for now.

## Steps

- [] Create virtual file system with a simple `elm.json`
- [] Read dependencies from elm.json
- [] Download dependencies
- [] Compile deps (see `verifyApp` fn) to `artifacts.dat` files
- [] Port editor from `welmo` (fork of elm-lang.org/try without server communication)


## TODO

After the first editor release, I want to get back to implementing the full `Ulm.Make.elm` (see commit 6afc00a) and compile multiple user-created Elm files.

## DEV:


cd ~/code/elm/compiler-in-browser-experiment/ulm-wasm/ulm-compiler-wasm
./init.sh
./dev.sh

cd ~/code/elm/compiler-in-browser-experiment/ulm-wasm/ulm-editor
node dev.mjs

cd ~/code/elm/compiler-in-browser-experiment/ulm-wasm/ulm-editor
npx elm-watch hot
