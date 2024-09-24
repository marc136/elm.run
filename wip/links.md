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
