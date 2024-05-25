Forked from https://github.com/mdgriffith/elm-dev

Get the GHC compiler with wasm backend. I used

```sh
nix shell https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/48ecacd922638487f65981c0018044b55dc7feb9/ghc-wasm-meta-master.tar.gz
```

Then run `./build-wasm.sh` to build the Elm compiler as a wasm file.
