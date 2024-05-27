Forked from https://github.com/mdgriffith/elm-dev

Get the GHC compiler with wasm backend. I used

```sh
nix shell https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/48ecacd922638487f65981c0018044b55dc7feb9/ghc-wasm-meta-master.tar.gz
```

Then run `./build-wasm.sh` to build the Elm compiler as a wasm file.

# TODO

Introduce a state monad to read the dependcy artifacts only once.

Handle installation of new packages.

Using `Reporting.Doc` to turn the errors into human-readable formatted JSON adds 2MiB to the wasm bundle size. I want to instead return a direct representation of the actual error, and then transform it in the viewer to the same or similar readable data.  
This will also allow to show the errors in [different languages](https://github.com/katjam/local-elm).
