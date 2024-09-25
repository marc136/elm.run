COMMIT=a04cc1a2206d2030326e1d49be9c6a94ee4283a3
nix shell https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/$COMMIT/ghc-wasm-meta-master.tar.gz --extra-experimental-features nix-command --extra-experimental-features flakes
