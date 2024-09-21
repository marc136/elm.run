#!/usr/bin/env bash
# From
# https://github.com/tweag/ormolu/blob/607978708809d97945c6036a60b8ffb9b719bc60/ormolu-live/build-wasm.sh

set -e

# I used a specific commit of ghc-wasm-meta because the latest master of 2024-04-29 did not work for me
#nix shell https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/48ecacd922638487f65981c0018044b55dc7feb9/ghc-wasm-meta-master.tar.gz

# build ulm.wasm
wasm32-wasi-cabal build exe:repl
DIR=../www/generated

cp "$(wasm32-wasi-cabal list-bin exe:repl)" "$DIR/repl.wasm"
cp "$DIR/repl.wasm" ../www/dist/ # Copied there until I make esbuild load the wasm file
tar -czvf "$DIR/repl.tar.gz" "$DIR/repl.wasm"
# Generate JS wrapper
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i "$DIR/repl.wasm" -o "$DIR/repl.js"

#echo "NOTE: Disabled wasm compression"; exit 0
tree -h "$DIR"
du -h ../www/generated/*.{wasm,tar.gz}
du -h "$DIR/*.{wasm,tar.gz}"

# "-Oz" -> optimize for file size
wasm-opt -Oz "$DIR/repl.wasm" -o "$DIR/repl.opt.wasm"
# For more optimization, look how wizer is used in
# https://github.com/tweag/ormolu/blob/607978708809d97945c6036a60b8ffb9b719bc60/ormolu-live/cbits/init.c
tar -czvf "$DIR/repl.opt.tar.gz" "$DIR/repl.opt.wasm"

du -h "$DIR/*.{wasm,tar.gz}"
