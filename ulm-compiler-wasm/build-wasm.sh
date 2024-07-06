#!/usr/bin/env bash
# From
# https://github.com/tweag/ormolu/blob/607978708809d97945c6036a60b8ffb9b719bc60/ormolu-live/build-wasm.sh

set -e

# I used a specific commit of ghc-wasm-meta because the latest master of 2024-04-29 did not work for me
# nix shell https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/48ecacd922638487f65981c0018044b55dc7feb9/ghc-wasm-meta-master.tar.gz

wasm32-wasi-cabal build exe:ulm
wasm32-wasi-cabal list-bin exe:ulm
BUILT="$(wasm32-wasi-cabal list-bin exe:ulm)"
DIR=../www/dist

cp "$BUILT" "$DIR/ulm.wasm"
tar -czvf "$DIR/ulm.tar.gz" "$DIR/ulm.wasm"
# Generate JS wrapper
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i "$DIR/ulm.wasm" -o "$DIR/ulm.js"

echo "NOTE: Disabled wasm compression"
du -h ../www/dist/ulm.{wasm,tar.gz}
exit 0

# "-Oz" -> optimize for file size
wasm-opt -Oz "$DIR/ulm.wasm" -o "$DIR/ulm.opt.wasm"
# For more optimiztion, look how wizer is used in
# https://github.com/tweag/ormolu/blob/607978708809d97945c6036a60b8ffb9b719bc60/ormolu-live/cbits/init.c
tar -czvf "$DIR/ulm.opt.tar.gz" "$DIR/ulm.opt.wasm"

tree -h "$DIR"

echo "NOTE: Disabled optimizer pass with wizer because I did not set it up properly"
exit 0

# TODO needs `wizer.initialize` exposed, see
# https://github.com/tweag/ormolu/blob/607978708809d97945c6036a60b8ffb9b719bc60/ormolu-live/cbits/init.c
wizer \
    --allow-wasi --wasm-bulk-memory true \
    "$BUILT" -o "$DIR/ulm.wizer.wasm" 
wasm-opt "-Oz" "$DIR/ulm.wizer.wasm" -o "$DIR/ulm.wizer.opt.wasm"
tar -czvf "$DIR/ulm.wizer.opt.tar.gz" "$DIR/ulm.wizer.opt.wasm"
tree -h "$DIR"
