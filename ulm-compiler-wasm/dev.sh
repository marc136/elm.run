while true; do
  ./build-wasm.sh
  inotifywait --quiet --recursive -e modify .
done
