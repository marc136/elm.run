#!/bin/bash

esbuild src/dev.ts --bundle --format=esm --outfile=dist/dev.mjs --watch --sourcemap --servedir=dist
