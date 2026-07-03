#!/bin/sh

set -e

rm -rf pkg
wasm-pack build --out-name nemu --scope quaqqer --target bundler
