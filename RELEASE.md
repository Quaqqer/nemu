# Creating a package release

Before publish a release, remember to update CHANGELOG if it exists and create
tags.

## nemu-emulator

```sh
cargo publish
```

## nemu-gui

```sh
cargo publish
```

## nemu-wasm

```
./package.sh
cd pkg
npm publish
```
