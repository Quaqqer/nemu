# A libretro core for Nemu

## Build

Simply run `cargo build --release` to build the core. The output will be a
dynamic library, in Windows a `.dll`, Linux a `.so` and MacOS `.dylib`. Rename
the file to `nemu_libretro.<ext>` where `<ext>` is the extension of your OS.

## Installing

To get metadata for the core, put the file `nemu_libretro.info` in your RetroArch `info/`
directory.

To install the core, press "Install core" and navigate to the file that you
built in the build step.
