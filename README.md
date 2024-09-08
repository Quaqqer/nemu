# Nemu

Nemu is my WIP nes emulator. Currently it can run Donkey Kong quite
successfully, and at least the first levels in Super Mario Bros.

The emulator is able to run in the browser over at
[Quaqqer.com/projects/nemu](https://quaqqer.com/projects/nemu), although the
debugging features are missing there.

![A video of Nemu running Donkey Kong](https://raw.githubusercontent.com/Quaqqer/nemu/master/.github/res/dk.gif)
![A screenshot of Nemu running Donkey Kong with debugging utilities](https://raw.githubusercontent.com/Quaqqer/nemu/master/.github/res/screenshot.png)

## Features

- CPU emulation
- PPU emulation (Picture Processing Unit)
- Keyboard controls
- Debugging
  - CPU info viewer
  - PPU info viewer
  - Pattern table viewer
  - Nametable viewer

## Missing features

- Audio emulation

## Mappers

Currently only [games running on mapper
0](https://nesdir.github.io/mapper0.html) are supported. Different game carts
have different internal wiring which must be emulated, at the moment I have
only implemented one type of cart. Some games that should run are Donkey Kong,
Super Mario Bros 1, and Ice Climber.

- [x] Mapper 0

## Acknowledgements

Thanks to

- The [NESDev wiki](https://www.nesdev.org/) for providing a reference for the NES hardware
- Nestest for providing a great test suite for the CPU
- [javid9x](https://www.youtube.com/@javidx9) for videos explaining how the NES PPU works
- The wonderful people over at [r/EmuDev](https://www.reddit.com/r/EmuDev/)'s Discord.
