# Nemu

Nemu is my WIP nes emulator. Currently it runs quite a few games.

The emulator running in the browser over at
[Quaqqer.com/projects/nemu](https://quaqqer.com/projects/nemu).

![A video of Nemu running Donkey Kong](https://raw.githubusercontent.com/Quaqqer/nemu/master/.github/res/github/dk.gif)
![A screenshot of Nemu running Donkey Kong with debugging utilities](https://raw.githubusercontent.com/Quaqqer/nemu/master/.github/res/github/screenshot.png)

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

Different games for the NES use different types of cartridges. These cartridges
have different internal wirings which must be emulated as well, hence every
single type of cartridge must be implemented correctly to support all NES
games. The implementations of these cartridges are referred to as mappers.

- [x] [Mapper 0](https://nesdir.github.io/mapper0.html)
- [-] [Mapper 1](https://nesdir.github.io/mapper1.html)
- [x] [Mapper 2](https://nesdir.github.io/mapper2.html)
- [x] [Mapper 3](https://nesdir.github.io/mapper3.html)
- [ ] [Mapper 4](https://nesdir.github.io/mapper4.html)
- [ ] ...

## Acknowledgements

Thanks to

- The [NESDev wiki](https://www.nesdev.org/) for providing a reference for the NES hardware
- Nestest for providing a great test suite for the CPU
- [javid9x](https://www.youtube.com/@javidx9) for videos explaining how the NES PPU works
- The wonderful people over at [r/EmuDev](https://www.reddit.com/r/EmuDev/)'s Discord.
