# Nemu

Nemu is my NES emulator. It currently runs quite a few games. Thanks to WASM,
the emulator runs in the browser over at
[Quaqqer.com/projects/nemu](https://quaqqer.com/projects/nemu).

![A screenshot of Nemu running Mario Bros. 3 with save states](https://raw.githubusercontent.com/Quaqqer/nemu/master/.github/res/github/screenshot.png)

## Features

- [x] CPU emulation
- [x] PPU emulation (Picture Processing Unit)
- [ ] Audio emulation
- [x] Input
- [x] Debugging
  - [x] CPU info viewer
  - [x] PPU info viewer
  - [x] Pattern table viewer
  - [x] Nametable viewer

## Mappers

Different games for the NES use different types of cartridges. These cartridges
have different internal wirings which must be emulated as well, hence every
single type of cartridge must be implemented correctly to support all NES
games. The implementations of these cartridges are referred to as mappers.

- [x] [Mapper 0](https://nesdir.github.io/mapper0.html) ~10% of games
- [x] [Mapper 1](https://nesdir.github.io/mapper1.html) ~28% of games
- [x] [Mapper 2](https://nesdir.github.io/mapper2.html) ~11% of games
- [x] [Mapper 3](https://nesdir.github.io/mapper3.html) ~6% of games
- [x] [Mapper 4](https://nesdir.github.io/mapper4.html) ~24% of games
- [ ] [Mapper 7](https://nesdir.github.io/mapper7.html) ~3% of games (BATTLETOADS!?!?!?)
- [ ] ...

## Acknowledgements

Thanks to

- The [NESDev wiki](https://www.nesdev.org/) for providing a reference for the NES hardware
- Nestest for providing a great test suite for the CPU
- [javid9x](https://www.youtube.com/@javidx9) for videos explaining how the NES PPU works
- The wonderful people over at [r/EmuDev](https://www.reddit.com/r/EmuDev/)'s Discord.
- [SingleStepTests](https://github.com/SingleStepTests) for the final
  percentages of CPU accuracy.
