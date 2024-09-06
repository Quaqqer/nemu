# Nemu

Nemu is my WIP nes emulator. Currently it can run Donkey Kong quite
successfully, and at least the first levels in Super Mario Bros.

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

## Mappers

Currently only mapper 0 is implemented, since Nemu is still in its infant stages.

## Acknowledgements

Thanks to

- The [NESDev wiki](https://www.nesdev.org/) for providing a reference for the NES hardware
- Nestest for providing a great test suite for the CPU
- [javid9x](https://www.youtube.com/@javidx9) for videos explaining how the NES PPU works
- The wonderful people over at [r/EmuDev](https://www.reddit.com/r/EmuDev/)'s Discord.
