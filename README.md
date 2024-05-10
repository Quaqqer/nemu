# Nemu

Nemu is my WIP nes emulator. Currently CPU emulation is pretty complete, it
passes the entire headless NesTest rom. I have started working on the picture
processing unit (PPU), which will give me graphics, but I have yet to render
my first pixel.

There is a GUI, but since neither input nor output (graphics, audio) has been
implemented yet it only acts as a debugging utility. In the UI there is a CPU
debugging helper and a pattern table viewer (showing the currently loaded
sprites).

## Screenshot

![A screenshot of the debugging utilities of Nemu](https://raw.githubusercontent.com/Quaqqer/nemu/master/screenshot.png)
