use super::NemuApp;
use eframe::egui::{
    self, load::SizedTexture, Color32, ColorImage, Context, TextureHandle, TextureOptions, Ui, Vec2,
};
use nemu_emulator::{
    cpu::{Cpu, CpuMemory},
    emulator::Emulator,
    nes_cpu_bus::NesCpuBus,
    ppu::PALETTE,
};

pub struct NemuAppDebug {
    // UI State
    pub open_cpu: bool,
    pub open_ppu: bool,
    pub open_pattern_tables: bool,
    pub open_nametables: bool,

    // Texture handles
    pub nt1: TextureHandle,
    pub nt2: TextureHandle,
    pub nt3: TextureHandle,
    pub nt4: TextureHandle,
    pub pt1: TextureHandle,
    pub pt2: TextureHandle,
}

impl NemuApp {
    pub(crate) fn cpu_debug_window(&mut self, ctx: &Context) {
        egui::Window::new("CPU Debug")
            .open(&mut self.debug.open_cpu)
            .show(ctx, |ui| {
                let Some(emu) = self.emulator.as_mut() else {
                    ui.label("No emulator is active");
                    return;
                };

                let Emulator {
                    cpu,
                    apu,
                    ppu,
                    cart,
                    controllers,
                    controller_shifters,
                    ram,
                } = emu;

                let cpu_bus = &mut NesCpuBus { apu, ppu, cart: cart.as_mut(), controllers, controller_shifters, ram };

                egui::Grid::new("CPU Debug Grid").show(ui, |ui| {
                    ui.label("PC");
                    ui.label(format!("{:#04x}", cpu.pc));
                    ui.end_row();

                    ui.label("SP");
                    ui.label(format!("{:#02x}", cpu.sp));
                    ui.end_row();

                    ui.label("P");
                    ui.label(format!("{}", cpu.p));
                    ui.end_row();

                    ui.label("A");
                    ui.label(format!("{:#02x}", cpu.a));
                    ui.end_row();

                    ui.label("X");
                    ui.label(format!("{:#02x}", cpu.x));
                    ui.end_row();

                    ui.label("Y");
                    ui.label(format!("{:#02x}", cpu.y));
                    ui.end_row();

                    ui.label("OP");
                    ui.label(format!(
                        "{:#02x}",
                        cpu_bus.inspect(cpu, cpu.pc).unwrap()
                    ));
                    ui.end_row();
                });

                egui::scroll_area::ScrollArea::new([false, true]).show(ui, |ui| {
                        for i in 0..Cpu::HISTORY_LEN {
                            let history_i = cpu.history_i.wrapping_sub(i) % Cpu::HISTORY_LEN;
                            let offset = cpu.history[history_i];

                            if let Some(opcode) = cpu_bus.inspect(cpu, offset) {
                                let (op, addr_mode, _, _) = &nemu_emulator::cpu::op::OPCODE_MATRIX[opcode as usize];

                                macro_rules! read_8(($base:expr, $offset:expr) => {
                                    cpu_bus.inspect(cpu, $base.wrapping_add($offset)).map(|v| format!("${:02x}", v)).unwrap_or("$??".to_string())
                                });

                                macro_rules! read_16(($base:expr, $offset:expr) => {
                                    cpu.inspect_mem16(cpu_bus, $base.wrapping_add($offset)).map(|v| format!("${:04x}", v)).unwrap_or("$????".to_string())
                                });

                                let addr_format = match addr_mode {
                                    nemu_emulator::cpu::op::AddrMode::Acc => "A {{ACC}}".to_string(),
                                    nemu_emulator::cpu::op::AddrMode::Imm => {
                                        format!("#{} {{IMM}}", read_8!(offset, 1))
                                    }
                                    nemu_emulator::cpu::op::AddrMode::Zp0 => format!("{} {{ZP}}", read_8!(offset, 1)),
                                    nemu_emulator::cpu::op::AddrMode::ZpX => {
                                        format!("{},x {{ZPX}}", read_8!(offset, 1))
                                    }
                                    nemu_emulator::cpu::op::AddrMode::ZpY => {
                                        format!("{},y {{ZPY}}", read_8!(offset, 1))
                                    }
                                    nemu_emulator::cpu::op::AddrMode::Abs => {
                                        format!("{} {{ABS}}", read_16!(offset, 1))
                                    }
                                    nemu_emulator::cpu::op::AddrMode::AbX => {
                                        format!("{},x {{ABX}}", read_16!(offset, 1))
                                    }
                                    nemu_emulator::cpu::op::AddrMode::AbY => {
                                        format!("{},y {{ABY}}", read_16!(offset, 1))
                                    }
                                    nemu_emulator::cpu::op::AddrMode::Rel => {
                                        format!("{} {{REL}}", read_8!(offset, 1))
                                    }
                                    nemu_emulator::cpu::op::AddrMode::Ind => {
                                        format!("{} {{IND}}", read_16!(offset, 1))
                                    }
                                    nemu_emulator::cpu::op::AddrMode::IdX => {
                                        format!("{},x {{IDX}}", read_16!(offset, 1))
                                    }
                                    nemu_emulator::cpu::op::AddrMode::IdY => {
                                        format!("{},y {{IDY}}", read_16!(offset, 1))
                                    }
                                };

                                let s = format!("${:#04x}: {} {}", offset, op, addr_format);

                                ui.label(s);
                            } else {
                            ui.label("$???? ?");
                        }
                    }
                });
            });
    }

    pub(crate) fn ppu_debug_window(&mut self, ctx: &Context) {
        egui::Window::new("PPU Debug")
            .open(&mut self.debug.open_ppu)
            .show(ctx, |ui| {
                let Some(emu) = self.emulator.as_mut() else {
                    ui.label("No emulator is active");
                    return;
                };

                let Emulator { ppu, .. } = emu;

                egui::Grid::new("CPU Debug Grid").show(ui, |ui| {
                    ui.label("CTRL");
                    ui.label(format!("{}", ppu.ppuctrl));
                    ui.end_row();

                    ui.label("MASK");
                    ui.label(format!("{}", ppu.ppumask));
                    ui.end_row();

                    ui.label("STATUS");
                    ui.label(format!("{}", ppu.ppustatus));
                    ui.end_row();

                    ui.label("T");
                    ui.label(format!("{:#04x}", ppu.t.into_bits()));
                    ui.end_row();

                    ui.label("V");
                    ui.label(format!("{:#04x}", ppu.v.into_bits()));
                    ui.end_row();

                    ui.label("Scanline");
                    ui.label(format!("{}", ppu.scanline));
                    ui.end_row();

                    ui.label("Cycle");
                    ui.label(format!("{}", ppu.cycle));
                    ui.end_row();

                    ui.label("Coarse x");
                    ui.label(format!("{}", ppu.v.coarse_x()));
                    ui.end_row();

                    ui.label("Coarse y");
                    ui.label(format!("{}", ppu.v.coarse_y()));
                    ui.end_row();

                    ui.label("Fine x");
                    ui.label(format!("{}", ppu.fine_x()));
                    ui.end_row();

                    ui.label("Fine y");
                    ui.label(format!("{}", ppu.v.fine_y()));
                    ui.end_row();
                });
            });
    }

    pub(crate) fn pattern_tables_window(&mut self, ctx: &Context) {
        egui::Window::new("Pattern tables")
            .open(&mut self.debug.open_pattern_tables)
            .show(ctx, |ui| {
                let Some(emu) = self.emulator.as_ref() else {
                    ui.label("No emulator is active");
                    return;
                };

                let read_tiles = |tex: &mut TextureHandle, pattern_table: usize| {
                    let mut buf: [u8; 128 * 128 * 3] = [0; 49152];

                    for tile_x in 0..16 {
                        for tile_y in 0..16 {
                            let sprite = nemu_emulator::debug::get_sprite(
                                emu.cart.as_ref(),
                                pattern_table,
                                tile_x,
                                tile_y,
                            );

                            for dx in 0..8 {
                                for dy in 0..8 {
                                    let pixel =
                                        nemu_emulator::debug::get_sprite_px(&sprite, dx, dy);

                                    let palette = Self::palette(
                                        &emu.ppu.palette[self.selected_palette as usize * 4
                                            ..self.selected_palette as usize * 4 + 4]
                                            .try_into()
                                            .unwrap(),
                                        &PALETTE,
                                    );
                                    let (r, g, b) = palette[pixel as usize];
                                    let color = Color32::from_rgb(r, g, b);

                                    let x = tile_x * 8 + dx;
                                    let y = tile_y * 8 + dy;

                                    let base = ((y * 128) + x) * 3;
                                    buf[base] = color.r();
                                    buf[base + 1] = color.g();
                                    buf[base + 2] = color.b();
                                }
                            }
                        }
                    }

                    let cimg = ColorImage::from_rgb([128, 128], &buf);
                    tex.set(
                        cimg,
                        TextureOptions {
                            magnification: egui::TextureFilter::Nearest,
                            ..Default::default()
                        },
                    );
                    egui::Image::from_texture(SizedTexture::from(&*tex))
                        .fit_to_exact_size(Vec2::new(256., 256.))
                };

                let pt1 = read_tiles(&mut self.debug.pt1, 0);
                let pt2 = read_tiles(&mut self.debug.pt2, 1);

                ui.with_layout(egui::Layout::left_to_right(egui::Align::Min), |ui| {
                    ui.add(pt1);
                    ui.add(pt2);
                });

                ui.vertical(|ui| {
                    for (i, palette_ram) in emu.ppu.palette.chunks(4).enumerate() {
                        ui.push_id(i, |ui| {
                            Self::palette_row(
                                ui,
                                Self::palette(palette_ram.try_into().unwrap(), &PALETTE),
                                i as u8,
                                &mut self.selected_palette,
                            );
                        });
                    }
                });
            });
    }

    pub(crate) fn nametable_window(&mut self, ctx: &Context) {
        egui::Window::new("Name tables")
            .open(&mut self.debug.open_nametables)
            .show(ctx, |ui| {
                let Some(emu) = self.emulator.as_ref() else {
                    ui.label("No emulator is active");
                    return;
                };

                // Assumes that pattern tables are stored in vram
                let Emulator { ppu, cart, .. } = emu;

                let nt_img = |tex: &mut TextureHandle, nt: usize| {
                    let mut buf: [u8; 30 * 32 * 8 * 8 * 3] = [0; 184320];
                    for row in 0..30 {
                        for col in 0..32 {
                            let nt_entry = nemu_emulator::debug::get_nametable_entry(
                                ppu,
                                cart.as_ref(),
                                nt,
                                col,
                                row,
                            );

                            let sprite = nemu_emulator::debug::get_sprite_i(
                                cart.as_ref(),
                                0,
                                nt_entry as usize,
                            );

                            for dy in 0..8 {
                                for dx in 0..8 {
                                    let px = nemu_emulator::debug::get_sprite_px(&sprite, dx, dy);

                                    let palette = Self::palette(
                                        &emu.ppu.palette[self.selected_palette as usize * 4
                                            ..self.selected_palette as usize * 4 + 4]
                                            .try_into()
                                            .unwrap(),
                                        &PALETTE,
                                    );

                                    let (r, g, b) = palette[px as usize];

                                    let base = ((row * 8 + dy) * (32 * 8) + (col * 8) + dx) * 3;

                                    buf[base] = r;
                                    buf[base + 1] = g;
                                    buf[base + 2] = b;
                                }
                            }
                        }
                    }
                    let cimg = ColorImage::from_rgb([32 * 8, 30 * 8], &buf);
                    tex.set(
                        cimg,
                        TextureOptions {
                            magnification: egui::TextureFilter::Nearest,
                            ..Default::default()
                        },
                    );
                    egui::Image::from_texture(SizedTexture::from(&*tex))
                        .fit_to_exact_size(Vec2::new(32. * 8., 30. * 8.))
                };

                egui::Grid::new("nametable_grid").show(ui, |ui| {
                    ui.add(nt_img(&mut self.debug.nt1, 0));
                    ui.add(nt_img(&mut self.debug.nt2, 1));
                    ui.end_row();

                    ui.add(nt_img(&mut self.debug.nt3, 2));
                    ui.add(nt_img(&mut self.debug.nt4, 3));
                    ui.end_row();
                });
            });
    }

    pub(crate) fn palette(palette_ram: &[u8; 4], palette: &[u8; 64 * 3]) -> [(u8, u8, u8); 4] {
        let mut palette_row = [(0, 0, 0); 4];

        for (i, &p) in palette_ram.iter().enumerate() {
            let pi = (p as usize & 0x3F) * 3;
            let &[r, g, b] = &palette[pi..pi + 3] else {
                unreachable!();
            };
            palette_row[i] = (r, g, b);
        }

        palette_row
    }

    pub(crate) fn palette_row(
        ui: &mut Ui,
        colors: [(u8, u8, u8); 4],
        palette_i: u8,
        selected_palette: &mut u8,
    ) {
        let w = 10.;

        ui.horizontal(|ui| {
            for &(r, g, b) in colors.iter() {
                let rect = ui.allocate_space(Vec2::new(w, w)).1;
                let color = Color32::from_rgb(r, g, b);
                let rounding = 0.;

                ui.painter().rect_filled(rect, rounding, color);

                if ui.interact(rect, ui.id(), egui::Sense::click()).clicked() {
                    *selected_palette = palette_i;
                }
            }
        });
    }
}
