#![allow(clippy::new_without_default, clippy::single_match, clippy::identity_op)]
#![feature(let_chains)]

use eframe::egui::{
    self, load::SizedTexture, Color32, ColorImage, Context, FontDefinitions, Frame, Pos2, Rect,
    Rounding, TextureHandle, TextureOptions, Ui, Vec2,
};
use egui::Id;
use nemu::{
    cpu::{Cpu, CpuBus},
    emulator::Emulator,
    ppu::{PpuCtrl, PALETTE},
};

fn main() {
    let native_options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_min_inner_size(Vec2::new(512., 512.)),
        ..Default::default()
    };

    eframe::run_native(
        "Nemu",
        native_options,
        Box::new(|cc| Box::new(NemuApp::new(cc))),
    )
    .expect("Shouldn't just crash?");
}

struct NemuApp {
    // Emulator stuff
    emulator: Option<nemu::emulator::Emulator>,
    paused: bool,
    tex: TextureHandle,
    nt1: TextureHandle,
    nt2: TextureHandle,
    nt3: TextureHandle,
    nt4: TextureHandle,
    pt1: TextureHandle,
    pt2: TextureHandle,

    // UI State
    cpu_debug_open: bool,
    ppu_debug_open: bool,
    pattern_tables_open: bool,
    nametable_open: bool,

    selected_palette: u8,
}

impl NemuApp {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        cc.egui_ctx.set_fonts(Self::create_fonts());

        let empty_tex = |name: &str, x: usize, y: usize| {
            cc.egui_ctx.load_texture(
                name,
                ColorImage::new([x, y], Color32::BLACK),
                TextureOptions {
                    magnification: egui::TextureFilter::Nearest,
                    ..Default::default()
                },
            )
        };

        let tex = empty_tex("tex", 256, 240);
        let pt1 = empty_tex("pt1", 256, 256);
        let pt2 = empty_tex("pt2", 256, 256);
        let nt1 = empty_tex("nt1", 32 * 8, 30 * 8);
        let nt2 = empty_tex("nt1", 32 * 8, 30 * 8);
        let nt3 = empty_tex("nt1", 32 * 8, 30 * 8);
        let nt4 = empty_tex("nt1", 32 * 8, 30 * 8);

        Self {
            emulator: None,
            paused: false,
            tex,
            pt1,
            pt2,
            nt1,
            nt2,
            nt3,
            nt4,

            cpu_debug_open: false,
            ppu_debug_open: false,
            pattern_tables_open: false,
            nametable_open: false,

            selected_palette: 0,
        }
    }

    fn create_fonts() -> FontDefinitions {
        let mut fonts = egui::FontDefinitions::default();
        fonts.font_data.insert(
            "Cozette".to_string(),
            egui::FontData::from_static(include_bytes!("../res/CozetteVector.ttf")),
        );
        fonts
            .families
            .get_mut(&egui::FontFamily::Proportional)
            .unwrap()
            .insert(0, "Cozette".to_string());
        fonts
            .families
            .get_mut(&egui::FontFamily::Monospace)
            .unwrap()
            .insert(0, "Cozette".to_string());
        fonts
    }
}

impl eframe::App for NemuApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if let Some(emu) = self.emulator.as_mut() {
            if !self.paused {
                emu.step_frame();
            }

            let frame = emu.display();
            self.tex.set(
                ColorImage::from_rgb([256, 240], &frame.pixels),
                TextureOptions {
                    magnification: egui::TextureFilter::Nearest,
                    ..Default::default()
                },
            );
        }

        egui::TopBottomPanel::new(egui::panel::TopBottomSide::Top, Id::new("Menu bar")).show(
            ctx,
            |ui| {
                egui::menu::bar(ui, |ui| {
                    ui.menu_button("File", |ui| {
                        if ui.button("Open").clicked() {
                            let path = rfd::FileDialog::new()
                                .add_filter("NES", &["nes"])
                                .pick_file();

                            if let Some(path) = path {
                                self.emulator = Some(nemu::emulator::Emulator::new(
                                    nemu::cart::Cart::read_ines1_0(
                                        path.to_str().expect("Should be convertible to str"),
                                    ),
                                ));
                                self.paused = false;
                            }
                        };

                        if ui.button("Open paused").clicked() {
                            let path = rfd::FileDialog::new()
                                .add_filter("NES", &["nes"])
                                .pick_file();

                            if let Some(path) = path {
                                self.emulator = Some(nemu::emulator::Emulator::new(
                                    nemu::cart::Cart::read_ines1_0(
                                        path.to_str().expect("Should be convertible to str"),
                                    ),
                                ));
                                self.paused = true;
                            }
                        };
                    });

                    ui.menu_button("Emulation", |ui| {
                        if ui
                            .button(if self.paused { "Resume" } else { "Pause" })
                            .clicked()
                        {
                            self.paused = !self.paused;
                        }
                    });

                    ui.menu_button("Debug", |ui| {
                        if ui.button("CPU").clicked() {
                            self.cpu_debug_open = true;
                        };
                        if ui.button("PPU").clicked() {
                            self.ppu_debug_open = true;
                        };
                        if ui.button("Pattern tables").clicked() {
                            self.pattern_tables_open = true;
                        };
                        if ui.button("Nametables").clicked() {
                            self.nametable_open = true;
                        };
                    });
                });
            },
        );

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.vertical_centered(|ui| {
                egui::Frame::default().show(ui, |ui| {
                    let sized: SizedTexture = (&self.tex).into();
                    let img = egui::Image::from_texture(sized)
                        .maintain_aspect_ratio(true)
                        .shrink_to_fit();
                    ui.add(img);
                });
            });
        });

        self.cpu_debug_window(ctx);
        self.ppu_debug_window(ctx);
        self.pattern_tables_window(ctx);
        self.nametable_window(ctx);

        // Always repaint
        ctx.request_repaint();
    }

    fn raw_input_hook(&mut self, _ctx: &egui::Context, raw_input: &mut egui::RawInput) {
        for ev in &raw_input.events {
            match ev {
                egui::Event::Key {
                    key, pressed: true, ..
                } => match key {
                    egui::Key::P => {
                        self.paused ^= true;
                    }
                    egui::Key::S => {
                        if let Some(emu) = self.emulator.as_mut() {
                            emu.step_scanline();
                        }
                    }
                    egui::Key::N => {
                        if let Some(emu) = self.emulator.as_mut() {
                            emu.step();
                        }
                    }
                    egui::Key::F => {
                        if let Some(emu) = self.emulator.as_mut() {
                            emu.step_frame();
                        }
                    }
                    egui::Key::M => {
                        self.selected_palette = (self.selected_palette + 1) % 8;
                    }
                    _ => {}
                },
                _ => {}
            }
        }
    }
}

impl NemuApp {
    fn cpu_debug_window(&mut self, ctx: &Context) {
        egui::Window::new("CPU Debug")
            .open(&mut self.cpu_debug_open)
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
                } = emu;

                let cpu_bus = &mut CpuBus { apu, ppu, cart };

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
                        cpu.inspect_mem8(cpu_bus, cpu.pc).unwrap()
                    ));
                    ui.end_row();
                });

                egui::scroll_area::ScrollArea::new([false, true]).show(ui, |ui| {
                        for i in 0..Cpu::HISTORY_LEN {
                            let history_i = cpu.history_i.wrapping_sub(i) % Cpu::HISTORY_LEN;
                            let offset = cpu.history[history_i];

                            if let Some(opcode) = cpu.inspect_mem8(cpu_bus, offset) {
                                let (op, addr_mode, _, _) = &nemu::op::OPCODE_MATRIX[opcode as usize];

                                macro_rules! read_8(($base:expr, $offset:expr) => {
                                    cpu.inspect_mem8(cpu_bus, $base.wrapping_add($offset)).map(|v| format!("${:02x}", v)).unwrap_or("$??".to_string())
                                });

                                macro_rules! read_16(($base:expr, $offset:expr) => {
                                    cpu.inspect_mem16(cpu_bus, $base.wrapping_add($offset)).map(|v| format!("${:04x}", v)).unwrap_or("$????".to_string())
                                });

                                let addr_format = match addr_mode {
                                    nemu::op::AddrMode::Acc => "A {{ACC}}".to_string(),
                                    nemu::op::AddrMode::Imm => {
                                        format!("#{} {{IMM}}", read_8!(offset, 1))
                                    }
                                    nemu::op::AddrMode::Zp0 => format!("{} {{ZP}}", read_8!(offset, 1)),
                                    nemu::op::AddrMode::ZpX => {
                                        format!("{},x {{ZPX}}", read_8!(offset, 1))
                                    }
                                    nemu::op::AddrMode::ZpY => {
                                        format!("{},y {{ZPY}}", read_8!(offset, 1))
                                    }
                                    nemu::op::AddrMode::Abs => {
                                        format!("{} {{ABS}}", read_16!(offset, 1))
                                    }
                                    nemu::op::AddrMode::AbX => {
                                        format!("{},x {{ABX}}", read_16!(offset, 1))
                                    }
                                    nemu::op::AddrMode::AbY => {
                                        format!("{},y {{ABY}}", read_16!(offset, 1))
                                    }
                                    nemu::op::AddrMode::Rel => {
                                        format!("{} {{REL}}", read_8!(offset, 1))
                                    }
                                    nemu::op::AddrMode::Ind => {
                                        format!("{} {{IND}}", read_16!(offset, 1))
                                    }
                                    nemu::op::AddrMode::IdX => {
                                        format!("{},x {{IDX}}", read_16!(offset, 1))
                                    }
                                    nemu::op::AddrMode::IdY => {
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

    fn ppu_debug_window(&mut self, ctx: &Context) {
        egui::Window::new("PPU Debug")
            .open(&mut self.ppu_debug_open)
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
                    ui.label(format!("{:#04x}", ppu.t));
                    ui.end_row();

                    ui.label("V");
                    ui.label(format!("{:#04x}", ppu.v));
                    ui.end_row();

                    ui.label("Scanline");
                    ui.label(format!("{}", ppu.scanline));
                    ui.end_row();

                    ui.label("Cycle");
                    ui.label(format!("{}", ppu.cycle));
                    ui.end_row();

                    ui.label("Coarse x");
                    ui.label(format!("{}", ppu.coarse_x()));
                    ui.end_row();

                    ui.label("Coarse y");
                    ui.label(format!("{}", ppu.coarse_y()));
                    ui.end_row();

                    ui.label("Fine x");
                    ui.label(format!("{}", ppu.fine_x()));
                    ui.end_row();

                    ui.label("Fine y");
                    ui.label(format!("{}", ppu.fine_y()));
                    ui.end_row();
                });
            });
    }

    fn pattern_tables_window(&mut self, ctx: &Context) {
        egui::Window::new("Pattern tables")
            .open(&mut self.pattern_tables_open)
            .show(ctx, |ui| {
                let Some(emu) = self.emulator.as_ref() else {
                    ui.label("No emulator is active");
                    return;
                };

                let read_tiles = |tex: &mut TextureHandle, page: u8| {
                    let mut buf: [u8; 128 * 128 * 3] = [0; 49152];

                    for x in 0..128 {
                        for y in 0..128 {
                            let tile_x = x / 8;
                            let tile_y = y / 8;

                            let pixel =
                                emu.cart
                                    .get_sprite_pixel(page, tile_x, tile_y, x % 8, y % 8);

                            let palette = Self::palette(
                                &emu.ppu.palette[self.selected_palette as usize * 4
                                    ..self.selected_palette as usize * 4 + 4]
                                    .try_into()
                                    .unwrap(),
                                &PALETTE,
                            );
                            let (r, g, b) = palette[pixel as usize];
                            let color = Color32::from_rgb(r, g, b);

                            let base = ((y as usize * 128) + x as usize) * 3;
                            buf[base] = color.r();
                            buf[base + 1] = color.g();
                            buf[base + 2] = color.b();
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

                let pt1 = read_tiles(&mut self.pt1, 0);
                let pt2 = read_tiles(&mut self.pt2, 1);

                ui.with_layout(egui::Layout::left_to_right(egui::Align::Min), |ui| {
                    ui.add(pt1);
                    ui.add(pt2);
                });

                ui.vertical(|ui| {
                    for (i, palette_ram) in emu.ppu.palette.chunks(4).enumerate() {
                        Self::palette_row(
                            ui,
                            Self::palette(palette_ram.try_into().unwrap(), &PALETTE),
                        );
                    }
                });
            });
    }

    fn nametable_window(&mut self, ctx: &Context) {
        egui::Window::new("Name tables")
            .open(&mut self.nametable_open)
            .show(ctx, |ui| {
                let Some(emu) = self.emulator.as_ref() else {
                    ui.label("No emulator is active");
                    return;
                };

                // Assumes that pattern tables are stored in vram
                let Emulator { ppu, cart, .. } = emu;

                let nt_img = |tex: &mut TextureHandle, nt: u8| {
                    let mut buf: [u8; 30 * 32 * 8 * 8 * 3] = [0; 184320];
                    for row in 0..30 {
                        for col in 0..32 {
                            let nt_entry =
                                cart.inspect_nametable(ppu, nt, row as u16 * 32 + col as u16);

                            let sprite = cart.get_sprite_i(
                                ppu.ppuctrl.intersects(PpuCtrl::BACKGROUND_TILE) as u8,
                                nt_entry,
                            );
                            for dy in 0..8 {
                                let low = sprite[dy];
                                let high = sprite[dy + 8];
                                for dx in 0..8 {
                                    let base = ((row * 8 + dy) * (32 * 8) + (col * 8) + dx) * 3;
                                    let low_bit = (low >> (7 - dx)) & 1;
                                    let high_bit = (high >> (7 - dx)) & 1;
                                    let px = (high_bit << 1) | low_bit;
                                    let (r, g, b) = match px {
                                        0 => (0, 0, 0),
                                        1 => (125, 0, 0),
                                        2 => (0, 125, 0),
                                        3 => (0, 0, 125),
                                        _ => unreachable!(),
                                    };
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
                    ui.add(nt_img(&mut self.nt1, 0));
                    ui.add(nt_img(&mut self.nt2, 1));
                    ui.end_row();

                    ui.add(nt_img(&mut self.nt3, 2));
                    ui.add(nt_img(&mut self.nt4, 3));
                    ui.end_row();
                });
            });
    }

    fn palette(palette_ram: &[u8; 4], palette: &[u8; 64 * 3]) -> [(u8, u8, u8); 4] {
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

    fn palette_row(ui: &mut Ui, colors: [(u8, u8, u8); 4]) {
        let w = 10.;

        ui.horizontal(|ui| {
            for &(r, g, b) in colors.iter() {
                let rect = ui.allocate_space(Vec2::new(w, w)).1;
                let color = Color32::from_rgb(r, g, b);
                let rounding = 0.;

                ui.painter().rect_filled(rect, rounding, color);
            }
        });
    }
}
