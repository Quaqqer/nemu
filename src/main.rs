#![allow(clippy::new_without_default, clippy::single_match)]
#![feature(let_chains)]

use eframe::egui::{
    self, load::SizedTexture, Color32, ColorImage, Context, FontDefinitions, TextureHandle,
    TextureOptions, Vec2,
};
use egui::Id;
use nemu::{
    cpu::{Cpu, CpuBus},
    emulator::Emulator,
    ppu::PpuCtrl,
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
    pattern_tables_open: bool,
    nametable_open: bool,
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
        let pt2 = empty_tex("pt1", 256, 256);
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
            pattern_tables_open: false,
            nametable_open: false,
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
                    ui.label(format!("{:#02x}", cpu.p));
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

    fn pattern_tables_window(&mut self, ctx: &Context) {
        egui::Window::new("Pattern tables")
            .open(&mut self.pattern_tables_open)
            .show(ctx, |ui| {
                let Some(emu) = self.emulator.as_ref() else {
                    ui.label("No emulator is active");
                    return;
                };

                let read_tiles = |page: u8| {
                    let mut buf: [u8; 128 * 128 * 3] = [0; 49152];

                    for x in 0..128 {
                        for y in 0..128 {
                            let tile_x = x / 8;
                            let tile_y = y / 8;

                            let pixel =
                                emu.cart
                                    .get_sprite_pixel(page, tile_x, tile_y, x % 8, y % 8);

                            let color = match pixel {
                                0 => Color32::BLACK,
                                1 => Color32::GREEN,
                                2 => Color32::BLUE,
                                3 => Color32::WHITE,
                                _ => unreachable!(),
                            };

                            let base = ((y as usize * 128) + x as usize) * 3;
                            buf[base] = color.r();
                            buf[base + 1] = color.g();
                            buf[base + 2] = color.b();
                        }
                    }

                    ColorImage::from_rgb([128, 128], &buf)
                };

                let pt1_img = read_tiles(0);
                self.pt1.set(
                    pt1_img,
                    TextureOptions {
                        magnification: egui::TextureFilter::Nearest,
                        ..Default::default()
                    },
                );
                let pt1_image = egui::Image::from_texture(SizedTexture::from(&self.pt1))
                    .fit_to_exact_size(Vec2::new(256., 256.));

                let pt2_img = read_tiles(1);
                self.pt2.set(
                    pt2_img,
                    TextureOptions {
                        magnification: egui::TextureFilter::Nearest,
                        ..Default::default()
                    },
                );
                let pt2_image = egui::Image::from_texture(SizedTexture::from(&self.pt2))
                    .fit_to_exact_size(Vec2::new(256., 256.));

                ui.with_layout(egui::Layout::left_to_right(egui::Align::Min), |ui| {
                    ui.add(pt1_image);
                    ui.add(pt2_image);
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

                let set_nt = |tex: &mut TextureHandle, nt: u8| {
                    let mut buf: [u8; 30 * 32 * 8 * 8 * 3] = [0; 184320];
                    for row in 0..30 {
                        for col in 0..32 {
                            let nt_entry =
                                cart.inspect_nametable(ppu, nt, row as u16 * 32 + col as u16);
                            let sprite = cart.get_sprite_i(
                                ppu.ppuctrl.intersects(PpuCtrl::NAMETABLE_ADDRESS) as u8,
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
                };
                set_nt(&mut self.nt1, 0);
                set_nt(&mut self.nt2, 1);
                set_nt(&mut self.nt3, 2);
                set_nt(&mut self.nt4, 3);

                egui::Grid::new("nametable_grid").show(ui, |ui| {
                    macro_rules! add_img {
                        ($tex:expr) => {
                            ui.add(
                                egui::Image::from_texture(SizedTexture::from($tex))
                                    .fit_to_exact_size(Vec2::new(32. * 8., 30. * 8.)),
                            );
                        };
                    }

                    add_img!(&self.nt1);
                    add_img!(&self.nt2);
                    ui.end_row();

                    add_img!(&self.nt3);
                    add_img!(&self.nt4);
                    ui.end_row();
                });
            });
    }
}
