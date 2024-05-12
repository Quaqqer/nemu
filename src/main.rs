#![allow(clippy::new_without_default)]
#![feature(let_chains)]

use std::collections::VecDeque;

use eframe::egui::{
    self, load::SizedTexture, Color32, ColorImage, Context, FontDefinitions, TextureHandle,
    TextureOptions, Vec2,
};
use egui::Id;
use nemu::{cpu::CpuBus, emulator::Emulator};

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

const OP_HISTORY_LIMIT: usize = 10;

struct NemuApp {
    // Emulator stuff
    emulator: Option<nemu::emulator::Emulator>,
    paused: bool,
    tex: TextureHandle,
    pt1: TextureHandle,
    pt2: TextureHandle,

    // Disassembler
    op_history: VecDeque<String>,

    // UI State
    cpu_debug_open: bool,
    pattern_tables_open: bool,
}

impl NemuApp {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        cc.egui_ctx.set_fonts(Self::create_fonts());

        let tex = cc.egui_ctx.load_texture(
            "tex",
            ColorImage::new([256, 240], Color32::BLACK),
            TextureOptions {
                magnification: egui::TextureFilter::Nearest,
                ..Default::default()
            },
        );

        let pt1 = cc.egui_ctx.load_texture(
            "pt1",
            ColorImage::new([256, 240], Color32::BLACK),
            TextureOptions {
                magnification: egui::TextureFilter::Nearest,
                ..Default::default()
            },
        );
        let pt2 = cc.egui_ctx.load_texture(
            "pt2",
            ColorImage::new([256, 240], Color32::BLACK),
            TextureOptions {
                magnification: egui::TextureFilter::Nearest,
                ..Default::default()
            },
        );

        Self {
            emulator: None,
            paused: false,
            tex,
            pt1,
            pt2,

            op_history: Default::default(),

            cpu_debug_open: false,
            pattern_tables_open: false,
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
        if let Some(emu) = self.emulator.as_mut()
            && !self.paused
        {
            let frame = emu.render_frame();
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
                    for history_label in &self.op_history {
                        ui.label(history_label);
                    }

                    let mut offset = cpu.pc;
                    for i in 0..10 {
                        if let Some(opcode) = cpu.inspect_mem8(cpu_bus, cpu.pc) {
                            let (op, addr_mode, _, _) = &nemu::op::OPCODE_MATRIX[opcode as usize];

                            macro_rules! read_8(($base:expr, $offset:expr) => {
                                cpu.inspect_mem8(cpu_bus, $base.wrapping_add($offset)).map(|v| format!("{:02x}", v)).unwrap_or("??".to_string())
                            });

                            macro_rules! read_16(($base:expr, $offset:expr) => {
                                cpu.inspect_mem16(cpu_bus, $base.wrapping_add($offset)).map(|v| format!("{:04x}", v)).unwrap_or("????".to_string())
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

                            offset = offset
                                .wrapping_add(1)
                                .wrapping_add(addr_mode.fetched_bytes() as u16);

                            if i == 0 {
                                // TODO: This is wrong since we tick an entire frame instead of
                                // a cpu instruction
                                self.op_history.push_back(s.clone());

                                if self.op_history.len() > OP_HISTORY_LIMIT {
                                    self.op_history.pop_front();
                                }
                            }

                            ui.label(s);
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
}
