use eframe::egui::{
    self, load::SizedTexture, Color32, ColorImage, Context, TextureHandle, TextureOptions,
};
use egui::Id;

fn main() {
    let native_options = eframe::NativeOptions::default();

    // TODO: REmvoe
    // let path = rfd::FileDialog::new()
    //     .add_filter("NES", &["nes"])
    //     .pick_file()
    //     .unwrap();

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
    tex: TextureHandle,
    pt1: TextureHandle,
    pt2: TextureHandle,

    // UI State
    cpu_debug_open: bool,
    pattern_tables_open: bool,
}

impl NemuApp {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
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
            tex,
            pt1,
            pt2,

            cpu_debug_open: false,
            pattern_tables_open: false,
        }
    }
}

impl eframe::App for NemuApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if let Some(emu) = self.emulator.as_mut() {
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
                            }
                        };
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

        egui::SidePanel::new(egui::panel::Side::Right, Id::new("Sidebar")).show(ctx, |ui| {
            ui.label("Hello there1");
            ui.label("Hello there2");
            ui.label("Hello there3");
            ui.label("Hello there4");
        });

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
                let Some(emu) = self.emulator.as_ref() else {
                    ui.label("No emulator is active");
                    return;
                };

                egui::Grid::new("CPU Debug Grid").show(ui, |ui| {
                    let cpu = &emu.cpu;
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
                    ui.label(format!("{:#02x}", cpu.read_mem8_safe(cpu.pc).unwrap()));
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

                let chr = &emu.cpu.bus.cart.chr;

                let read_tiles = |offset: usize| {
                    let mut buf: [u8; 128 * 128 * 3] = [0; 49152];

                    for x in 0_usize..128 {
                        for y in 0_usize..128 {
                            let tile_x = x / 8;
                            let tile_y = y / 8;

                            let base = (tile_y * 16 + tile_x) * 16;
                            let pixel_l = (chr[base + y % 8] >> (7 - x % 8)) & 0x1;
                            let pixel_r = (chr[base + 8 + y % 8] >> (7 - x % 8)) & 0x1;
                            let pixel = (pixel_r << 1) | pixel_l;

                            let color = match pixel {
                                0 => Color32::RED,
                                1 => Color32::GREEN,
                                2 => Color32::BLUE,
                                3 => Color32::WHITE,
                                _ => unreachable!(),
                            };

                            let base = ((y * 128) + x) * 3;
                            buf[base] = color.r();
                            buf[base + 1] = color.g();
                            buf[base + 2] = color.b();
                        }
                    }

                    ColorImage::from_rgb([128, 128], &buf)
                };

                let pt1_img = read_tiles(0x0000);
                self.pt1.set(
                    pt1_img,
                    TextureOptions {
                        magnification: egui::TextureFilter::Nearest,
                        ..Default::default()
                    },
                );

                let pt2_img = read_tiles(0x0000);
                self.pt2.set(
                    pt2_img,
                    TextureOptions {
                        magnification: egui::TextureFilter::Nearest,
                        ..Default::default()
                    },
                );

                ui.image(&self.pt1);
                ui.image(&self.pt2);
            });
    }
}
