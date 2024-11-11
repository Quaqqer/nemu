use debug::NemuAppDebug;
use eframe::egui::{
    self, load::SizedTexture, Color32, ColorImage, FontDefinitions, TextureHandle, TextureOptions,
    Ui,
};
use egui::Id;
use nemu_emulator::controller::NesController;

use crate::action::{Action, Toggleable};

mod debug;

pub(crate) struct NemuApp {
    // Emulator stuff
    pub(crate) emulator: Option<nemu_emulator::emulator::Emulator>,
    config: nemu_emulator::config::NemuConfig,
    pub(crate) save_states: Vec<Option<nemu_emulator::emulator::Emulator>>,
    pub(crate) paused: bool,
    tex: TextureHandle,

    pub(crate) debug: NemuAppDebug,

    selected_palette: u8,

    unused_time: f64,
    pub(crate) prev_time: Option<std::time::Instant>,

    menu_bar_root: MenuBarRoot,
}

impl NemuApp {
    pub(crate) fn new(cc: &eframe::CreationContext<'_>) -> Self {
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
            config: Default::default(),
            save_states: vec![None; 10],
            paused: false,
            tex,
            debug: NemuAppDebug {
                open_cpu: false,
                open_ppu: false,
                open_pattern_tables: false,
                open_nametables: false,

                pt1,
                pt2,
                nt1,
                nt2,
                nt3,
                nt4,
            },

            selected_palette: 0,

            unused_time: 0.,
            prev_time: None,

            menu_bar_root: create_menu_bar(),
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
                let now = std::time::Instant::now();

                if let Some(prev_time) = self.prev_time {
                    self.unused_time += now.duration_since(prev_time).as_secs_f64();
                }

                self.prev_time = Some(now);

                while self.unused_time >= 1. / 60. {
                    self.unused_time -= 1. / 60.;
                    emu.step_frame(&self.config);
                }
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

        egui::TopBottomPanel::new(egui::panel::TopBottomSide::Top, Id::new("Menu bar"))
            .show(ctx, |ui| self.menu_bar(ui));

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
                        if self.paused {
                            self.prev_time = None;
                        }
                    }
                    egui::Key::M => {
                        self.selected_palette = (self.selected_palette + 1) % 8;
                    }

                    egui::Key::ArrowUp => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] |= NesController::UP;
                        }
                    }
                    egui::Key::ArrowDown => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] |= NesController::DOWN;
                        }
                    }
                    egui::Key::ArrowLeft => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] |= NesController::LEFT;
                        }
                    }
                    egui::Key::ArrowRight => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] |= NesController::RIGHT;
                        }
                    }
                    egui::Key::Enter => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] |= NesController::START;
                        }
                    }
                    egui::Key::Backspace => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] |= NesController::SELECT;
                        }
                    }
                    egui::Key::Z => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] |= NesController::B;
                        }
                    }
                    egui::Key::X => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] |= NesController::A;
                        }
                    }
                    _ => {}
                },
                egui::Event::Key {
                    key,
                    pressed: false,
                    ..
                } => match key {
                    egui::Key::ArrowUp => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] -= NesController::UP;
                        }
                    }
                    egui::Key::ArrowDown => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] -= NesController::DOWN;
                        }
                    }
                    egui::Key::ArrowLeft => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] -= NesController::LEFT;
                        }
                    }
                    egui::Key::ArrowRight => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] -= NesController::RIGHT;
                        }
                    }
                    egui::Key::Enter => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] -= NesController::START;
                        }
                    }
                    egui::Key::Backspace => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] -= NesController::SELECT;
                        }
                    }
                    egui::Key::Z => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] -= NesController::B;
                        }
                    }
                    egui::Key::X => {
                        if let Some(emu) = &mut self.emulator {
                            emu.controllers[0] -= NesController::A;
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
    fn menu_bar(&mut self, ui: &mut Ui) {
        egui::menu::bar(ui, |ui| {
            ui.menu_button("File", |ui| {
                if ui.button("Open").clicked() {
                    let path = rfd::FileDialog::new()
                        .add_filter("NES", &["nes"])
                        .pick_file();

                    if let Some(path) = path {
                        self.execute_action(&Action::LoadRom {
                            path,
                            paused: false,
                        });
                    }
                };

                if ui.button("Open paused").clicked() {
                    let path = rfd::FileDialog::new()
                        .add_filter("NES", &["nes"])
                        .pick_file();

                    if let Some(path) = path {
                        self.execute_action(&Action::LoadRom {
                            path,
                            paused: false,
                        });
                    }
                };
            });

            ui.menu_button("Emulation", |ui| {
                if ui
                    .button(if self.paused { "Resume" } else { "Pause" })
                    .clicked()
                {
                    self.execute_action(&Action::Toggle(Toggleable::Running));
                }

                ui.menu_button("Save state", |ui| {
                    for i in 0..self.save_states.len() {
                        if ui
                            .add_enabled(
                                self.emulator.is_some(),
                                egui::Button::new(format!("Save state {}", i + 1)),
                            )
                            .clicked()
                        {
                            self.execute_action(&Action::SaveState(i))
                        };
                    }
                });

                ui.menu_button("Load state", |ui| {
                    for i in 0..self.save_states.len() {
                        if ui
                            .add_enabled(
                                self.save_states[i].is_some(),
                                egui::Button::new(format!("Load state {}", i + 1)),
                            )
                            .clicked()
                        {
                            self.execute_action(&Action::LoadState(i))
                        };
                    }
                });
            });

            ui.menu_button("Debug", |ui| {
                if ui.button("CPU").clicked() {
                    self.debug.open_cpu ^= true;
                };
                if ui.button("PPU").clicked() {
                    self.debug.open_ppu ^= true;
                };
                if ui.button("Pattern tables").clicked() {
                    self.debug.open_pattern_tables ^= true;
                };
                if ui.button("Nametables").clicked() {
                    self.debug.open_nametables ^= true;
                };
            });
        });
    }

    pub(crate) fn show_error(error: impl Into<String>) {
        let m = rfd::MessageDialog::new()
            .set_level(rfd::MessageLevel::Error)
            .set_title("Nemu Error")
            .set_buttons(rfd::MessageButtons::Ok)
            .set_description(error);

        m.show();
    }
}

fn create_menu_bar() -> MenuBarRoot {
    MenuBarRoot(vec![
        (
            "File".to_string(),
            vec![MenuBar::CreateAction("Load ROM".to_string(), &|| {
                let path = rfd::FileDialog::new()
                    .add_filter("NES", &["nes"])
                    .pick_file();

                match path {
                    Some(path) => Action::LoadRom {
                        path,
                        paused: false,
                    },
                    None => Action::Noop,
                }
            })],
        ),
        (
            "Debug".to_string(),
            vec![
                MenuBar::Action(Action::Toggle(Toggleable::DebugCpu)),
                MenuBar::Action(Action::Toggle(Toggleable::DebugPpu)),
                MenuBar::Action(Action::Toggle(Toggleable::DebugNameTable)),
                MenuBar::Action(Action::Toggle(Toggleable::DebugPatternTable)),
            ],
        ),
    ])
}

pub struct MenuBarRoot(Vec<(String, Vec<MenuBar>)>);

pub enum MenuBar {
    List(String, Vec<(String, MenuBar)>),
    Action(Action),
    CreateAction(String, &'static dyn Fn() -> Action),
}

impl MenuBar {
    fn name(&self) -> String {
        match self {
            MenuBar::List(n, _) => n.clone(),
            MenuBar::Action(a) => a.name(),
            MenuBar::CreateAction(n, _) => n.clone(),
        }
    }
}
