use std::sync::{Arc, Mutex};

use debug::NemuAppDebug;
use eframe::egui::{
    self, load::SizedTexture, Color32, ColorImage, FontDefinitions, TextureHandle, TextureOptions,
    Ui, Vec2,
};
use egui::Id;
use nemu_emulator::{emulator::Emulator, ppu::Display};

use crate::{
    action::{Action, Toggleable},
    audio_runner::NemuAudioRunner,
    map::{create_action_map, ActionMap},
};

pub const SAVE_STATES: usize = 10;

mod debug;

#[derive(PartialEq)]
enum AspectRatio {
    Raw,
    Aspect4_3,
}

pub struct RunningState {
    pub emulator: Arc<Mutex<Emulator>>,
    pub runner: Option<NemuAudioRunner>,
}

pub(crate) struct NemuApp {
    // Emulator stuff
    pub running_state: Option<RunningState>,
    pub(crate) rom_name: Option<String>,
    pub(crate) config: Arc<Mutex<nemu_emulator::config::NemuConfig>>,
    pub(crate) save_states: Vec<Option<nemu_emulator::emulator::Emulator>>,
    pub tex: TextureHandle,

    pub(crate) debug: NemuAppDebug,

    selected_palette: u8,

    action_map: ActionMap,

    // Rendering options
    aspect_ratio: AspectRatio,
}

impl NemuApp {
    pub(crate) fn new(cc: &eframe::CreationContext<'_>) -> Self {
        cc.egui_ctx.set_fonts(Self::create_fonts());

        let empty_tex = |name: &str, x: usize, y: usize| {
            cc.egui_ctx.load_texture(
                name,
                ColorImage::filled([x, y], Color32::BLACK),
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
            running_state: None,
            rom_name: None,
            config: Default::default(),
            save_states: vec![None; SAVE_STATES],
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

            action_map: create_action_map(),

            aspect_ratio: AspectRatio::Aspect4_3,
        }
    }

    fn create_fonts() -> FontDefinitions {
        let mut fonts = egui::FontDefinitions::default();

        fonts.font_data.insert(
            "Cozette".to_string(),
            Arc::new(egui::FontData::from_static(include_bytes!(
                "../res/CozetteVector.ttf"
            ))),
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

    pub fn update_display(tex: &mut TextureHandle, display: &Display) {
        tex.set(
            ColorImage::from_rgb([256, 240], &display.pixels),
            TextureOptions {
                magnification: egui::TextureFilter::Nearest,
                ..Default::default()
            },
        );
    }
}

impl eframe::App for NemuApp {
    fn raw_input_hook(&mut self, _ctx: &egui::Context, raw_input: &mut egui::RawInput) {
        for ev in &raw_input.events {
            match ev {
                egui::Event::Key {
                    key, pressed: true, ..
                } => {
                    if let Some((Some(action), _)) = self.action_map.get(key).cloned() {
                        self.execute_action(&action)
                    }
                }
                egui::Event::Key {
                    key,
                    pressed: false,
                    ..
                } => {
                    if let Some((_, Some(action))) = self.action_map.get(key).cloned() {
                        self.execute_action(&action)
                    }
                }
                _ => {}
            }
        }
    }

    fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut eframe::Frame) {
        egui::Panel::top(Id::new("Menu bar")).show(ui, |ui| self.menu_bar(ui));

        egui::CentralPanel::default().show(ui, |ui| {
            ui.vertical_centered(|ui| {
                egui::Frame::default().show(ui, |ui| {
                    let sized: SizedTexture = (&self.tex).into();

                    let img = match self.aspect_ratio {
                        AspectRatio::Raw => egui::Image::from_texture(sized).shrink_to_fit(),
                        AspectRatio::Aspect4_3 => {
                            let aspect_w = 4.;
                            let aspect_h = 3.;

                            let scale = f32::min(
                                ui.available_width() / aspect_w,
                                ui.available_height() / aspect_h,
                            );

                            let width = scale * aspect_w;
                            let height = scale * aspect_h;

                            egui::Image::from_texture(sized)
                                .maintain_aspect_ratio(false)
                                .fit_to_exact_size(Vec2::new(width, height))
                        }
                    };

                    ui.add(img);
                });
            });
        });

        self.cpu_debug_window(ui);
        self.ppu_debug_window(ui);
        self.pattern_tables_window(ui);
        self.nametable_window(ui);

        // Always repaint
        ui.request_repaint();
    }
}

impl NemuApp {
    fn menu_bar(&mut self, ui: &mut Ui) {
        egui::menu::MenuBar::new().ui(ui, |ui| {
            ui.menu_button("File", |ui| {
                if ui.button("Open").clicked() {
                    self.execute_action(&Action::OpenRom { paused: false });
                };

                if ui.button("Open paused").clicked() {
                    self.execute_action(&Action::OpenRom { paused: true });
                };
            });

            ui.menu_button("Emulation", |ui| {
                if ui
                    .button(
                        if self
                            .running_state
                            .as_ref()
                            .and_then(|rs| rs.runner.as_ref())
                            .is_some()
                        {
                            "Pause"
                        } else {
                            "Resume"
                        },
                    )
                    .clicked()
                {
                    self.execute_action(&Action::Toggle(Toggleable::Running));
                }

                if ui.button("Reset").clicked() {
                    self.execute_action(&Action::Reset);
                }

                ui.menu_button("Save state", |ui| {
                    for i in 0..self.save_states.len() {
                        if ui
                            .add_enabled(
                                self.running_state.is_some(),
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
                    self.execute_action(&Action::Toggle(Toggleable::DebugCpu));
                };
                if ui.button("PPU").clicked() {
                    self.execute_action(&Action::Toggle(Toggleable::DebugPpu));
                };
                if ui.button("Pattern tables").clicked() {
                    self.execute_action(&Action::Toggle(Toggleable::DebugPatternTable));
                };
                if ui.button("Nametables").clicked() {
                    self.execute_action(&Action::Toggle(Toggleable::DebugNameTable));
                };
            });
            ui.menu_button("Graphics", |ui| {
                ui.label("Aspect ratio");
                ui.radio_value(&mut self.aspect_ratio, AspectRatio::Raw, "Raw");
                ui.radio_value(&mut self.aspect_ratio, AspectRatio::Aspect4_3, "4:3");
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
