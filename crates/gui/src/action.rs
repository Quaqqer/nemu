use std::{
    io::Read,
    path::PathBuf,
    sync::{Arc, Mutex},
};

use directories::ProjectDirs;
use nemu_emulator::{controller::NesController, emulator::Emulator};

use crate::{
    app::{NemuApp, RunningState, SAVE_STATES},
    audio_runner::NemuAudioRunner,
};

fn project_dirs() -> ProjectDirs {
    ProjectDirs::from("com", "quaqqer", "Nemu").unwrap()
}

fn save_state_dir() -> PathBuf {
    let pd = project_dirs();
    let mut dir = pd.data_dir().to_path_buf();
    dir.push("states");
    dir
}

fn save_state_path(rom_name: &str, i: usize) -> PathBuf {
    let mut dir = save_state_dir();
    dir.push(format!("{}.{}.nemu_state", rom_name, i));
    dir
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Action {
    OpenRom { paused: bool },
    LoadRom { path: PathBuf, paused: bool },
    SaveState(usize),
    LoadState(usize),
    Toggle(Toggleable),
    ButtonDown { btn: NesButton, p2: bool },
    ButtonUp { btn: NesButton, p2: bool },
    Reset,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub(crate) enum NesButton {
    Right,
    Left,
    Down,
    Up,
    Start,
    Select,
    B,
    A,
}

impl Action {
    #[allow(unused)]
    pub(crate) fn name(&self) -> String {
        match self {
            Action::OpenRom { paused } => match *paused {
                true => "Open ROM paused".to_string(),
                false => "Open ROM".to_string(),
            },
            Action::LoadRom { path, .. } => format!("Load ROM {}", path.to_str().unwrap_or("?")),
            Action::SaveState(n) => format!("Save state {}", n),
            Action::LoadState(n) => format!("Load state {}", n),
            Action::Toggle(t) => match t {
                Toggleable::Running => "Toggle running".to_string(),
                Toggleable::DebugCpu => "Toggle cpu debug".to_string(),
                Toggleable::DebugPpu => "Toggle PPU debug".to_string(),
                Toggleable::DebugPatternTable => "Toggle patern table viewer".to_string(),
                Toggleable::DebugNameTable => "Toggle name table viewer".to_string(),
            },
            Action::ButtonDown { btn, p2 } => {
                format!("{} press {}", if *p2 { "P2" } else { "P1" }, btn.name())
            }
            Action::ButtonUp { btn, p2 } => {
                format!("{} release {}", if *p2 { "P2" } else { "P1" }, btn.name())
            }
            Action::Reset => "reset".to_string(),
        }
    }
}

impl NesButton {
    pub(crate) fn name(&self) -> &'static str {
        match self {
            NesButton::Right => "DPAD right",
            NesButton::Left => "DPAD left",
            NesButton::Down => "DPAD down",
            NesButton::Up => "DPAD up",
            NesButton::Start => "Start",
            NesButton::Select => "Select",
            NesButton::B => "B",
            NesButton::A => "A",
        }
    }

    pub(crate) fn mask(&self) -> NesController {
        match self {
            NesButton::Right => NesController::RIGHT,
            NesButton::Left => NesController::LEFT,
            NesButton::Down => NesController::DOWN,
            NesButton::Up => NesController::UP,
            NesButton::Start => NesController::START,
            NesButton::Select => NesController::SELECT,
            NesButton::B => NesController::B,
            NesButton::A => NesController::A,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Toggleable {
    Running,
    DebugCpu,
    DebugPpu,
    DebugPatternTable,
    DebugNameTable,
}

impl NemuApp {
    pub fn execute_action(&mut self, action: &Action) {
        match action {
            Action::OpenRom { paused } => {
                let path = rfd::FileDialog::new()
                    .add_filter("NES", &["nes"])
                    .pick_file();

                if let Some(path) = path {
                    self.execute_action(&Action::LoadRom {
                        path,
                        paused: *paused,
                    });
                }
            }
            Action::LoadRom { path, paused } => {
                // Set rom name
                self.rom_name = Some(path.file_name().unwrap().to_str().unwrap().to_string());

                for i in 0..SAVE_STATES {
                    let ss = save_state_path(self.rom_name.as_ref().unwrap(), i);
                    if let Ok(f) = std::fs::File::open(ss).as_mut() {
                        let state = bincode::decode_from_std_read(f, bincode::config::standard());
                        match state {
                            Ok(state) => self.save_states[i] = Some(state),
                            Err(e) => eprintln!("Failed to load save state {}: {}", i, e),
                        }
                    };
                }

                let Ok(mut f) = std::fs::File::open(path) else {
                    Self::show_error(format!(
                        "Could not open file '{}'",
                        path.to_str().unwrap_or("?")
                    ));
                    return;
                };

                let mut bytes = Vec::new();
                let Ok(_) = f.read_to_end(&mut bytes) else {
                    Self::show_error("Failed to read rom contents");
                    return;
                };

                let cart = match nemu_emulator::carts::reader::read_rom(&bytes) {
                    Ok(c) => c,
                    Err(msg) => {
                        Self::show_error(format!("Failed to laod rom: {}", msg));
                        return;
                    }
                };

                let emulator = Arc::new(Mutex::new(Emulator::new(cart)));
                let mut tex = self.tex.clone();
                let config = self.config.clone();
                self.running_state = Some(RunningState {
                    emulator: emulator.clone(),
                    runner: if *paused {
                        None
                    } else {
                        Some(NemuAudioRunner::new(emulator, config, move |display| {
                            Self::update_display(&mut tex, &display)
                        }))
                    },
                });
            }
            Action::SaveState(slot) => {
                if let Some(state) = self.save_states.get_mut(*slot) {
                    *state = self
                        .running_state
                        .as_ref()
                        .map(|rs| rs.emulator.lock().unwrap().clone());

                    if let Some(emu) = state.as_ref() {
                        let rom_name = self.rom_name.as_ref().unwrap();

                        // Write to file
                        std::fs::create_dir_all(save_state_dir()).unwrap();
                        // Remove file if it already exists
                        let f_path = save_state_path(rom_name, *slot);
                        let _ = std::fs::remove_file(&f_path);
                        let mut f = std::fs::File::create_new(f_path).unwrap();
                        bincode::encode_into_std_write(emu, &mut f, bincode::config::standard())
                            .unwrap();
                    }
                }
            }
            Action::LoadState(slot) => {
                if let Some(Some(state)) = self.save_states.get(*slot) {
                    let emulator = Arc::new(Mutex::new(state.clone()));
                    let mut tex = self.tex.clone();
                    let config = self.config.clone();

                    self.running_state = Some(RunningState {
                        emulator: emulator.clone(),
                        runner: Some(NemuAudioRunner::new(emulator, config, move |display| {
                            Self::update_display(&mut tex, &display)
                        })),
                    });
                }
            }
            Action::Toggle(t) => {
                *match t {
                    Toggleable::Running => {
                        if let Some(running_state) = self.running_state.as_mut() {
                            if running_state.runner.is_some() {
                                running_state.runner = None;
                            } else {
                                let mut tex = self.tex.clone();
                                let config = self.config.clone();
                                let emulator = running_state.emulator.clone();
                                running_state.runner =
                                    Some(NemuAudioRunner::new(emulator, config, move |d| {
                                        Self::update_display(&mut tex, &d)
                                    }));
                            }
                        }
                        return;
                    }
                    Toggleable::DebugCpu => &mut self.debug.open_cpu,
                    Toggleable::DebugPpu => &mut self.debug.open_ppu,
                    Toggleable::DebugPatternTable => &mut self.debug.open_pattern_tables,
                    Toggleable::DebugNameTable => &mut self.debug.open_nametables,
                } ^= true;
            }
            Action::ButtonDown { btn, p2 } => {
                if let Some(mut emu) = self
                    .running_state
                    .as_mut()
                    .map(|rs| rs.emulator.lock().unwrap())
                {
                    let controller = &mut emu.controllers[if *p2 { 1 } else { 0 }];
                    *controller |= btn.mask();
                }
            }
            Action::ButtonUp { btn, p2 } => {
                if let Some(mut emu) = self
                    .running_state
                    .as_mut()
                    .map(|rs| rs.emulator.lock().unwrap())
                {
                    let controller = &mut emu.controllers[if *p2 { 1 } else { 0 }];
                    *controller -= btn.mask();
                }
            }
            Action::Reset => {
                if let Some(mut emu) = self
                    .running_state
                    .as_mut()
                    .map(|rs| rs.emulator.lock().unwrap())
                {
                    emu.reset();
                }
            }
        }
    }
}
