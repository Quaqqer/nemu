use std::{io::Read, path::PathBuf};

use nemu_emulator::controller::NesController;

use crate::app::NemuApp;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Action {
    OpenRom { paused: bool },
    LoadRom { path: PathBuf, paused: bool },
    SaveState(usize),
    LoadState(usize),
    Toggle(Toggleable),
    ButtonDown { btn: NesButton, p2: bool },
    ButtonUp { btn: NesButton, p2: bool },
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

                let cart = match nemu_emulator::carts::read_rom(&bytes) {
                    Ok(c) => c,
                    Err(msg) => {
                        Self::show_error(format!("Failed to laod rom: {}", msg));
                        return;
                    }
                };

                self.emulator = Some(nemu_emulator::emulator::Emulator::new(cart));
                self.paused = *paused;
            }
            Action::SaveState(slot) => {
                if let Some(state) = self.save_states.get_mut(*slot) {
                    *state = self.emulator.clone();
                }
            }
            Action::LoadState(slot) => {
                if let Some(state) = self.save_states.get(*slot) {
                    self.emulator = state.clone();
                }
            }
            Action::Toggle(t) => {
                *match t {
                    Toggleable::Running => {
                        self.paused = !self.paused;
                        if self.paused {
                            self.prev_time = None;
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
                if let Some(emu) = self.emulator.as_mut() {
                    let controller = &mut emu.controllers[if *p2 { 1 } else { 0 }];
                    *controller |= btn.mask();
                }
            }
            Action::ButtonUp { btn, p2 } => {
                if let Some(emu) = self.emulator.as_mut() {
                    let controller = &mut emu.controllers[if *p2 { 1 } else { 0 }];
                    *controller -= btn.mask();
                }
            }
        }
    }
}
