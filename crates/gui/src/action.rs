use std::{
    io::Read,
    path::{Path, PathBuf},
};

use crate::app::NemuApp;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Action {
    Noop,
    LoadRom { path: PathBuf, paused: bool },
    SaveState(usize),
    LoadState(usize),
    Resume,
    Pause,
    Toggle(Toggleable),
}

impl Action {
    pub(crate) fn name(&self) -> String {
        match self {
            Action::Noop => "Do nothing".to_string(),
            Action::LoadRom { path, .. } => format!("Load ROM {}", path.to_str().unwrap_or("?")),
            Action::SaveState(n) => format!("Save state {}", n),
            Action::LoadState(n) => format!("Load state {}", n),
            Action::Resume => "Resume".to_string(),
            Action::Pause => "Pause".to_string(),
            Action::Toggle(t) => match t {
                Toggleable::Running => "Toggle running".to_string(),
                Toggleable::DebugCpu => "Toggle cpu debug".to_string(),
                Toggleable::DebugPpu => "Toggle PPU debug".to_string(),
                Toggleable::DebugPatternTable => "Toggle patern table viewer".to_string(),
                Toggleable::DebugNameTable => "Toggle name table viewer".to_string(),
            },
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
            Action::Noop => {}
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
            Action::Resume => {
                self.paused = false;
            }
            Action::Pause => {
                self.paused = true;
                self.prev_time = None;
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
        }
    }
}
