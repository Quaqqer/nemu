use eframe::egui::ahash::HashMap;

use crate::action::{Action, NesButton, Toggleable};

fn nes_button(btn: NesButton, p2: bool) -> (Option<Action>, Option<Action>) {
    (
        Some(Action::ButtonDown { btn, p2 }),
        Some(Action::ButtonUp { btn, p2 }),
    )
}

pub(crate) type ActionMap = HashMap<eframe::egui::Key, (Option<Action>, Option<Action>)>;

pub(crate) fn create_action_map() -> ActionMap {
    use eframe::egui::Key;
    HashMap::from_iter(vec![
        (Key::P, (Some(Action::Toggle(Toggleable::Running)), None)),
        (Key::ArrowLeft, nes_button(NesButton::Left, false)),
        (Key::ArrowRight, nes_button(NesButton::Right, false)),
        (Key::ArrowDown, nes_button(NesButton::Down, false)),
        (Key::ArrowUp, nes_button(NesButton::Up, false)),
        (Key::Enter, nes_button(NesButton::Start, false)),
        (Key::Backspace, nes_button(NesButton::Select, false)),
        (Key::Z, nes_button(NesButton::B, false)),
        (Key::X, nes_button(NesButton::A, false)),
    ])
}
