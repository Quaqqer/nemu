#![allow(clippy::new_without_default, clippy::single_match, clippy::identity_op)]
#![feature(let_chains)]

mod action;
mod app;

use app::NemuApp;
use eframe::egui::{self, Vec2};

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
