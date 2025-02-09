#![allow(clippy::new_without_default, clippy::single_match, clippy::identity_op)]
#![feature(let_chains)]

mod action;
mod app;
mod map;

use app::NemuApp;
use eframe::egui::{self, Vec2};

fn main() {
    let icon = include_bytes!("../../../misc/logo.png");
    let image = image::load_from_memory(icon).expect("Failed to load icon");

    let native_options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_title("Nemu")
            .with_icon(egui::viewport::IconData {
                rgba: image.to_rgba8().to_vec(),
                width: image.width(),
                height: image.height(),
            })
            .with_min_inner_size(Vec2::new(512., 512.)),
        ..Default::default()
    };

    eframe::run_native(
        "Nemu",
        native_options,
        Box::new(|cc| Box::new(NemuApp::new(cc))),
    )
    .expect("Shouldn't just crash?");
}
