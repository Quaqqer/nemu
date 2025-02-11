#![allow(clippy::new_without_default, clippy::single_match, clippy::identity_op)]
#![feature(let_chains)]

pub mod apu;
pub mod carts;
pub mod config;
pub mod controller;
pub mod cpu;
pub mod debug;
pub mod emulator;
pub mod nes_cpu_bus;
pub mod ppu;
mod util;

#[cfg(test)]
mod test;
