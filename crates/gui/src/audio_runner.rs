use std::sync::{Arc, Mutex};

use cpal::traits::{DeviceTrait as _, HostTrait as _, StreamTrait as _};
use nemu_emulator::{config::NemuConfig, emulator::Emulator, ppu::Display};

const CPU_CLOCK: f32 = 1_789_773.;

pub struct NemuAudioRunner {
    stream: cpal::Stream,
}

impl NemuAudioRunner {
    pub fn new<F: FnMut(Display) + Send + 'static>(
        emulator: Arc<Mutex<Emulator>>,
        nemu_config: Arc<Mutex<NemuConfig>>,
        on_display_update: F,
    ) -> Self {
        let host = cpal::default_host();
        let device = host.default_output_device().unwrap();
        let config = device.default_output_config().unwrap();

        let stream = match config.sample_format() {
            cpal::SampleFormat::I8 => Self::create_stream::<i8, _>(
                emulator,
                nemu_config,
                &device,
                config.into(),
                on_display_update,
            ),
            cpal::SampleFormat::I16 => Self::create_stream::<i16, _>(
                emulator,
                nemu_config,
                &device,
                config.into(),
                on_display_update,
            ),
            cpal::SampleFormat::I24 => Self::create_stream::<cpal::I24, _>(
                emulator,
                nemu_config,
                &device,
                config.into(),
                on_display_update,
            ),
            cpal::SampleFormat::I32 => Self::create_stream::<i32, _>(
                emulator,
                nemu_config,
                &device,
                config.into(),
                on_display_update,
            ),
            cpal::SampleFormat::I64 => Self::create_stream::<i64, _>(
                emulator,
                nemu_config,
                &device,
                config.into(),
                on_display_update,
            ),
            cpal::SampleFormat::U8 => Self::create_stream::<u8, _>(
                emulator,
                nemu_config,
                &device,
                config.into(),
                on_display_update,
            ),
            cpal::SampleFormat::U16 => Self::create_stream::<u16, _>(
                emulator,
                nemu_config,
                &device,
                config.into(),
                on_display_update,
            ),
            cpal::SampleFormat::U24 => Self::create_stream::<cpal::U24, _>(
                emulator,
                nemu_config,
                &device,
                config.into(),
                on_display_update,
            ),
            cpal::SampleFormat::U32 => Self::create_stream::<u32, _>(
                emulator,
                nemu_config,
                &device,
                config.into(),
                on_display_update,
            ),
            cpal::SampleFormat::U64 => Self::create_stream::<u64, _>(
                emulator,
                nemu_config,
                &device,
                config.into(),
                on_display_update,
            ),
            cpal::SampleFormat::F32 => Self::create_stream::<f32, _>(
                emulator,
                nemu_config,
                &device,
                config.into(),
                on_display_update,
            ),
            cpal::SampleFormat::F64 => Self::create_stream::<f64, _>(
                emulator,
                nemu_config,
                &device,
                config.into(),
                on_display_update,
            ),
            unsupported => panic!("unsupported sample format '{}'", unsupported),
        };

        let mut runner = Self { stream };

        runner.play();

        runner
    }

    pub fn play(&mut self) {
        self.stream.play().unwrap();
    }

    fn create_stream<T, F>(
        emulator: Arc<Mutex<Emulator>>,
        nemu_config: Arc<Mutex<NemuConfig>>,
        device: &cpal::Device,
        config: cpal::StreamConfig,
        mut on_display_update: F,
    ) -> cpal::Stream
    where
        T: cpal::SizedSample + cpal::FromSample<f32>,
        F: FnMut(Display) + Send + 'static,
    {
        let sample_rate = config.sample_rate as f32;
        let channels = config.channels as usize;

        use cpal::ErrorKind;
        let err_fn = |err: cpal::Error| match err.kind() {
            ErrorKind::DeviceChanged | ErrorKind::Xrun | ErrorKind::RealtimeDenied => {
                eprintln!("{}", err);
            }
            _ => eprintln!("Stream error: {}", err),
        };

        device
            .build_output_stream(
                config,
                move |data: &mut [T], _: &cpal::OutputCallbackInfo| {
                    Self::fill_samples(
                        data,
                        channels,
                        sample_rate,
                        &emulator,
                        &nemu_config,
                        &mut on_display_update,
                    )
                },
                err_fn,
                None,
            )
            .unwrap()
    }

    fn fill_samples<T, F>(
        output: &mut [T],
        channels: usize,
        sample_rate: f32,
        emulator: &Mutex<Emulator>,
        nemu_config: &Mutex<NemuConfig>,
        on_display_update: &mut F,
    ) where
        T: cpal::Sample + cpal::FromSample<f32>,
        F: FnMut(Display),
    {
        let mut emulator = emulator.lock().unwrap();
        let nemu_config = nemu_config.lock().unwrap();

        for frame in output.chunks_mut(channels) {
            let value: T = T::from_sample_(Self::next_sample(
                sample_rate,
                &mut emulator,
                &nemu_config,
                on_display_update,
            ));
            for sample in frame.iter_mut() {
                *sample = value;
            }
        }
    }

    #[inline]
    fn next_sample<F>(
        sample_rate: f32,
        emulator: &mut Emulator,
        nemu_config: &NemuConfig,
        on_display_update: &mut F,
    ) -> f32
    where
        F: FnMut(Display),
    {
        let to_do_cpu_cycles = (CPU_CLOCK / sample_rate) as u64;

        let mut executed_cycles = 0;
        while executed_cycles < to_do_cpu_cycles {
            let cpu_cycles = emulator.tick_cpu();
            for _ in 0..cpu_cycles {
                emulator.tick_apu();
            }
            for _ in 0..3 * cpu_cycles {
                let update_display = emulator.tick_ppu(nemu_config);
                if update_display {
                    on_display_update(emulator.display().clone())
                }
            }

            executed_cycles += cpu_cycles;
        }

        emulator.sample_apu()
    }
}
