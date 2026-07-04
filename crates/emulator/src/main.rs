use std::time::Duration;

use cpal::{
    traits::{DeviceTrait, HostTrait, StreamTrait as _},
    Device, Error, ErrorKind, FromSample, OutputCallbackInfo, Sample, SizedSample, StreamConfig,
    I24, U24,
};

fn main() {
    let host = cpal::default_host();
    let device = host.default_output_device().unwrap();
    let config = device.default_output_config().unwrap();

    match config.sample_format() {
        cpal::SampleFormat::I8 => run::<i8>(&device, config.into()),
        cpal::SampleFormat::I16 => run::<i16>(&device, config.into()),
        cpal::SampleFormat::I24 => run::<I24>(&device, config.into()),
        cpal::SampleFormat::I32 => run::<i32>(&device, config.into()),
        cpal::SampleFormat::I64 => run::<i64>(&device, config.into()),
        cpal::SampleFormat::U8 => run::<u8>(&device, config.into()),
        cpal::SampleFormat::U16 => run::<u16>(&device, config.into()),
        cpal::SampleFormat::U24 => run::<U24>(&device, config.into()),
        cpal::SampleFormat::U32 => run::<u32>(&device, config.into()),
        cpal::SampleFormat::U64 => run::<u64>(&device, config.into()),
        cpal::SampleFormat::F32 => run::<f32>(&device, config.into()),
        cpal::SampleFormat::F64 => run::<f64>(&device, config.into()),
        unsupported => panic!("unsupported sample format '{}'", unsupported),
    }
}

fn run<T>(device: &Device, config: StreamConfig)
where
    T: SizedSample + FromSample<f32>,
{
    let sample_rate = config.sample_rate as f32;
    let channels = config.channels as usize;

    let mut sample_clock = 0f32;
    let mut next_value = move || {
        sample_clock = (sample_clock + 1.0) % sample_rate;
        (sample_clock * 2000.0 * 2.0 * std::f32::consts::PI / sample_rate).sin()
    };

    let err_fn = |err: Error| match err.kind() {
        ErrorKind::DeviceChanged | ErrorKind::Xrun | ErrorKind::RealtimeDenied => {
            eprintln!("{}", err);
        }
        _ => eprintln!("Stream error: {}", err),
    };

    let stream = device
        .build_output_stream(
            config,
            move |data: &mut [T], _: &OutputCallbackInfo| {
                write_data(data, channels, &mut next_value)
            },
            err_fn,
            None,
        )
        .unwrap();

    stream.play().unwrap();

    std::thread::sleep(Duration::from_millis(1000));
}

fn write_data<T>(output: &mut [T], channels: usize, next_sample: &mut dyn FnMut() -> f32)
where
    T: Sample + FromSample<f32>,
{
    for frame in output.chunks_mut(channels) {
        let value: T = T::from_sample_(next_sample());
        for sample in frame.iter_mut() {
            *sample = value;
        }
    }
}
