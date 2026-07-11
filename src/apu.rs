use bitfields::bitfield;

use crate::GbEmulator;

#[bitfield(u8)]
struct Sweep {
    #[bits(3)]
    step: u8,
    direction: bool,
    #[bits(3)]
    pace: u8,
    _unused: bool,
}

#[bitfield(u8)]
struct Envelope {
    #[bits(3)]
    sweep_pace: u8,
    direction: bool,
    #[bits(4)]
    vol_initial: u8,
}

struct Pulse {
    sweep: Sweep,
    len_initial: u8,
    wave_duty: u8,
    env: Envelope,
    period: u16,
    len_enable: bool,
}

impl Pulse {}

struct Noise {}

impl Noise {}

#[bitfield(u8)]
struct Ctrl {
    ch1_enable: bool,
    ch2_enable: bool,
    ch3_enable: bool,
    ch4_enable: bool,
    #[bits(3, default = 0x7)]
    _unused: u8,
    audio_enable: bool,
}

#[bitfield(u8)]
struct Panning {
    ch1_right: bool,
    ch2_right: bool,
    ch3_right: bool,
    ch4_right: bool,
    ch1_left: bool,
    ch2_left: bool,
    ch3_left: bool,
    ch4_left: bool,
}

#[bitfield(u8)]
struct Volume {
    #[bits(3)]
    right_vol: u8,
    vin_right: bool,
    #[bits(3)]
    left_vol: u8,
    vin_left: bool,
}

struct Apu {
    ctrl: Ctrl,
    pan: Panning,
    vol: Volume,

    p0: Pulse,
    p1: Pulse,
    noise: Noise,
}

impl GbEmulator {
    pub fn step_apu(&mut self) {}
}
