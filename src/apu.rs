use bitfields::bitfield;

use crate::GbEmulator;

#[bitfield(u8)]
pub struct Sweep {
    #[bits(3)]
    step: u8,
    direction: bool,
    #[bits(3)]
    pace: u8,
    _unused: bool,
}

#[bitfield(u8)]
pub struct Envelope {
    #[bits(3)]
    sweep_pace: u8,
    direction: bool,
    #[bits(4)]
    vol_initial: u8,
}

#[derive(Default)]
pub struct Pulse {
    enabled: bool,
    dac_enable: bool,

    sweep_count: u8,
    sweep_enable: bool,
    sweep_period: u16,
    pub sweep: Sweep,

    vol: u8,
    env_count: u8,
    pub env: Envelope,

    period: u16,
    div: u16,
    wave_duty: u8,
    wave_pos: u8,

    len_initial: u8,
    len_enable: bool,
    len_count: u8,
}
impl Pulse {
    pub const TABLE: [[u8; 8]; 4] = [
        [1, 1, 1, 1, 1, 1, 1, 0],
        [0, 1, 1, 1, 1, 1, 1, 0],
        [0, 1, 1, 1, 1, 0, 0, 0],
        [1, 0, 0, 0, 0, 0, 0, 1],
    ];

    fn is_enabled(&self) -> bool {
        self.dac_enable && self.enabled
    }

    fn tick_div(&mut self) {
        self.div += 1;
        if self.div >= 2048 {
            self.div = self.period;
            self.wave_pos = (self.wave_pos + 1) % 8;
        }
    }

    fn tick_len(&mut self) {
        if self.len_enable {
            self.len_count += 1;
            if self.len_count >= 64 {
                self.enabled = false;
            }
        }
    }

    fn tick_env(&mut self) {
        if self.env.sweep_pace() == 0 {
            return;
        }

        if self.env_count == 0 {
            self.env_count = self.env.sweep_pace();

            if self.env.direction() && self.vol < 0xf {
                self.vol += 1;
            } else if self.vol > 0 {
                self.vol -= 1;
            }
        } else {
            self.env_count -= 1;
        }
    }

    fn tick_sweep(&mut self) {
        if !self.sweep_enable { return; }

        if self.sweep_count == 0 {
            self.sweep_count = if self.sweep.pace() > 0 { self.sweep.pace() } else { 8 };

        } else {
            self.sweep_count -= 1;
        }
    }

    fn trigger(&mut self, cond: bool) {
        if !cond {
            return;
        }

        self.enabled = true;
        self.len_count = self.len_initial;
        self.div = self.period;
        self.env_count = self.env.sweep_pace();
        self.vol = self.env.vol_initial();
        self.sweep_period = self.period;
        self.sweep_count = self.sweep.pace();
        self.sweep_enable = self.sweep.pace() != 0 || self.sweep.step() != 0;
    }

    fn digital_output(&self) -> u8 {
        // TODO: cache this
        if self.is_enabled() {
            self.vol * Self::TABLE[self.wave_duty as usize][self.wave_pos as usize]
        } else {
            0
        }
    }

    fn dac_output(&self, panning: bool) -> f32 {
        // If a DAC is enabled, the digital range $0 to $F is linearly translated to the analog range -1 to 1, in arbitrary units. Importantly, the slope is negative: “digital 0” maps to “analog 1”, not “analog -1”.
        // If a DAC is disabled, it fades to an analog value of 0, which corresponds to “digital 7.5”. The nature of this fade is not entirely deterministic and varies between models.
        if self.dac_enable && panning {
            -(self.digital_output() as f32 / 7.5) + 1.0
        } else {
            0.0
        }
    }
}

#[derive(Default)]
struct Noise {}

impl Noise {}

#[bitfield(u8)]
pub struct Panning {
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
// CAREFUL: volume is actually volume+1 (it is never 0)
pub struct Volume {
    #[bits(3)]
    right_vol: u8,
    vin_right: bool,
    #[bits(3)]
    left_vol: u8,
    vin_left: bool,
}

#[derive(Default)]
pub struct Apu {
    pub master_enable: bool,

    pub pan: Panning,
    pub vol: Volume,

    pub p1: Pulse,
    pub p2: Pulse,
    noise: Noise,
}
impl Apu {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn nr52_read(&mut self) -> u8 {
        let mut res = 0;
        res |= (self.master_enable as u8) << 7;
        res |= (self.p1.is_enabled() as u8) << 0;
        res |= (self.p2.is_enabled() as u8) << 1;

        res | 0x70
    }

    pub fn nr52_write(&mut self, val: u8) {
        self.master_enable = val & 0x80 != 0;
        // TODO: clear all apu registers
    }

    pub fn nr10_write(&mut self, val: u8, pulse: fn(&mut Apu) -> &mut Pulse) {
        if self.master_enable {
            let p = pulse(self);
            p.sweep = Sweep::from_bits_with_defaults(val);
        }
    }

    pub fn nr11_read(&self, pulse: fn(&Apu) -> &Pulse) -> u8 {
        let p = pulse(self);
        let mut res = 0;
        res |= p.len_initial;
        res |= p.wave_duty << 6;
        res
    }

    pub fn nr11_write(&mut self, val: u8, pulse: fn(&mut Apu) -> &mut Pulse) {
        if self.master_enable {
            let p = pulse(self);
            p.wave_duty = val >> 6;
            p.len_initial = val & 0x3f;
        }
    }

    pub fn nr12_write(&mut self, val: u8, pulse: fn(&mut Apu) -> &mut Pulse) {
        if self.master_enable {
            let p = pulse(self);
            p.env = Envelope::from_bits_with_defaults(val);
            p.dac_enable = val & 0xf8 != 0;
        }
    }

    pub fn nr13_write(&mut self, val: u8, pulse: fn(&mut Apu) -> &mut Pulse) {
        if self.master_enable {
            let p = pulse(self);
            p.period = (p.period & 0xff00) | (val as u16);
        }
    }

    pub fn nr14_read(&self, pulse: fn(&Apu) -> &Pulse) -> u8 {
        let p = pulse(self);
        ((p.len_enable as u8) << 6) | 0xbf
    }

    pub fn nr14_write(&mut self, val: u8, pulse: fn(&mut Apu) -> &mut Pulse) {
        if self.master_enable {
            let p = pulse(self);
            p.period = (p.period & 0x00ff) | ((val as u16 & 0x7) << 8);
            p.len_enable = val & 0x40 != 0;
            let trigger = val & 0x80 != 0;
            p.trigger(trigger);
        }
    }

    pub fn mix_channels(&mut self) -> (f32, f32) {
        let p1_left = self.p1.dac_output(self.pan.ch1_left());
        let p2_left = self.p2.dac_output(self.pan.ch2_left());

        let p1_right = self.p1.dac_output(self.pan.ch1_right());
        let p2_right = self.p2.dac_output(self.pan.ch2_right());

        let left = (p1_left + p2_left) / 4.0 * (self.vol.left_vol() as f32 + 1.0);
        let right = (p1_right + p2_right) / 4.0 * (self.vol.right_vol() as f32 + 1.0);

        (left, right)
    }
}

impl GbEmulator {
    pub(crate) fn apu_step(&mut self) {
        let cycles = self.cpu.mcycles;
        let apu = &mut self.apu;

        // Periods are clocked at 1048576 Hz
        // 4194304Hz / 1048576Hz = 4Tcycles = 1Mcycle
        apu.p1.tick_div();
        apu.p2.tick_div();

        // This should be clocked by timer DIV
        if cycles % 2048 == 0 {
            if cycles % 16384 == 0 {
                // tick env
                apu.p1.tick_env();
                apu.p2.tick_env();
            }

            if cycles % 4096 == 0 {
                // tick length
                apu.p1.tick_len();
                apu.p2.tick_len();
            }

            if cycles % 8192 == 0 {
                // tick sweep
                apu.p1.tick_sweep();
            }
        }

        let (left, right) = apu.mix_channels();
        if let Some((left, right)) = self.output.resampler.add_sample(left, right) {
            self.output.audiobuf.push(left);
            self.output.audiobuf.push(right);
        }
    }
}
