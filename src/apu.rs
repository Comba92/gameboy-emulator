use bitfields::bitfield;

use crate::GbEmulator;

fn digital_to_analog(sample: u8) -> f32 {
    // If a DAC is enabled, the digital range $0 to $F is linearly translated to the analog range -1 to 1, in arbitrary units. Importantly, the slope is negative: “digital 0” maps to “analog 1”, not “analog -1”.
    // If a DAC is disabled, it fades to an analog value of 0, which corresponds to “digital 7.5”. The nature of this fade is not entirely deterministic and varies between models.

    -(sample as f32 / 7.5) + 1.0
}

#[derive(Default)]
struct Length {
    enable: bool,
    count: u8,
    initial: u8,
}
impl Length {
    pub fn new(initial: u8) -> Self {
        Self {
            initial,
            count: 0,
            enable: false,
        }
    }
    pub fn tick(&mut self, enabled: &mut bool) {
        if !self.enable || self.count == 0 {
            return;
        }

        self.count -= 1;
        if self.count == 0 {
            *enabled = false;
        }
    }

    pub fn trigger(&mut self, target: u8) {
        if self.count == 0 {
            self.count = target - self.initial;
        }
    }
}

#[bitfield(u8)]
pub struct EnvFlags {
    #[bits(3)]
    sweep_pace: u8,
    direction: bool,
    #[bits(4)]
    vol_initial: u8,
}

#[derive(Default)]
struct Envelope {
    flags: EnvFlags,
    vol: u8,
    count: u8,
}
impl Envelope {
    pub fn tick(&mut self) {
        if self.flags.sweep_pace() == 0 || self.count == 0 {
            return;
        }

        self.count -= 1;
        if self.count == 0 {
            self.count = self.flags.sweep_pace();

            if self.flags.direction() && self.vol < 0xf {
                self.vol += 1;
            } else if self.vol > 0 {
                self.vol -= 1;
            }
        }
    }

    fn trigger(&mut self) {
        self.count = self.flags.sweep_pace();
        self.vol = self.flags.vol_initial();

        self.count = if self.flags.sweep_pace() > 0 {
            self.flags.sweep_pace()
        } else {
            8
        };
    }
}

trait Channel {
    fn is_enabled(&self) -> bool;
    fn is_dac_enabled(&self) -> bool;
    fn tick_div(&mut self);
    fn tick_len(&mut self);
    fn trigger(&mut self, cond: bool);
    fn digital_output(&self) -> u8;
    fn dac_output(&self, panning: bool) -> f32 {
        if self.is_dac_enabled() && panning {
            digital_to_analog(self.digital_output())
        } else {
            0.0
        }
    }
}

#[bitfield(u8)]
pub struct SweepFlags {
    #[bits(3)]
    step: u8,
    direction: bool,
    #[bits(3)]
    pace: u8,
    _unused: bool,
}

#[derive(Default)]
pub struct Pulse {
    enabled: bool,
    dac_enable: bool,

    has_sweep: bool,
    sweep_count: u8,
    sweep_enable: bool,
    sweep_period: u16,
    sweep: SweepFlags,

    len: Length,
    env: Envelope,

    period: u16,
    div: u16,
    wave_duty: u8,
    wave_pos: u8,
}
impl Channel for Pulse {
    fn is_enabled(&self) -> bool {
        // TODO: cache this
        self.dac_enable && self.enabled
    }

    fn is_dac_enabled(&self) -> bool {
        self.dac_enable
    }

    fn tick_div(&mut self) {
        if !self.enabled {
            return;
        }

        if self.div == 0 {
            self.div = 4 * (2048 - self.period);
            self.wave_pos = (self.wave_pos + 1) % 8;
        } else {
            self.div -= 1
        }
    }

    fn tick_len(&mut self) {
        self.len.tick(&mut self.enabled);
    }

    fn trigger(&mut self, cond: bool) {
        if !cond || !self.dac_enable {
            return;
        }

        self.enabled = true;
        self.len.trigger(64);
        self.div = 4 * (2048 - self.period);
        self.env.trigger();

        self.sweep_period = self.period;
        self.sweep_count = if self.sweep.pace() > 0 {
            self.sweep.pace()
        } else {
            8
        };
        self.sweep_enable = self.sweep.pace() > 0 || self.sweep.step() > 0;

        if self.sweep.step() > 0 {
            self.update_sweep_period();
        }
    }

    fn digital_output(&self) -> u8 {
        // TODO: cache
        self.env.vol * Self::TABLE[self.wave_duty as usize][self.wave_pos as usize]
    }
}

impl Pulse {
    pub const TABLE: [[u8; 8]; 4] = [
        [1, 1, 1, 1, 1, 1, 1, 0],
        [0, 1, 1, 1, 1, 1, 1, 0],
        [0, 1, 1, 1, 1, 0, 0, 0],
        [1, 0, 0, 0, 0, 0, 0, 1],
    ];

    pub fn new(has_sweep: bool) -> Self {
        Self {
            has_sweep,
            len: Length::new(64),
            period: 2 * 2048,
            ..Default::default()
        }
    }

    fn tick_env(&mut self) {
        self.env.tick();
    }

    fn update_sweep_period(&mut self) -> u16 {
        if !self.has_sweep {
            return 0;
        }

        let res = self.sweep_period >> self.sweep.step();
        let res = if self.sweep.direction() {
            self.sweep_period - res
        } else {
            self.sweep_period + res
        };

        if res > 2047 {
            self.enabled = false;
        }

        res
    }

    fn tick_sweep(&mut self) {
        if !self.sweep_enable {
            return;
        }

        if self.sweep_count == 0 {
            self.sweep_count = if self.sweep.pace() > 0 {
                self.sweep.pace()
            } else {
                8
            };

            if self.sweep.pace() > 0 {
                let period = self.update_sweep_period();
                if period <= 2047 && self.sweep.step() > 0 {
                    self.sweep_period = period;
                    self.period = period;
                    self.update_sweep_period();
                }
            }
        } else {
            self.sweep_count -= 1;
        }
    }
}

#[bitfield(u8)]
struct NoiseFlags {
    #[bits(3)]
    clock_div: u8,
    lsfr_width: bool,
    #[bits(4)]
    clock_shift: u8,
}

#[derive(Default)]
struct Noise {
    enabled: bool,
    dac_enable: bool,

    len: Length,
    env: Envelope,

    lfsr: u16,
    div: u8,
    rnd: NoiseFlags,
}
impl Channel for Noise {
    fn is_enabled(&self) -> bool {
        self.enabled && self.dac_enable
    }

    fn is_dac_enabled(&self) -> bool {
        self.dac_enable
    }

    fn tick_div(&mut self) {
        if !self.enabled || self.rnd.clock_shift() >= 14 {
            return;
        }

        if self.div == 0 {
            self.div = if self.rnd.clock_div() == 0 {
                8 << self.rnd.clock_shift()
            } else {
                (16 * self.rnd.clock_div()) << self.rnd.clock_shift()
            };

            let xor = (self.lfsr & 0b01) ^ ((self.lfsr & 0b10) >> 1);
            self.lfsr = (self.lfsr & !0x8000) | (xor << 15);
            if self.rnd.lsfr_width() {
                self.lfsr = (self.lfsr & !0x80) | (xor << 7);
            }

            self.lfsr >>= 1;
        } else {
            self.div -= 1;
        }
    }

    fn tick_len(&mut self) {
        self.len.tick(&mut self.enabled);
    }

    fn trigger(&mut self, cond: bool) {
        if !cond || !self.dac_enable {
            return;
        }

        self.enabled = true;
        self.len.trigger(64);
        self.env.trigger();
        self.lfsr = 0xffff;
    }

    fn digital_output(&self) -> u8 {
        self.env.vol * ((self.lfsr & 1) as u8 ^ 1)
    }
}
impl Noise {
    pub fn new() -> Self {
        Self {
            len: Length::new(64),
            lfsr: 0xffff,
            ..Default::default()
        }
    }

    fn tick_env(&mut self) {
        self.env.tick();
    }
}

#[derive(Default)]
struct Wave {
    enabled: bool,
    dac_enable: bool,

    len: Length,

    vol: u8,
    div: u16,
    period: u16,

    pos: u8,
    sample: u8,
    pub ram: [u8; 16],
}
impl Wave {
    pub fn new() -> Self {
        Self {
            len: Length::new(255),
            period: 2 * 2048,
            enabled: true, // TODO: why is this the case?
            ..Default::default()
        }
    }
}
impl Channel for Wave {
    fn is_enabled(&self) -> bool {
        self.enabled && self.dac_enable
    }

    fn is_dac_enabled(&self) -> bool {
        self.dac_enable
    }

    fn tick_div(&mut self) {
        if !self.enabled {
            return;
        }

        if self.div == 0 {
            self.div = 2 * (2048 - self.period);
            self.sample = self.ram[self.pos as usize / 2];
            self.pos = (self.pos + 1) % 32;
        } else {
            self.div -= 1;
        }
    }

    fn tick_len(&mut self) {
        self.len.tick(&mut self.enabled);
    }

    fn trigger(&mut self, cond: bool) {
        if !cond || !self.dac_enable {
            return;
        }

        self.enabled = true;
        self.len.trigger(255);
        self.div = 2 * (2048 - self.period);
        self.pos = 0;
    }

    fn digital_output(&self) -> u8 {
        let sample = if self.pos % 2 == 0 {
            self.sample >> 4
        } else {
            self.sample & 0xf
        };

        if self.vol > 0 {
            sample >> (self.vol - 1)
        } else {
            0
        }
    }
}

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
    vol_right: u8,
    vin_right: bool,
    #[bits(3)]
    vol_left: u8,
    vin_left: bool,
}

#[derive(Default)]
pub struct Apu {
    pub master_enable: bool,

    pub pan: Panning,
    pub vol: Volume,
    vol_left: f32,
    vol_right: f32,
    frame_count: u8,

    pub p1: Pulse,
    pub p2: Pulse,
    w: Wave,
    n: Noise,
}
impl Apu {
    pub fn new() -> Self {
        Self {
            p1: Pulse::new(true),
            p2: Pulse::new(false),
            n: Noise::new(),
            w: Wave::new(),
            ..Default::default()
        }
    }

    pub fn nr50_write(&mut self, val: u8) {
        if self.master_enable {
            self.vol = Volume::from_bits_with_defaults(val);
            self.vol_left = (self.vol.vol_left() as f32 + 1.0) / 8.0;
            self.vol_right = (self.vol.vol_right() as f32 + 1.0) / 8.0;
        }
    }

    pub fn nr52_read(&self) -> u8 {
        let mut res = 0;
        res |= (self.p1.is_enabled() as u8) << 0;
        res |= (self.p2.is_enabled() as u8) << 1;
        res |= (self.w.is_enabled() as u8) << 2;
        res |= (self.n.is_enabled() as u8) << 3;
        res |= (self.master_enable as u8) << 7;

        res | 0x70
    }

    pub fn nr52_write(&mut self, val: u8) {
        self.master_enable = val & 0x80 != 0;

        if !self.master_enable {
            *self = Apu::new();
        }
    }

    pub fn nr10_read(&self, pulse: fn(&Apu) -> &Pulse) -> u8 {
        let p = pulse(self);
        p.sweep.into_bits()
    }

    pub fn nr10_write(&mut self, val: u8, pulse: fn(&mut Apu) -> &mut Pulse) {
        if self.master_enable {
            let p = pulse(self);
            p.sweep = SweepFlags::from_bits_with_defaults(val);
        }
    }

    pub fn nr11_read(&self, pulse: fn(&Apu) -> &Pulse) -> u8 {
        let p = pulse(self);
        let mut res = 0;
        res |= p.len.initial;
        res |= p.wave_duty << 6;
        res
    }

    pub fn nr11_write(&mut self, val: u8, pulse: fn(&mut Apu) -> &mut Pulse) {
        if self.master_enable {
            let p = pulse(self);
            p.wave_duty = val >> 6;
            p.len.initial = val & 0x3f;
        }
    }

    pub fn nr12_read(&self, pulse: fn(&Apu) -> &Pulse) -> u8 {
        let p = pulse(self);
        p.env.flags.into_bits()
    }

    pub fn nr12_write(&mut self, val: u8, pulse: fn(&mut Apu) -> &mut Pulse) {
        if self.master_enable {
            let p = pulse(self);
            p.env.flags = EnvFlags::from_bits_with_defaults(val);
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
        ((p.len.enable as u8) << 6) | 0xbf
    }

    pub fn nr14_write(&mut self, val: u8, pulse: fn(&mut Apu) -> &mut Pulse) {
        if self.master_enable {
            let p = pulse(self);
            p.period = (p.period & 0x00ff) | ((val as u16 & 0x7) << 8);
            p.len.enable = val & 0x40 != 0;
            let trigger = val & 0x80 != 0;
            p.trigger(trigger);
        }
    }

    pub fn nr30_read(&mut self) -> u8 {
        ((self.w.dac_enable as u8) << 7) | 0x3f
    }

    pub fn nr30_write(&mut self, val: u8) {
        if self.master_enable {
            self.w.dac_enable = val & 0x80 != 0;
        }
    }

    pub fn nr31_write(&mut self, val: u8) {
        if self.master_enable {
            self.w.len.initial = val;
        }
    }

    pub fn nr32_read(&mut self) -> u8 {
        ((self.w.vol as u8) << 5) | 0x9f
    }

    pub fn nr32_write(&mut self, val: u8) {
        if self.master_enable {
            self.w.vol = (val >> 5) & 0x3;
        }
    }

    pub fn nr33_write(&mut self, val: u8) {
        if self.master_enable {
            self.w.period = (self.w.period & 0xff00) | (val as u16);
        }
    }

    pub fn nr34_read(&self) -> u8 {
        ((self.w.len.enable as u8) << 6) | 0xbf
    }

    pub fn nr34_write(&mut self, val: u8) {
        if self.master_enable {
            self.w.period = (self.w.period & 0x00ff) | ((val as u16 & 0x7) << 8);
            self.w.len.enable = val & 0x40 != 0;
            let trigger = val & 0x80 != 0;
            self.w.trigger(trigger);
        }
    }

    pub fn wave_read(&self, addr: u16) -> u8 {
        if self.w.enabled {
            self.w.ram[addr as usize % 16]
        } else {
            0xff
        }
    }

    pub fn wave_write(&mut self, addr: u16, val: u8) {
        if self.w.enabled {
            self.w.ram[addr as usize % 16] = val;
        }
    }

    pub fn nr41_write(&mut self, val: u8) {
        if self.master_enable {
            self.n.len.initial = val & 0x3f;
        }
    }

    pub fn nr42_read(&self) -> u8 {
        self.n.env.flags.into_bits()
    }

    pub fn nr42_write(&mut self, val: u8) {
        if self.master_enable {
            self.n.env.flags = EnvFlags::from_bits_with_defaults(val);
            self.n.dac_enable = val & 0xf8 != 0;
        }
    }

    pub fn nr43_read(&self) -> u8 {
        self.n.rnd.into_bits()
    }

    pub fn nr43_write(&mut self, val: u8) {
        if self.master_enable {
            self.n.rnd = NoiseFlags::from_bits_with_defaults(val);
        }
    }

    pub fn nr44_read(&self) -> u8 {
        ((self.n.len.enable as u8) << 6) | 0xbf
    }

    pub fn nr44_write(&mut self, val: u8) {
        if self.master_enable {
            self.n.len.enable = val & 0x40 != 0;
            let trigger = val & 0x80 != 0;
            self.n.trigger(trigger);
        }
    }

    pub fn mix_channels(&mut self) -> (f32, f32) {
        let p1_left = self.p1.dac_output(self.pan.ch1_left());
        let p2_left = self.p2.dac_output(self.pan.ch2_left());
        let w_left = self.w.dac_output(self.pan.ch3_left());
        let n_left = self.n.dac_output(self.pan.ch4_left());

        let p1_right = self.p1.dac_output(self.pan.ch1_right());
        let p2_right = self.p2.dac_output(self.pan.ch2_right());
        let w_right = self.w.dac_output(self.pan.ch3_right());
        let n_right = self.n.dac_output(self.pan.ch4_right());

        let left = (p1_left + p2_left + w_left + n_left) / 4.0 * self.vol_left;
        let right = (p1_right + p2_right + w_right + n_right) / 4.0 * self.vol_right;

        (left, right)
    }
}

impl GbEmulator {
    pub(crate) fn apu_step(&mut self) {
        let apu = &mut self.apu;
        if !apu.master_enable {
            return;
        }

        // Pulses are clocked at 1048576 Hz
        // 4194304Hz / 1048576Hz = 4Tcycles = 1Mcycle
        apu.p1.tick_div();
        apu.p2.tick_div();

        // Noise is clocked at 262144 Hz
        // 4194304Hz / 262144Hz = 16Tcycles = 4Mcycle
        // But the period reload does something that makes it clock every 1Mcycle...
        apu.n.tick_div();
        apu.w.tick_div();

        let (left, right) = apu.mix_channels();
        if let Some((left, right)) = self.output.resampler.add_sample(left, right) {
            self.output.audiobuf.push(left);
            self.output.audiobuf.push(right);
        }
    }

    pub(crate) fn apu_frame_step(&mut self) {
        let apu = &mut self.apu;
        if !apu.master_enable {
            return;
        }

        // Frame sequencer: 512 Hz
        // 4194304Hz / 512Hz = 8192 Tcycles = 2048 Mcycles

        if apu.frame_count % 2 == 0 {
            apu.p1.tick_len();
            apu.p2.tick_len();
            apu.w.tick_len();
            apu.n.tick_len();
        }

        if apu.frame_count == 2 || apu.frame_count == 6 {
            apu.p1.tick_sweep();
        }

        if apu.frame_count == 7 {
            apu.p1.tick_env();
            apu.p2.tick_env();
            apu.n.tick_env();
        }

        apu.frame_count = (apu.frame_count + 1) % 8;
    }
}
