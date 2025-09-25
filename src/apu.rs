use blip_buf::BlipBuf;
use crate::emu::Emu;

#[derive(Default)]
struct Length {
  enabled: bool,
  count: u8,
  initial: u8,
}
impl Length {
  fn tick(&mut self) {
    if self.enabled && self.count < 64 {
      self.count += 1;
      if self.count == 64 {
        self.count = self.initial;
        self.enabled = false;
      }
    }
  }
}

#[derive(Default)]
struct Env {
  enabled: bool,
  initial: u8,
  direction: bool,

  volume: u8,
  pace: u8,
  count: u8,
}
impl Env {
  fn tick(&mut self) {
    if self.enabled && self.count < self.pace {
      self.count += 1;
      if self.count == self.pace {
        self.count = 0;

        if self.direction { 
          self.volume = self.volume.saturating_add(1); 
        } else { 
          self.volume = self.volume.saturating_sub(1);
        };
      }
    }
  }
}

#[derive(Default)]
struct Pulse {
  dac_enabled: bool,
  len: Length,
  env: Env,
  period: u16,
  divider: u16,

  duty_cycle: u8,
  duty_step: u8,
}
impl Pulse {
  const WAVES: [[u8; 8]; 4] = [
    [1, 1, 1, 1, 1, 1, 1, 0],
    [0, 1, 1, 1, 1, 1, 1, 0],
    [0, 1, 1, 1, 1, 0, 0, 0],
    [1, 0, 0, 0, 0, 0, 0, 1]
  ];

  fn read_NRx1(&mut self) -> u8 {
    let mut res = 0;
    res |= self.len.initial;
    res |= self.duty_cycle << 6;
    res
  }

  fn write_NRx1(&mut self, val: u8) {
    self.len.initial = val & 0x3f;
    self.duty_cycle = val >> 6;
  }

  
  fn read_NRx2(&mut self) -> u8 {
    let mut res = 0;
    res |= self.env.initial << 4;
    res |= (self.env.direction as u8) << 3;
    res |= self.env.pace;
    res
  }

  fn write_NRx2(&mut self, val: u8) {
    // Setting bits 3-7 of this register all to 0 (initial volume = 0, envelope = decreasing) turns the DAC off
    self.env.initial = val >> 4;
    self.env.direction = val & 0x8 > 0;
    self.env.pace = val & 0x7;
    self.env.enabled = self.env.pace > 0;

    self.dac_enabled = self.env.initial > 0 || self.env.direction;
  }

  fn write_NRx3(&mut self, val: u8) {
    self.period = (self.period & 0xff00) | val as u16;
  }
  
  fn read_NRx4(&mut self) -> u8 {
    (self.len.enabled as u8) << 6
  }

  fn write_NRx4(&mut self, val: u8) {
    self.trigger(val & 0x80 > 0);
    self.len.enabled = val & 0x40 > 0;
    self.period = (self.period & 0x00ff) | ((val as u16 & 0x7) << 8);
  }

  fn trigger(&mut self, cond: bool) {
    // If the channel’s DAC is off, the channel will not turn on.
    if !self.dac_enabled || !cond { return; }

    self.len.enabled = true;
    self.divider = self.period;
    self.env.count = 0;
    self.env.volume = self.env.initial;
  }

  fn tick(&mut self) {
    if self.is_enabled() && self.divider < 2048 {
      self.divider += 1;
      if self.divider == 2048 {
        self.divider = self.period;
        self.duty_step = (self.duty_step + 1) % 8;
      }
    }
  }

  fn is_enabled(&self) -> bool {
    self.len.enabled && self.dac_enabled
  }

  fn sample(&self) -> f64 {
    // The digital value produced by the generator, which ranges between $0 and $F (0 and 15), is linearly translated by the DAC into an analog1 value between -1 and 1 (the unit is arbitrary).
    if self.is_enabled() {
      let sample = self.env.volume * Self::WAVES[self.duty_cycle as usize][self.duty_step as usize];
      (sample as f64 / 7.5) / 1.0
    } else {
      1.0
    }
  }
}

// 4_194_304 are tcycles in a second
// 1_048_576 are mcycles in a second

pub(crate) struct AudioBuf(pub BlipBuf);
// TODO: make sample rate configurable
impl Default for AudioBuf {
  fn default() -> Self {
    let mut blip = BlipBuf::new(48000);
    blip.set_rates(4194304.0 / 4.0, 48000.0);
    Self(blip)
  }
}


#[derive(Default)]
pub struct Apu {
  enabled: bool,
  panning: u8,
  left_volume: u8,
  right_volume: u8,

  p0: Pulse,
  p1: Pulse,

  prev_sample: f64,
  pub(crate) blip: AudioBuf,
  pub frame_cycles: usize,
  div_cycles: usize,
}

impl Emu {
  pub(crate) fn apu_read(&mut self, addr: usize) -> u8 {
    let apu = &mut self.apu;

    match addr {
      0xff26 => {
        let mut res = 0;
        res |= (apu.enabled as u8) << 7;
        res |= apu.p0.len.enabled as u8;
        res |= (apu.p1.len.enabled as u8) << 1;
        res
      }
      0xff25 => apu.panning, // TODO
      0xff24 => {
        let mut res = 0;
        res |= apu.left_volume << 4;
        res |= apu.right_volume;
        res
      },

      0xff10 => 0, // TODO
      0xff11 => apu.p0.read_NRx1(),
      0xff12 => apu.p0.read_NRx2(),
      0xff14 => apu.p0.read_NRx4(),

      0xff16 => apu.p1.read_NRx1(),
      0xff17 => apu.p1.read_NRx2(),
      0xff19 => apu.p1.read_NRx4(),

      _ => 0xff
    }
  }

  pub(crate) fn apu_write(&mut self, addr: usize, val: u8) {
    let apu = &mut self.apu;

    match addr {
      0xff26 => {
        apu.enabled = val & 0x80 > 0
        // TODO: enable/disable logic
      }
      0xff25 => apu.panning = val, // TODO
      0xff24 => {
        apu.left_volume = (val >> 4) & 0x7;
        apu.right_volume = val & 0x7;
      },

      0xff11 => apu.p0.write_NRx1(val),
      0xff12 => apu.p0.write_NRx2(val),
      0xff13 => apu.p0.write_NRx3(val),
      0xff14 => apu.p0.write_NRx4(val),

      0xff16 => apu.p1.write_NRx1(val),
      0xff17 => apu.p1.write_NRx2(val),
      0xff18 => apu.p1.write_NRx3(val),
      0xff19 => apu.p1.write_NRx4(val),
      _ => {}
    }
  }


  pub(crate) fn apu_div_step(&mut self) {
    let apu = &mut self.apu;

    apu.div_cycles += 1;
    if apu.div_cycles % 2 == 0 {
      apu.p0.len.tick();
      apu.p1.len.tick();
    }

    if apu.div_cycles % 8 == 0 {
      apu.p0.env.tick();
      apu.p1.env.tick();
    }
  }

  pub(crate) fn apu_step(&mut self) {
    let apu = &mut self.apu;

    // The pulse channels’ period dividers are clocked at 1048576 Hz, once per four dots
    // this means they are clocked each mcycle.
    apu.p0.tick();
    apu.p1.tick();

    let sample = apu.p0.sample() as f64 + apu.p1.sample() as f64;
    let delta = 0.05 * (sample / 2.0) - apu.prev_sample;

    apu.blip.0.add_delta(apu.frame_cycles as u32, delta as i32);
    apu.prev_sample = sample;
    apu.frame_cycles += 1;
  }
}