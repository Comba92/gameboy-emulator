use crate::{apu::Apu, bus::{self, Bus}, cart::CartHeader, cpu::CpuSM83, joypad::Joypad, mbc::Mbc, ppu::Ppu, serial::Serial, timer::Timer};

#[derive(Default, Debug)]
pub(crate) enum CGBMode {
  #[default] Monochrome, ColorEnhanced, ColorOnly
}

#[bitfields::bitfield(u8)]
#[derive(Clone, Copy)]
pub(crate) struct Interrupt {
  vblank: bool,
  lcd: bool,
  timer: bool,
  serial: bool,
  joypad: bool,
  #[bits(3)]
  _unused: u8,
}

pub struct Emu {
  pub cpu: CpuSM83,
  pub ppu: Ppu,
  pub bus: Bus,
  pub apu: Apu,
  pub(crate) joypad: Joypad,
  pub(crate) serial: Serial,
  pub(crate) timer: Timer,
  pub(crate) mbc: Mbc,

  pub(crate) inte: Interrupt,
  pub(crate) intf: Interrupt,

  header: CartHeader,
  pub(crate) videobuf: [u8; 160 * 144],
  audiobuf: [i16; 4 * 1024],
  pub(crate) frame_ready: bool,
}

impl Default for Emu {
  fn default() -> Self {
    Self {
      cpu: CpuSM83::new(),
      ppu: Ppu::new(),
      apu: Apu::default(),
      // reads from an absent cartridge usually return $FF
      bus: Bus::new(&vec![0xff; 32 * 1024], 0),
      joypad: Joypad::default(),
      serial: Serial::default(),
      timer: Timer::default(),
      mbc: Mbc::None,

      header: CartHeader::default(),
      inte: Interrupt::default(),
      intf: Interrupt::default(),
      videobuf: [0; 160 * 144],
      audiobuf: [0; 4 * 1024],
      frame_ready: false,
    }
  }
}

impl Emu {
  pub fn debug() -> Self {
    let mut emu = Self::default();
    emu.bus.handlers = [bus::Handler::Debug; 16];
    emu.bus.sram = vec![0; 64 * 1024];
    emu
  }

  pub fn new(bytes: &[u8]) -> Result<Self, String> {
    let header = CartHeader::parse(&bytes)?;
    let mbc = Mbc::new(header.mapper)?;

    // TODO: sram should be disabled by default, except for no mbc games
    let mut bus = Bus::new(bytes, header.ram_size);
    mbc.init(&mut bus);

    let emu = Self {
      cpu: CpuSM83::new(),
      ppu: Ppu::new(),
      apu: Apu::default(),
      bus,
      joypad: Joypad::default(),
      serial: Serial::default(),
      timer: Timer::default(),
      mbc,

      header,
      inte: Interrupt::default(),
      intf: Interrupt::default(),
      videobuf: [0; 160 * 144],
      audiobuf: [0; 4 * 1024],
      frame_ready: false,
    };

    Ok(emu)
  }

  fn dma_step(&mut self) {
    // TODO: dma bus conflicts
    if self.bus.dma.transfering {
      let src_addr = ((self.bus.dma.src as u16) << 8) | self.bus.dma.count as u16;
      let val = self.dispatch_read(src_addr);
      self.dispatch_write(0xfe00 | self.bus.dma.count as u16, val);
      self.bus.dma.count += 1;

      if self.bus.dma.count >= 160 {
        self.bus.dma.transfering = false;
      }
    }
  }

  fn timer_step(&mut self) {
    // 512 hz
    if self.timer.div % 2048 == 0 {
      self.apu_div_step();
    }

    self.timer_tima_step();
    
    // 16384 hz, first 6 bits are not visible
    self.timer.div = self.timer.div.wrapping_add(1);
  }

  pub(crate) fn timer_tima_step(&mut self) {
    // TODO: obscure TIMA overflow behaviour
    let timer = &mut self.timer;

    if timer.tac & 0x4 > 0 && timer.div as u8 & timer.clock_mask == 0 {
      if timer.tima == 0xff {
        timer.tima = timer.tma;
        self.intf.set_timer(true);
      } else {
        timer.tima += 1;
      }
    }
  }

  fn serial_step(&mut self) {
    let serial = &mut self.serial;
    
    if serial.flags.tx_enable() {
      serial.data = (serial.data << 1) | 1;
      if serial.count > 0 {
        serial.count -= 1;
      } else {
        serial.flags.set_tx_enable(false);
        self.intf.set_serial(true);
      }
    }
  }

	pub(crate) fn tick(&mut self) {
		self.cpu.mcycles += 1;
    
    self.dma_step();
    self.timer_step();
    self.serial_step();
    self.apu_step();
    
		self.ppu_step();
		self.ppu_step();
		self.ppu_step();
		self.ppu_step();
	}

  pub fn emu_step_until_vblank(&mut self) {
    let cycles = self.cpu.mcycles;
    while !self.frame_ready {
      self.cpu_step();
    }
    let cycles_run = self.cpu.mcycles - cycles;

    self.frame_ready = false;

    self.apu.blip.0.end_frame(self.apu.frame_cycles as u32);
    self.apu.frame_cycles -= cycles_run;
  }

  #[deprecated]
  pub fn get_debug_framebuf_rgba(&self, buf: &mut [u8; 160 * 144 * 4]) {
    const GB_PALETTE: [(u8, u8, u8); 4] = [
      (155,188,15),
      (139,172,15),
      (48,98,48),
      (15,56,15)
    ];

    for y in 0..18 {
      for x in 0..20 {
        let tile = self.bus.vram[0x1800 + y * 32 + x];
        let tile_addr = self.ppu.tileset_addr(tile);

        for row in 0..8 {
          let pttrn_lo = self.bus.vram[tile_addr as usize - 0x8000 + row*2].reverse_bits();
          let pttrn_hi = self.bus.vram[tile_addr as usize - 0x8000 + row*2 + 1].reverse_bits();

          for col in 0..8 {
            let pixel = (((pttrn_hi >> col) & 1) << 1) | ((pttrn_lo >> col) & 1);
            let color = GB_PALETTE[pixel as usize];

            let y = 160 * 4 * (y*8 + row);
            let x = 4 * (x*8 + col);
            buf[y + x + 0] = color.0;
            buf[y + x + 1] = color.1;
            buf[y + x + 2] = color.2;
            buf[y + x + 3] = 255;
          }
        }
      }
    }
  }

  pub fn get_framebuf_rgba(&self, buf: &mut [u8]) {
    const GB_PALETTE: [(u8, u8, u8); 4] = [
      (155,188,15),
      (139,172,15),
      (48,98,48),
      (15,56,15)
    ];

    for (i, pixel) in self.videobuf.iter().enumerate() {
      let color = GB_PALETTE[*pixel as usize];
      buf[i * 4 + 0] = color.0;
      buf[i * 4 + 1] = color.1;
      buf[i * 4 + 2] = color.2;
      buf[i * 4 + 3] = 255;
    }
  }

  pub fn get_audio(&mut self) -> &[i16] {
    let read = self.apu.blip.0.read_samples(&mut self.audiobuf[..self.apu.blip.0.samples_avail() as usize], false);
    &self.audiobuf[..read]
  }

  pub fn btn_pressed(&mut self, btn: u8) {
    // if button is pressed, it should be 0
    let old = self.joypad.buttons.into_bits();
    self.joypad.buttons.set_bits(old & !btn);

    // The Joypad interrupt is requested when any of P1 bits 0-3 change from High to Low (so when first button is pressed)
    // This interrupt is useful to identify button presses if we have only selected either action (bit 5) or direction (bit 4), but not both.
    if old != self.joypad.buttons.into_bits() {
      self.intf.set_joypad(true);
    }
  }

  pub fn btn_released(&mut self, btn: u8) {
    // button pressed should be 0
    let curr = self.joypad.buttons.into_bits();
    self.joypad.buttons.set_bits(curr | btn);
  }
}