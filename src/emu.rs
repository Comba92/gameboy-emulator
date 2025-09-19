use crate::{bus::{self, Bus}, cart::CartHeader, cpu::CpuSM83, joypad::Joypad, mbc::Mbc, ppu::Ppu, serial::Serial, timer::Timer};

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
  pub(crate) joypad: Joypad,
  pub(crate) serial: Serial,
  pub(crate) timer: Timer,
  pub(crate) mbc: Mbc,

  pub(crate) inte: Interrupt,
  pub(crate) intf: Interrupt,

  header: CartHeader,
  pub(crate) videobuf: [u8; 160 * 144],
  pub(crate) frame_ready: bool,
}

impl Default for Emu {
  fn default() -> Self {
    Self {
      cpu: CpuSM83::new(),
      ppu: Ppu::new(),
      // reads from an absent cartridge usually return $FF
      bus: Bus::new(vec![0xff; 32 * 1024], 0),
      joypad: Joypad::default(),
      serial: Serial::default(),
      timer: Timer::default(),
      mbc: Mbc::None,

      header: CartHeader::default(),
      inte: Interrupt::default(),
      intf: Interrupt::default(),
      videobuf: [0; 160 * 144],
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

  pub fn from_slice(bytes: &[u8]) -> Result<Self, String> {
    Self::new(bytes.to_vec())
  }

  pub fn new(bytes: Vec<u8>) -> Result<Self, String> {
    let header = CartHeader::parse(&bytes)?;
    let mbc = Mbc::new(header.mapper)?;

    let mut emu = Self {
      cpu: CpuSM83::new(),
      ppu: Ppu::new(),
      bus: Bus::new(bytes, header.ram_size),
      joypad: Joypad::default(),
      serial: Serial::default(),
      timer: Timer::default(),
      mbc,

      header,
      inte: Interrupt::default(),
      intf: Interrupt::default(),
      videobuf: [0; 160 * 144],
      frame_ready: false,
    };

    match &emu.mbc {
      Mbc::MBC1(mbc1) => {
        mbc1.update_banks(&mut emu.bus);
        emu.bus.sram_enable(false);
      }
      Mbc::None => {
        emu.bus.rom_banks.set_page(1, 1);
      }
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
    let timer = &mut self.timer;
    
    // TODO: obscure behaviour
    timer.div = timer.div.wrapping_add(1);
    if timer.tac & 0x4 > 0 && timer.div as u8 & timer.clock_mask == 0 {
      if timer.tima == 0xff {
        timer.tima = timer.tma;
        self.intf.set_timer(true);
      } else {
        timer.tima += 1;
      }
    }
  }

	pub(crate) fn tick(&mut self) {
		self.cpu.mcycles += 1;
    self.dma_step();

    self.timer_step();
    
		self.ppu_step();
		self.ppu_step();
		self.ppu_step();
		self.ppu_step();
	}

  pub fn step_until_vblank(&mut self) {
    while !self.frame_ready {
      self.cpu_step();
    }
    self.frame_ready = false;
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