use crate::{bus::{self, Bus}, cart::CartHeader, cpu::CpuSM83, joypad::Joypad, ppu::Ppu};

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
  pub(crate) bus: Bus,
  pub(crate) joypad: Joypad,

  pub(crate) inte: Interrupt,
  pub(crate) intf: Interrupt,

  header: CartHeader,
  pub(crate) videobuf: [u8; 160 * 144],
  pub(crate) frame_ready: bool,
}

impl Default for Emu {
  fn default() -> Self {
    Self {
      cpu: CpuSM83::default(),
      // reads from an absent cartridge usually return $FF
      ppu: Ppu::default(),
      bus: Bus::new(vec![0xff; 32 * 1024], 0),
      joypad: Joypad::default(),
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

    Ok(Self {
      cpu: CpuSM83::default(),
      ppu: Ppu::default(),
      bus: Bus::new(bytes, 0),
      joypad: Joypad::default(),
      header,
      inte: Interrupt::default(),
      intf: Interrupt::default(),
      videobuf: [0; 160 * 144],
      frame_ready: false,
    })
  }

	pub(crate) fn tick(&mut self) {
		self.cpu.mcycles += 1;

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
    let curr = self.joypad.buttons.into_bits();

    // The Joypad interrupt is requested when any of P1 bits 0-3 change from High to Low.
    let high_to_low = curr & !btn;
    // This interrupt is useful to identify button presses if we have only selected either action (bit 5) or direction (bit 4), but not both. 
    let not_both = !(self.joypad.select_btns && self.joypad.select_dpad);
    let btns_int = self.joypad.select_btns && high_to_low & 0x0f > 0;
    let dpad_int = self.joypad.select_dpad && high_to_low & 0xf0 > 0;
    if not_both && (btns_int || dpad_int) {
      self.intf.set_joypad(true);
    }

    self.joypad.buttons.set_bits(curr & !btn);
  }

  pub fn btn_released(&mut self, btn: u8) {
    // if button is released, it should be 1
    let curr = self.joypad.buttons.into_bits();
    self.joypad.buttons.set_bits(curr | btn);
  }
}