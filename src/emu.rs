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
  pub(crate) videobuf: [u8; 160 * 140],
  pub(crate) frame_ready: bool,
}

impl Default for Emu {
  fn default() -> Self {
    Self {
      cpu: CpuSM83::default(),
      // reads from an absent cartridge usually return $FF
      ppu: Ppu::default(),
      bus: Bus::new(vec![0xff; 32 * 1024]),
      joypad: Joypad::default(),
      header: CartHeader::default(),
      inte: Interrupt::default(),
      intf: Interrupt::default(),
      videobuf: [0; 160 * 140],
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
      bus: Bus::new(bytes),
      joypad: Joypad::default(),
      header,
      inte: Interrupt::default(),
      intf: Interrupt::default(),
      videobuf: [0; 160 * 140],
      frame_ready: false,
    })
  }

  pub fn emu_step(&mut self) {
    self.cpu_step();
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