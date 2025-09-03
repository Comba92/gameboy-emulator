use crate::{bus::{self, Bus}, cart::CartHeader, cpu::CpuSM83};

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
  pub(crate) bus: Bus,

  pub(crate) inte: Interrupt,
  pub(crate) intf: Interrupt,

  header: CartHeader,
}

impl Default for Emu {
  fn default() -> Self {
    Self {
      cpu: CpuSM83::default(),
      // reads from an absent cartridge usually return $FF
      bus: Bus::new(vec![0xff; 32 * 1024]),
      header: CartHeader::default(),
      inte: Interrupt::default(),
      intf: Interrupt::default(),
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
      bus: Bus::new(bytes),
      header,
      inte: Interrupt::default(),
      intf: Interrupt::default(),
    })
  }

  pub fn emu_step(&mut self) {
    self.cpu_step();
  }
}