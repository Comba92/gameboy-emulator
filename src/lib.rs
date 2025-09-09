pub mod emu;
pub mod cpu;
mod bus;
mod ppu;
mod cart;

pub mod joypad {
  #[bitfields::bitfield(u8)]
#[derive(Clone, Copy)]
  pub(super) struct Buttons {
    pub a: bool,
    pub b: bool,
    pub select: bool,
    pub start: bool,
    pub right: bool,
    pub left: bool,
    pub up: bool,
    pub down: bool,
  }
  
  pub(super) struct Joypad {
    pub(super) buttons: Buttons,
    pub(super) select_dpad: bool,
    pub(super) select_btns: bool,
  }

  pub const A:    u8 = 0x01;
  pub const B:    u8 = 0x02;
  pub const SELECT: u8 = 0x04;
  pub const START:  u8 = 0x08;
  pub const RIGHT:  u8 = 0x10;
  pub const LEFT:   u8 = 0x20;
  pub const UP:   u8 = 0x40;
  pub const DOWN: u8 = 0x80;

  impl Default for Joypad {
    fn default() -> Self {
      Self {
        buttons: Buttons::from_bits(0xff),
        select_btns: true,
        select_dpad: true,
      }
    }
  }

  impl Joypad {
    // Note that, rather unconventionally for the Game Boy, a button being pressed is seen as the corresponding bit being 0, not 1.
    pub(super) fn read(&self) -> u8 {
      let mut res = (0b1100_0000) | ((self.select_dpad as u8) << 4) | ((self.select_btns as u8) << 5);
      
      // TODO: which wins when both are selected?
      if self.select_dpad {
        res |= self.buttons.0 >> 4
      } else if self.select_btns {
        res |= self.buttons.0 & 0xf
      } else { res = 0xff }

      res
    }

    pub(super) fn write(&mut self, val: u8) {
      self.select_dpad = val & 0x10 == 0;
      self.select_btns = val & 0x20 == 0;
    }
  }
}