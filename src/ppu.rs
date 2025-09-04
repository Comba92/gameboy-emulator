use bitfields::bitfield;

use crate::emu::Emu;

#[bitfield(u8)]
#[derive(Clone, Copy)]
pub(crate) struct Ctrl {
  bg_win_enable: bool,
  obj_enable: bool,
  obj_size: bool,
  bg_tilemap: bool,
  tileset_mode: bool,
  win_enable: bool,
  win_tileamp: bool,
  lcd_enable: bool,
}

#[bitfield(u8)]
#[derive(Clone, Copy)]
pub(crate) struct Stat {
  #[bits(2)]
  ppu_mode: u8,
  lyc_eq_ly: bool,
  mode0_int: bool,
  mode1_int: bool,
  mode2_int: bool,
  lyc_int: bool,
  _unused: bool,
}

#[derive(Default)]
pub struct Ppu {
  pub(crate) ctrl: Ctrl,
  pub(crate) ly: u8,
  pub(crate) lyc: u8,
  pub(crate) stat: Stat,
  pub(crate) scy: u8,
  pub(crate) scx: u8,
  pub(crate) wy: u8,
  pub(crate) wx: u8,
  pub(crate) bgp: u8,
  pub(crate) obp0: u8,
  pub(crate) obp1: u8,

  pub(crate) obj_size: u8,
  pub(crate) bg_tilemap: u16,
  pub(crate) win_tilemap: u16,

  dots: u16,
}

impl Emu {
  fn tileset_addr(&self, tile_number: u8) -> u16 {
    if self.ppu.ctrl.tileset_mode() {
      // unsigned mode
      0x8000 + tile_number as u16 * 16
    } else {
      // signed mode
      let tile_number = tile_number as i8 as i32;
      let offset = tile_number * 16;
      (0x9000 + offset) as u16
    }
  }

  pub(crate) fn ppu_step(&mut self) {
    self.ppu.dots += 1;
    if self.ppu.dots >= 456 {
      self.ppu.dots = 0;
      self.ppu.ly += 1;
      if self.ppu.ly == 144 {
        self.intf.set_vblank(true);
        self.frame_ready = true;
      } else if self.ppu.ly >= 154 {
        self.ppu.ly = 0;
      }
    }
  }
}