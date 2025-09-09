use std::collections::VecDeque;

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

#[bitfield(u8)]
struct PixelData {
  #[bits(2)]
  color: u8,
  #[bits(3)]
  palette: u8,
  priority: bool,
  #[bits(2)]
  _unused: u8,
}

#[derive(Default)]
enum FetcherState {
  #[default] TileIdIdle, TileId, TileLoIdle, TileLo, TileHiIdle, TileHi, Sleep1, Sleep2, Push
}
#[derive(Default)]
struct Fetcher {
  tile_count: u8,
  tile_id: u8,
  tile_data_addr: u16,
  tile_data_lo: u8,
  tile_data_hi: u8,
  state: FetcherState,
}

#[derive(Default)]
enum Mode {
  #[default] OamScan = 2, Drawing = 3, Hblank = 0, Vblank = 1
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

  bg_fetcher: Fetcher,
  bg_fifo: VecDeque<PixelData>,

  mode: Mode,
  dots: u16,
}
impl Ppu {
  pub(crate) fn tileset_addr(&self, tile_number: u8) -> u16 {
    if self.ctrl.tileset_mode() {
      // unsigned mode
      0x8000 + tile_number as u16 * 16
    } else {
      // signed mode
      let tile_number = tile_number as i8 as i16;
      let offset = tile_number * 16;
      (0x9000 + offset as i32) as u16
    }
  }

  fn bg_tilemap(&self) -> u16 {
    if self.ctrl.bg_tilemap() {
      0x9c00
    } else {
      0x9800
    }
  }
}

impl Emu {
  fn fetcher_step(&mut self) {
    let ppu = &mut self.ppu;

    use FetcherState::*;
    match ppu.bg_fetcher.state {
      TileIdIdle => ppu.bg_fetcher.state = TileId,
      TileId => {
        ppu.bg_fetcher.state = TileLoIdle;

        let tile_x = (ppu.bg_fetcher.tile_count + ppu.scx / 8) % 32;
        let tile_y = 32 * (((ppu.ly + ppu.scy) & 0xff) / 8);
        let vram_addr = ppu.bg_tilemap() + ((tile_x as u16 + tile_y as u16) & 0x3ff);
        self.ppu.bg_fetcher.tile_id = self.dispatch_read(vram_addr);
      }

      TileLoIdle => ppu.bg_fetcher.state = TileLo,
      TileLo => {
        ppu.bg_fetcher.state = TileHiIdle;

        let tile_addr = ppu.tileset_addr(ppu.bg_fetcher.tile_id);
        let tile_data_addr = tile_addr + 2 * ((ppu.ly as u16 + ppu.scy as u16) % 8);
        ppu.bg_fetcher.tile_data_addr = tile_data_addr;
        self.ppu.bg_fetcher.tile_data_lo = self.dispatch_read(tile_data_addr);
      },

      TileHiIdle => ppu.bg_fetcher.state = TileHi,
      TileHi => {
        ppu.bg_fetcher.state = Sleep1;

        let tile_data_addr = ppu.bg_fetcher.tile_data_addr + 1;
        self.ppu.bg_fetcher.tile_data_hi = self.dispatch_read(tile_data_addr);
      },

      Sleep1 => ppu.bg_fetcher.state = Sleep2,
      Sleep2 => ppu.bg_fetcher.state = Push,
      Push => {
        if ppu.bg_fifo.is_empty() {
          ppu.bg_fetcher.state = TileIdIdle;
          ppu.bg_fetcher.tile_count += 1;

          for i in 0..8 {
            let color_lo = (ppu.bg_fetcher.tile_data_lo >> i) & 1;
            let color_hi = (ppu.bg_fetcher.tile_data_hi >> i) & 1;
            let color = (color_hi << 1) | color_lo;

            let pixel_data = PixelDataBuilder::new()
              .with_color(color)
              .with_palette(0)
              .build();

            ppu.bg_fifo.push_back(pixel_data);
          }
        }
      }
    }
  }

  fn pixel_push(&mut self) {
    if !self.ppu.bg_fifo.is_empty() {
      // push pixel to framebuffer
    }
  }

  pub(crate) fn ppu_step(&mut self) {
    let ppu = &mut self.ppu;
    match ppu.mode {
      Mode::OamScan => {
        if ppu.dots >= 79 {
          ppu.mode = Mode::Drawing;
        }
      }

      Mode::Drawing => {
        if ppu.dots >= 171 {
          ppu.mode = Mode::Hblank;
        }
      }

      Mode::Hblank => {}
      Mode::Vblank => {}
    }


    self.ppu.dots += 1;
    if self.ppu.dots >= 456 {
      self.ppu.mode = Mode::OamScan;
      self.ppu.dots = 0;
      self.ppu.ly += 1;
      if self.ppu.ly == 144 {
        self.ppu.mode = Mode::Vblank;
        self.intf.set_vblank(true);
        self.frame_ready = true;
      } else if self.ppu.ly >= 154 {
        self.ppu.ly = 0;
      }
    }
  }
}