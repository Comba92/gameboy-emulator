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
  #[bits(default = true)]
  _unused: bool,
}

#[bitfield(u8)]
struct StatInterrupts {
  lyc: bool,
  mode0: bool,
  mode1: bool,
  mode2: bool,
  #[bits(4)]
  _unused: u8,
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
  #[default] VramWait, Vram, TileLoWait, TileLo, TileHiWait, TileHi, Push
}
struct Fetcher {
  pixel_x: i16,
  pixel_y: u16,
  tile_count: u8,
  tile_id: u8,
  tile_data_addr: u16,
  tile_data_lo: u8,
  tile_data_hi: u8,
  state: FetcherState,
}
impl Default for Fetcher {
  fn default() -> Self {
    Self {
      // the fetcher starts rendering from the first tile to the left of the viewport (first tile is fetched two times)
      pixel_x: -8,
      pixel_y: 0,
      tile_count: 0,
      tile_id: 0,
      tile_data_addr: 0,
      tile_data_lo: 0,
      tile_data_hi: 0,
      state: Default::default(),
    }
  }
}
impl Fetcher {
  fn reset(&mut self, scx: u8) {
    *self = Self::default();
    self.pixel_x = -(8 + (scx as i16 % 8));
  }
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

  win_ly: u8,
  win_wy_cond: bool,
  win_wx_cond: bool,
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

  // TODO: consider precomputing these
  fn color_from_palette(&self, color_id: u8) -> u8 {
    (self.bgp >> (color_id * 2)) & 0b11
  }
}

impl Emu {
  pub(crate) fn handle_lyc(&mut self) {
    let ppu = &mut self.ppu;
    ppu.stat.set_lyc_eq_ly(ppu.ly == ppu.lyc);
    if ppu.stat.lyc_eq_ly() && ppu.stat.lyc_int() {
      self.intf.set_lcd(true);
    }
  }

  fn fetcher_step(&mut self) {
    let ppu = &mut self.ppu;

    if !ppu.win_wx_cond {
      let is_window_tile = ppu.ctrl.win_enable()
        && ppu.win_wy_cond
        && ppu.bg_fetcher.pixel_x + 7 == ppu.wx as i16;

      if is_window_tile {
        ppu.win_wx_cond = true;
        // When rendering the window the background FIFO is cleared and the fetcher is reset to step 1.
        // A 6-dot penalty is incurred while the BG fetcher is being set up for the window.
        ppu.bg_fetcher.tile_count = 0;
        ppu.bg_fetcher.state = VramWait;
        ppu.bg_fifo.clear();
      }
    }

    use FetcherState::*;
    match ppu.bg_fetcher.state {
      VramWait => ppu.bg_fetcher.state = Vram,
      Vram => {
        ppu.bg_fetcher.state = TileLoWait;

        let vram_addr = match ppu.win_wx_cond {
          false => {
            // background
            // The scroll registers are re-read on each tile fetch, except for the low 3 bits of SCX, which are only read at the beginning of the scanline (for the initial shifting of pixels).
            let tile_x = (ppu.bg_fetcher.tile_count as u16 + ppu.scx as u16 / 8) % 32;
            ppu.bg_fetcher.pixel_y = ppu.ly as u16 + ppu.scy as u16;
            let tile_y = 32 * ((ppu.bg_fetcher.pixel_y % 256) / 8);
            let tile_offset = tile_y + tile_x;
            ppu.bg_tilemap + (tile_offset % 1024)
          }
          true => {
            // window
            let tile_x = ppu.bg_fetcher.tile_count as u16;
            ppu.bg_fetcher.pixel_y = ppu.win_ly as u16;
            let tile_y = 32 * (ppu.bg_fetcher.pixel_y / 8);
            let tile_offset = tile_y + tile_x;
            ppu.win_tilemap + (tile_offset % 1024)
          }
        };

        self.ppu.bg_fetcher.tile_id = self.dispatch_read(vram_addr);
      }

      TileLoWait => ppu.bg_fetcher.state = TileLo,
      TileLo => {
        ppu.bg_fetcher.state = TileHiWait;

        let tile_addr = ppu.tileset_addr(ppu.bg_fetcher.tile_id);
        let tile_data_addr = tile_addr + 2 * (ppu.bg_fetcher.pixel_y % 8);
        ppu.bg_fetcher.tile_data_addr = tile_data_addr;
        self.ppu.bg_fetcher.tile_data_lo = self.dispatch_read(tile_data_addr);
      },

      TileHiWait => ppu.bg_fetcher.state = TileHi,
      TileHi => {
        ppu.bg_fetcher.state = Push;

        let tile_data_addr = ppu.bg_fetcher.tile_data_addr + 1;
        self.ppu.bg_fetcher.tile_data_hi = self.dispatch_read(tile_data_addr);
      },

      Push => if ppu.bg_fifo.is_empty() {
        ppu.bg_fetcher.state = VramWait;

        // The 12 extra dots of penalty come from two tile fetches at the beginning of Mode 3. One is the first tile in the scanline (the one that gets shifted by SCX % 8 pixels), the other is simply discarded.
        // pixel_x starts at -8 or less (if SCX%8 > 0). If it is less than -8, it means we are rendering the first tile, and we should fetch it twice 
        if ppu.bg_fetcher.pixel_x > -8 {
          ppu.bg_fetcher.tile_count += 1;
        }

        for i in (0..8).rev() {
          let color_lo = (ppu.bg_fetcher.tile_data_lo >> i) & 1;
          let color_hi = (ppu.bg_fetcher.tile_data_hi >> i) & 1;
          let color = (color_hi << 1) | color_lo;

          let pixel_data = PixelDataBuilder::new()
            .with_color(color)
            .build();

          ppu.bg_fifo.push_back(pixel_data);
        }
      }
    }
  }

  fn pixel_push(&mut self) {
    let ppu = &mut self.ppu;

    if !ppu.bg_fifo.is_empty() {
      if ppu.bg_fetcher.pixel_x < 0 {
        // discard pixel
        ppu.bg_fifo.pop_front();
      } else {
        // push pixel to framebuffer
        let pixel = ppu.bg_fifo.pop_front().unwrap();
        let color = ppu.color_from_palette(pixel.color());
        let idx = (ppu.ly as usize * 160) + ppu.bg_fetcher.pixel_x as usize;
        self.videobuf[idx] = color;
      }

      ppu.bg_fetcher.pixel_x += 1;
    }
  }

  pub(crate) fn ppu_step(&mut self) {
    let ppu = &mut self.ppu;
    // we increase it early, and transition to the next state now; next state will be handled next step
    ppu.dots += 1;

    match ppu.mode {
      Mode::OamScan => {
        // WY condition was triggered: i.e. at some point in this frame the value of WY was equal to LY (checked at the start of Mode 2 only)
        if ppu.ctrl.win_enable() && ppu.ly == ppu.wy {
          ppu.win_wy_cond = true;
        }

        if ppu.dots >= 80 {
          ppu.mode = Mode::Drawing;
        }
      }

      Mode::Drawing => {
        self.fetcher_step();
        self.pixel_push();

        if self.ppu.bg_fetcher.pixel_x >= 160 {
          self.ppu.mode = Mode::Hblank;
        }
      }

      Mode::Hblank => {
        if ppu.dots >= 456 {
          ppu.dots = 0;
          ppu.ly += 1;
          self.handle_lyc();

          let ppu = &mut self.ppu;
          if ppu.win_wx_cond {
            ppu.win_ly += 1;
          }
          ppu.win_wx_cond = false;

          if ppu.ly >= 144 {
            ppu.mode = Mode::Vblank;   
            self.intf.set_vblank(true);
            self.frame_ready = true;
          } else {
            ppu.mode = Mode::OamScan;
            // The scroll registers are re-read on each tile fetch, except for the low 3 bits of SCX, which are only read at the beginning of the scanline (for the initial shifting of pixels).
            self.ppu.bg_fetcher.reset(self.ppu.scx);
            self.ppu.bg_fifo.clear();
          }
        }
      }

      Mode::Vblank => {
        if ppu.dots >= 456 {
          ppu.dots = 0;
          ppu.ly += 1;
          self.handle_lyc();

          let ppu = &mut self.ppu;
          if ppu.ly >= 154 {
            ppu.mode = Mode::OamScan;
            ppu.ly = 0;
            ppu.win_ly = 0;
            ppu.win_wy_cond = false;
            ppu.bg_fetcher.reset(ppu.scx);
            ppu.bg_fifo.clear();
          }
        }
      }
    }
  }
}