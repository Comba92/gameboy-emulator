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

struct Sprite {
  drawn: bool,
  x: u8,
  tile_row: u8,
  tile_id: u8,
  attributes: u8,
}
impl Sprite {
  fn priority(&self) -> bool { self.attributes & 0x80 > 0 }
  fn flip_y(&self) -> bool { self.attributes & 0x40 > 0 }
  fn flip_x(&self) -> bool { self.attributes & 0x20 > 0 }
  fn dmg_palette(&self) -> bool { self.attributes & 0x10 > 0 }
}

#[derive(Default, Clone, Copy)]
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

  fetcher: Fetcher,

  win_ly: u8,
  is_window_in_scanline: bool,
  is_fetching_window: bool,
  bg_fifo: VecDeque<PixelData>,

  obj_to_fetch: Option<usize>,
  obj_buf: Vec<Sprite>,
  obj_fifo: VecDeque<PixelData>,

  mode: Mode,
  dots: u16,
}
impl Ppu {
  pub(crate) fn tileset_addr(&self, tile_number: u8) -> u16 {
    if self.ctrl.tileset_mode() {
      // unsigned mode
      let offset = tile_number as u16 * 16;
      0x8000 | offset
    } else {
      // signed mode
      let tile_number = tile_number as i8 as i16;
      let offset = tile_number * 16;
      (0x9000 + offset as i32) as u16
    }
  }

  // TODO: consider precomputing these
  fn color_from_bg_palette(&self, color_id: u8) -> u8 {
    (self.bgp >> (color_id * 2)) & 0b11
  }

  fn color_from_obj_palette(&self, palette_select: u8, color_id: u8) -> u8 {
    if color_id == 0 { return 0 };
    
    let palette = match palette_select {
      0 => self.obp0,
      _  => self.obp1
    };

    (palette >> (color_id * 2)) & 0b11
  }
}

impl Emu {
  pub(crate) fn handle_lyc_int(&mut self) {
    let ppu = &mut self.ppu;
    ppu.stat.set_lyc_eq_ly(ppu.ly == ppu.lyc);
    if ppu.stat.lyc_eq_ly() && ppu.stat.lyc_int() {
      self.intf.set_lcd(true);
    }
  }

  fn handle_mode0_int(&mut self) {
    self.ppu.stat.set_ppu_mode(self.ppu.mode as u8);
    if self.ppu.stat.mode0_int() {
      self.intf.set_lcd(true);
    }
  }

  fn handle_mode1_int(&mut self) {
    self.ppu.stat.set_ppu_mode(self.ppu.mode as u8);
    if self.ppu.stat.mode1_int() {
      self.intf.set_lcd(true);
    }
  }

  fn handle_mode2_int(&mut self) {
    self.ppu.stat.set_ppu_mode(self.ppu.mode as u8);
    if self.ppu.stat.mode2_int() {
      self.intf.set_lcd(true);
    }
  }

  // TODO: every byte fetch should take 2 tcycles
  fn oam_scan(&mut self) {
    self.ppu.obj_buf.clear();

    for bytes in self.bus.oam.chunks(4) {
      let y = bytes[0];
      let ly = self.ppu.ly + 16;

      if y <= ly && ly < y + self.ppu.obj_size {
        let sprite = Sprite {
          drawn: false,
          tile_row: ly - bytes[0],
          x: bytes[1],
          tile_id: bytes[2],
          attributes: bytes[3],
        };
        self.ppu.obj_buf.push(sprite);
        if self.ppu.obj_buf.len() >= 10 { break; }
      }
    }
  }

  fn fetcher_step(&mut self) {
    let ppu = &mut self.ppu;

    if ppu.obj_to_fetch.is_none() {
      if let Some(obj) = ppu.obj_buf.iter()
      .position(|s| !s.drawn && s.x as i16 == ppu.fetcher.pixel_x + 8) 
      {
        ppu.fetcher.state = FetcherState::default();
        ppu.obj_fifo.clear();
        ppu.obj_to_fetch = Some(obj);
      }
    }

    if !ppu.is_fetching_window {
      let is_window_first_tile = ppu.ctrl.win_enable()
        && ppu.is_window_in_scanline
        && ppu.fetcher.pixel_x + 7 == ppu.wx as i16;

      if is_window_first_tile {
        ppu.is_fetching_window = true;
        // When rendering the window the background FIFO is cleared and the fetcher is reset to step 1.
        // A 6-dot penalty is incurred while the BG fetcher is being set up for the window.
        ppu.fetcher.tile_count = 0;
        ppu.fetcher.state = FetcherState::default();
        ppu.bg_fifo.clear();
      }
    }

    match ppu.obj_to_fetch {
      Some(obj_idx) => self.fetch_obj(obj_idx),
      None => self.fetch_bg(),
    }
  }

  fn fetch_bg(&mut self) {
    let ppu = &mut self.ppu;
    
    use FetcherState::*;
    match ppu.fetcher.state {
      VramWait => ppu.fetcher.state = Vram,
      Vram => {
        ppu.fetcher.state = TileLoWait;

        let vram_addr = match ppu.is_fetching_window {
          false => {
            // background
            // The scroll registers are re-read on each tile fetch, except for the low 3 bits of SCX, which are only read at the beginning of the scanline (for the initial shifting of pixels).
            let tile_x = (ppu.fetcher.tile_count as u16 + ppu.scx as u16 / 8) % 32;
            ppu.fetcher.pixel_y = ppu.ly as u16 + ppu.scy as u16;
            let tile_y = 32 * ((ppu.fetcher.pixel_y % 256) / 8);
            let tile_offset = tile_y + tile_x;
            ppu.bg_tilemap + (tile_offset % 1024)
          }
          true => {
            // window
            let tile_x = ppu.fetcher.tile_count as u16;
            ppu.fetcher.pixel_y = ppu.win_ly as u16;
            let tile_y = 32 * (ppu.fetcher.pixel_y / 8);
            let tile_offset = tile_y + tile_x;
            ppu.win_tilemap + (tile_offset % 1024)
          }
        };

        self.ppu.fetcher.tile_id = self.dispatch_read(vram_addr);
      }

      TileLoWait => ppu.fetcher.state = TileLo,
      TileLo => {
        ppu.fetcher.state = TileHiWait;

        let tile_addr = ppu.tileset_addr(ppu.fetcher.tile_id);
        let tile_data_addr = tile_addr + 2 * (ppu.fetcher.pixel_y % 8);
        ppu.fetcher.tile_data_addr = tile_data_addr;
        self.ppu.fetcher.tile_data_lo = self.dispatch_read(tile_data_addr);
      },

      TileHiWait => ppu.fetcher.state = TileHi,
      TileHi => {
        ppu.fetcher.state = Push;

        let tile_data_addr = ppu.fetcher.tile_data_addr + 1;
        self.ppu.fetcher.tile_data_hi = self.dispatch_read(tile_data_addr);
      },

      Push => if ppu.bg_fifo.is_empty() {
        ppu.fetcher.state = VramWait;

        // The 12 extra dots of penalty come from two tile fetches at the beginning of Mode 3. One is the first tile in the scanline (the one that gets shifted by SCX % 8 pixels), the other is simply discarded.
        // pixel_x starts at -8 or less (if SCX%8 > 0). If it is less than -8, it means we are rendering the first tile, and we should fetch it twice 
        if ppu.fetcher.pixel_x > -8 {
          ppu.fetcher.tile_count += 1;
        }

        for i in (0..8).rev() {
          let color_lo = (ppu.fetcher.tile_data_lo >> i) & 1;
          let color_hi = (ppu.fetcher.tile_data_hi >> i) & 1;
          let color = (color_hi << 1) | color_lo;

          let pixel_data = PixelDataBuilder::new()
            .with_color(color)
            .build();

          ppu.bg_fifo.push_back(pixel_data);
        }
      }
    }
  }

  fn fetch_obj(&mut self, obj_idx: usize) {
    let ppu = &mut self.ppu;
    
    use FetcherState::*;
    match ppu.fetcher.state {
      VramWait => ppu.fetcher.state = Vram,
      Vram => {
        ppu.fetcher.state = TileLoWait;

        let obj = &mut ppu.obj_buf[obj_idx];
        obj.drawn = true;
        ppu.fetcher.pixel_y = if obj.flip_y() { 7 - obj.tile_row } else { obj.tile_row } as u16; 
        ppu.fetcher.tile_id = obj.tile_id;
      }

      TileLoWait => ppu.fetcher.state = TileLo,
      TileLo => {
        ppu.fetcher.state = TileHiWait;

        let tile_addr = 0x8000 | (ppu.fetcher.tile_id as u16 * 16);
        let tile_data_addr = tile_addr + 2 * (ppu.fetcher.pixel_y % 8);
        ppu.fetcher.tile_data_addr = tile_data_addr;
        self.ppu.fetcher.tile_data_lo = self.dispatch_read(tile_data_addr);
      },

      TileHiWait => ppu.fetcher.state = TileHi,
      TileHi => {
        ppu.fetcher.state = Push;

        let tile_data_addr = ppu.fetcher.tile_data_addr + 1;
        self.ppu.fetcher.tile_data_hi = self.dispatch_read(tile_data_addr);
      },

      Push => {
        ppu.fetcher.state = VramWait;

        let obj = &ppu.obj_buf[obj_idx];
        if obj.flip_x() {
          ppu.fetcher.tile_data_lo = ppu.fetcher.tile_data_lo.reverse_bits();
          ppu.fetcher.tile_data_hi = ppu.fetcher.tile_data_hi.reverse_bits();
        }

        for i in (0..8).rev() {
          let color_lo = (ppu.fetcher.tile_data_lo >> i) & 1;
          let color_hi = (ppu.fetcher.tile_data_hi >> i) & 1;
          let color = (color_hi << 1) | color_lo;

          let pixel_data = PixelDataBuilder::new()
            .with_color(color)
            .with_palette(obj.dmg_palette() as u8)
            .with_priority(obj.priority())
            .build();

          ppu.obj_fifo.push_back(pixel_data);
        }

        ppu.obj_to_fetch = None;
      }
    }
  }

  fn pixel_push(&mut self) {
    let ppu = &mut self.ppu;

    if !ppu.bg_fifo.is_empty() && ppu.obj_to_fetch.is_none() {
      if ppu.fetcher.pixel_x < 0 {
        // discard pixel
        ppu.bg_fifo.pop_front();
        ppu.obj_fifo.pop_front();
      } else {
        // push pixel to framebuffer
        let bg_pixel = ppu.bg_fifo.pop_front().unwrap();
        let obj_pixel = ppu.obj_fifo.pop_front().unwrap_or_default();

        let bg_color = ppu.color_from_bg_palette(bg_pixel.color());
        let obj_color = ppu.color_from_obj_palette(obj_pixel.palette(), obj_pixel.color());

        let final_color = if obj_color > 0 {
          obj_color
        } else {
          bg_color
        };

        let idx = (ppu.ly as usize * 160) + ppu.fetcher.pixel_x as usize;
        self.videobuf[idx] = final_color;
      }

      ppu.fetcher.pixel_x += 1;
    }
  }

  pub(crate) fn ppu_step(&mut self) {
    let ppu = &mut self.ppu;
    // we increase it early, and transition to the next state now; next state will be handled next step
    ppu.dots += 1;

    match ppu.mode {
      Mode::OamScan => {
        // WY condition was triggered: i.e. at some point in this frame the value of WY was equal to LY (checked at the start of Mode 2 only)
        if ppu.dots-1 == 0 && ppu.ctrl.win_enable() && ppu.ly == ppu.wy {
          ppu.is_window_in_scanline = true;
        }

        if ppu.dots >= 80 {
          ppu.mode = Mode::Drawing;
          ppu.stat.set_ppu_mode(ppu.mode as u8);
          self.oam_scan();
        }
      }

      Mode::Drawing => {
        self.fetcher_step();
        self.pixel_push();

        if self.ppu.fetcher.pixel_x >= 160 {
          self.ppu.mode = Mode::Hblank;
          self.handle_mode0_int();
        }
      }

      Mode::Hblank => {
        if ppu.dots >= 456 {
          ppu.dots = 0;
          ppu.ly += 1;
          self.handle_lyc_int();

          let ppu = &mut self.ppu;
          if ppu.is_fetching_window {
            ppu.win_ly += 1;
          }
          ppu.is_fetching_window = false;

          if ppu.ly >= 144 {
            ppu.mode = Mode::Vblank;
            self.handle_mode1_int();
            
            self.intf.set_vblank(true);
            self.frame_ready = true;
          } else {
            // The scroll registers are re-read on each tile fetch, except for the low 3 bits of SCX, which are only read at the beginning of the scanline (for the initial shifting of pixels).
            ppu.fetcher.reset(ppu.scx);
            ppu.bg_fifo.clear();
            ppu.obj_fifo.clear();
            ppu.obj_to_fetch = None;

            ppu.mode = Mode::OamScan;
            self.handle_mode2_int();
          }
        }
      }

      Mode::Vblank => {
        if ppu.dots >= 456 {
          ppu.dots = 0;
          ppu.ly += 1;
          self.handle_lyc_int();

          let ppu = &mut self.ppu;
          if ppu.ly >= 154 {
            ppu.ly = 0;
            ppu.win_ly = 0;
            ppu.is_window_in_scanline = false;
            ppu.fetcher.reset(ppu.scx);
            ppu.bg_fifo.clear();
            ppu.obj_fifo.clear();
            ppu.obj_to_fetch = None;
            
            ppu.mode = Mode::OamScan;
            self.handle_mode2_int();
          }
        }
      }
    }
  }
}