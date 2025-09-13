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

  fn get_color_at(&self, column: u8) -> u8 {
    let color_lo = (self.tile_data_lo >> column) & 1;
    let color_hi = (self.tile_data_hi >> column) & 1;
    (color_hi << 1) | color_lo
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
  // 0 = Sprite is always rendered above background
  // 1 = Background colors 1-3 overlay sprite, sprite is still rendered above color 0
  fn priority(&self) -> bool { self.attributes & 0x80 > 0 }
  fn flip_y(&self) -> bool { self.attributes & 0x40 > 0 }
  fn flip_x(&self) -> bool { self.attributes & 0x20 > 0 }
  fn dmg_palette(&self) -> bool { self.attributes & 0x10 > 0 }
}

#[derive(Default, Clone, Copy)]
enum Mode {
  #[default] OamScan = 2, Drawing = 3, Hblank = 0, Vblank = 1, Disabled = 4
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
  is_window_scanline_reached: bool,
  is_fetching_window: bool,
  bg_fifo: VecDeque<PixelData>,

  is_fetching_objects: bool,
  obj_fetching: Option<usize>,
  obj_visible_buf: Vec<Sprite>,
  obj_fifo: VecDeque<PixelData>,
  
  mode: Mode,
  stat_intf: bool, 
  dots: u16,
}
impl Ppu {
  pub fn new() -> Self {
    let mut ppu = Self::default();
    ppu.ctrl.set_lcd_enable(true);
    ppu.ctrl.set_bg_win_enable(true);
    ppu.ctrl.set_tileset_mode(true);
    ppu.stat.set_ppu_mode(1);
    ppu.bgp = 0xfc;
    ppu.bg_tilemap = 0x9800;
    ppu.win_tilemap = 0x9800;
    ppu.obj_size = 8;

    ppu
  }

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

  fn reset(&mut self) {
    self.fetcher.reset(self.scx);
    self.bg_fifo.clear();
    self.obj_fifo.clear();
    self.is_fetching_window = false;
    self.is_fetching_objects = false;
    self.obj_fetching = None;
  }
}

impl Emu {
  pub(crate) fn lcd_set_enabled(&mut self) {
    self.ppu.dots = 0;
    self.enter_mode2();
    self.handle_lyc();
  }

  pub(crate) fn lcd_set_disabled(&mut self) {
    let ppu = &mut self.ppu;
    ppu.mode = Mode::Disabled;
    ppu.dots = 0;
    ppu.ly = 0;
    ppu.lyc = 0;
    // PPU Stat mode reports 0 instead when the PPU is disabled.
    ppu.stat.set_ppu_mode(0);
    self.videobuf.fill(0);
  }

  pub(crate) fn ly_increase(&mut self) {
    let ppu = &mut self.ppu;
    ppu.dots = 0;
    ppu.ly += 1;
    self.handle_lyc();
  }

  pub(crate) fn handle_stat_int(&mut self) {
    let stat = &self.ppu.stat;
    let new_int = (stat.lyc_eq_ly() && stat.lyc_int())
      || (stat.ppu_mode() == 0 && stat.mode0_int())
      || (stat.ppu_mode() == 1 && stat.mode1_int())
      || (stat.ppu_mode() == 2 && stat.mode2_int());

    if !self.ppu.stat_intf && new_int {
      self.intf.set_lcd(true);
    }

    self.ppu.stat_intf = new_int;
  }

  fn enter_mode2(&mut self) {
    // The scroll registers are re-read on each tile fetch, except for the low 3 bits of SCX, which are only read at the beginning of the scanline (for the initial shifting of pixels).
    self.ppu.reset();
    self.ppu.dots = 0;

    self.ppu.mode = Mode::OamScan;
    self.ppu.stat.set_ppu_mode(2);
    if self.ppu.stat.mode2_int() {
      self.intf.set_lcd(true);
    }
    // self.handle_stat_int();
  }

  pub(crate) fn handle_lyc(&mut self) {
    let ppu = &mut self.ppu;
    ppu.stat.set_lyc_eq_ly(ppu.ly == ppu.lyc);
    if ppu.stat.lyc_eq_ly() && ppu.stat.lyc_int() {
      self.intf.set_lcd(true);
    }
    // self.handle_stat_int();
  }

  fn enter_hblank(&mut self) {
    self.ppu.mode = Mode::Hblank;
    self.ppu.stat.set_ppu_mode(0);
    if self.ppu.stat.mode0_int() {
      self.intf.set_lcd(true);
    }
    // self.handle_stat_int();
  }

  fn enter_vblank(&mut self) {
    self.ppu.mode = Mode::Vblank;
    self.ppu.stat.set_ppu_mode(1);
    if self.ppu.stat.mode1_int() {
      self.intf.set_lcd(true);
    }
    // self.handle_stat_int();

    self.intf.set_vblank(true);
    self.frame_ready = true;
  }

  // TODO: every byte fetch should take 2 tcycles
  fn oam_scan(&mut self) {
    self.ppu.obj_visible_buf.clear();

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
        self.ppu.obj_visible_buf.push(sprite);
        if self.ppu.obj_visible_buf.len() >= 10 { break; }
      }
    }
  }

  fn drawing_step(&mut self) {
    let ppu = &mut self.ppu;

    if ppu.obj_fetching.is_none() {
      if let Some(obj) = ppu.obj_visible_buf.iter()
        .position(|s| !s.drawn && s.x as i16 == ppu.fetcher.pixel_x + 8)
      {
        ppu.is_fetching_objects = true;
        ppu.fetcher.state = FetcherState::default();
        ppu.obj_fetching = Some(obj);
      } else {
        ppu.is_fetching_objects = false;
      }
    }

    if !ppu.is_fetching_window {
      let is_window_first_tile = ppu.ctrl.win_enable()
        && ppu.is_window_scanline_reached
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

    match ppu.obj_fetching {
      Some(obj_idx) => self.fetch_obj(obj_idx),
      None => self.fetch_bg(),
    }

    self.pixel_push();
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
            ppu.bg_tilemap + ((tile_y + tile_x) % 1024)
          }
          true => {
            // window
            let tile_x = ppu.fetcher.tile_count as u16;
            ppu.fetcher.pixel_y = ppu.win_ly as u16;
            let tile_y = 32 * (ppu.fetcher.pixel_y / 8);
            ppu.win_tilemap + ((tile_y + tile_x) % 1024)
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
          let color = ppu.fetcher.get_color_at(i);

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
        let obj = &mut ppu.obj_visible_buf[obj_idx];

        // In 8×16 mode (LCDC bit 2 = 1), the memory area at $8000-$8FFF is still interpreted as a series of 8×8 tiles, where every 2 tiles form an object.
        ppu.fetcher.pixel_y = if obj.flip_y() { ppu.obj_size - 1 - obj.tile_row } else { obj.tile_row } as u16;
        // In 8×16 mode, the least significant bit of the tile index is ignored;
        ppu.fetcher.tile_id = if ppu.ctrl.obj_size() { obj.tile_id & !1 } else { obj.tile_id };
      }

      TileLoWait => ppu.fetcher.state = TileLo,
      TileLo => {
        ppu.fetcher.state = TileHiWait;

        // This unsigned value selects a tile from the memory area at $8000-$8FFF
        let tile_addr = 0x8000 | (ppu.fetcher.tile_id as u16 * 16);
        let tile_data_addr = tile_addr + 2 * ppu.fetcher.pixel_y;
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
        let obj = &mut ppu.obj_visible_buf[obj_idx];

        // Be careful here, we need to push to deque in correct order (from left to right!)
        // pixel bits are reversed in memory. so we reverse them if we dont have flip_x. 
        if !obj.flip_x() {
          ppu.fetcher.tile_data_lo = ppu.fetcher.tile_data_lo.reverse_bits();
          ppu.fetcher.tile_data_hi = ppu.fetcher.tile_data_hi.reverse_bits();
        }

        for i in 0..8 {
          let color = ppu.fetcher.get_color_at(i);

          let pixel_data = PixelDataBuilder::new()
            .with_color(color)
            .with_palette(obj.dmg_palette() as u8)
            .with_priority(obj.priority())
            .build();

          match ppu.obj_fifo.get_mut(i as usize) {
            // If the target object pixel is not white and the pixel in the OAM FIFO is white, or if the pixel in the OAM FIFO has higher priority than the target object’s pixel, then the pixel in the OAM FIFO is replaced with the target object’s properties.
            Some(pixel) => if pixel.color() == 0 { *pixel = pixel_data; }
            None => ppu.obj_fifo.push_back(pixel_data),
          }
        }
        
        // set this object as drawn (so we don't check for it next time we iterate)
        obj.drawn = true;
        ppu.obj_fetching = None;
      }
    }
  }

  fn pixel_push(&mut self) {
    let ppu = &mut self.ppu;

    if !ppu.bg_fifo.is_empty() && !ppu.is_fetching_objects {
      if ppu.fetcher.pixel_x < 0 {
        // discard pixel
        ppu.bg_fifo.pop_front();
        ppu.obj_fifo.pop_front();
      } else {
        // push pixel to framebuffer
        let bg_pixel = ppu.bg_fifo.pop_front().unwrap();
        // if we don't have anything in object fifo, we simply use an all zeros pixel
        let obj_pixel = ppu.obj_fifo.pop_front().unwrap_or_default();

        let bg_color = ppu.color_from_bg_palette(bg_pixel.color()) * ppu.ctrl.bg_win_enable() as u8;
        let obj_color = ppu.color_from_obj_palette(obj_pixel.palette(), obj_pixel.color()) * ppu.ctrl.obj_enable() as u8;

        let final_color = if obj_color > 0 && (!obj_pixel.priority() || bg_color == 0) {
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
          ppu.is_window_scanline_reached = true;
        }

        if ppu.dots >= 80 {
          ppu.mode = Mode::Drawing;
          ppu.stat.set_ppu_mode(3);

          // we do oam scan all in one go
          self.oam_scan();
        }
      }

      Mode::Drawing => {
        self.drawing_step();
        if self.ppu.fetcher.pixel_x >= 160 {
          self.enter_hblank();
        }
      }

      Mode::Hblank => if ppu.dots >= 456 {
        self.ly_increase();

        let ppu = &mut self.ppu;
        if ppu.is_fetching_window {
          ppu.win_ly += 1;
        }

        if ppu.ly >= 144 {
          self.enter_vblank();
        } else {
          self.enter_mode2();
        }
      }

      Mode::Vblank => if ppu.dots >= 456 {
        self.ly_increase();

        let ppu = &mut self.ppu;
        if ppu.ly >= 154 {
          ppu.ly = 0;
          ppu.win_ly = 0;
          ppu.is_window_scanline_reached = false;
          self.enter_mode2();
        }
      }

      Mode::Disabled => {
        // if ppu.dots >= 456 {
        //   ppu.dots = 0;
        //   ppu.ly += 1;

        //   if ppu.ly == 144 {
        //     self.intf.set_vblank(true);
        //     self.frame_ready = true;
        //   } else if ppu.ly >= 154 {
        //     ppu.ly = 0;
        //   }
        // }
      }
    }
  }
}