use crate::emu::{GbEmulator, SCREEN_WIDTH};
use bitfields::{bitfield, bitflag};
use std::collections::{HashMap, VecDeque};

pub const DMG_PALETTE: [(u8, u8, u8); 4] =
    [(155, 188, 15), (139, 172, 15), (48, 98, 48), (15, 56, 15)];

pub const OAM_SCAN_DOTS: u16 = 80;
pub const SCANLINE_DOTS: u16 = 456;
pub const SCANLINES: u8 = 153;

#[bitfield(u8)]
pub struct Ctrl {
    bg_wnd_priority: bool,
    obj_enable: bool,
    obj_size: bool,
    bg_map: bool,
    bg_wnd_tiles: bool,
    wnd_enable: bool,
    wnd_map: bool,
    lcd_enable: bool,
}

#[bitflag(u8)]
#[derive(PartialEq)]
pub enum Mode {
    HBlank = 0,
    #[base]
    VBlank = 1,
    OAMScan = 2,
    Drawing = 3,
}

#[bitfield(u8)]
pub struct Stat {
    #[bits(2)]
    mode: Mode,
    lyc_eq_ly: bool,
    mode0_int: bool,
    mode1_int: bool,
    mode2_int: bool,
    lyc_int: bool,

    _unused: bool,
}

#[bitfield(u16)]
struct FifoPixel {
    #[bits(2)]
    color: u8,

    #[bits(3)]
    palette: u8,

    #[bits(4)]
    idx: u8,

    priority: bool,

    #[bits(6)]
    _unused: u8,
}

#[bitfield(u8)]
struct ObjAttr {
    #[bits(3)]
    cgb_palette: u8,
    bank: bool,
    dmg_palette: bool,
    flip_x: bool,
    flip_y: bool,
    priority: bool,
}

#[derive(Default, Clone)]
struct Object {
    y: u8,
    x: u8,
    tile_id: u8,
    attr: ObjAttr,
    idx: u8,
}
impl Object {
    pub fn new(bytes: &[u8], idx: u8) -> Self {
        Self {
            y: bytes[0],
            x: bytes[1],
            tile_id: bytes[2],
            attr: ObjAttr::from(bytes[3]),
            idx,
        }
    }
}

enum BgFetcherState {
    Idle,
    Wait { count: u8, scx: u8, scy: u8 },
    Push { tile_lo: u8, tile_hi: u8 },
    Discard { amt: u8 },
}

impl Default for BgFetcherState {
    fn default() -> Self {
        Self::start(0, 0)
    }
}

impl BgFetcherState {
    pub fn start(scx: u8, scy: u8) -> Self {
        Self::Wait { count: 6, scx, scy }
    }
}

#[derive(Default)]
enum ObjFetcherState {
    #[default]
    Idle,
    Wait {
        count: u8,
        obj_idx: u8,
    },
    Push {
        obj_idx: u8,
        tile_lo: u8,
        tile_hi: u8,
    },
}

#[derive(Default)]
pub struct Ppu {
    pub lcdc: Ctrl,
    pub ly: u8,
    pub lyc: u8,
    pub stat: Stat,
    pub scy: u8,
    pub scx: u8,
    pub wy: u8,
    pub wx: u8,

    wlc: u8,
    wy_eq_ly: bool,
    wnd_rendering: bool,

    stat_intr: bool,

    pub bgp: u8,
    pub obp0: u8,
    pub obp1: u8,

    pub dot: u16,
    pixel_idx: usize,

    fetch_coarse_x: u8,
    fetch_fine_x: i16,
    fetch_scrolled: bool,

    bg_fifo: VecDeque<FifoPixel>,
    bg_fetch: BgFetcherState,

    obj_buf: Vec<Object>,
    obj_pos_x: HashMap<i16, u8>,
    obj_fetch: ObjFetcherState,
    obj_fifo: VecDeque<FifoPixel>,
}

impl Ppu {
    pub fn new() -> Self {
        Self {
            fetch_fine_x: -8,
            ..Default::default()
        }
    }
}

impl GbEmulator {
    pub fn lcdc_write(&mut self, val: u8) {
        let ppu = &mut self.ppu;

        let was_enabled = ppu.lcdc.lcd_enable();
        ppu.lcdc = Ctrl::from_bits(val);

        // TODO
        // turn on
        if !was_enabled && ppu.lcdc.lcd_enable() {
        }
        // turn off
        else if was_enabled && !ppu.lcdc.lcd_enable() {
        }
    }

    fn oam_scan_tick(&mut self) {
        let ppu = &mut self.ppu;

        if ppu.dot % 2 == 1 || ppu.obj_buf.len() > 10 {
            return;
        }

        let obj_start = 4 * (ppu.dot as usize / 2);
        let obj = &self.bus.oam[obj_start..obj_start + 4];

        let y = obj[0];
        let obj_size = if ppu.lcdc.obj_size() { 16 } else { 8 };
        // let dist = (ppu.ly as i16 + 16) - y;
        if y <= ppu.ly + 16 && ppu.ly + 16 < y + obj_size {
            // if 0 <= dist && dist < obj_size {
            let obj = Object::new(obj, ppu.obj_buf.len() as u8);
            ppu.obj_buf.push(obj);
        }
    }

    fn fetch_bg(&mut self, scx: u8, scy: u8) -> (u8, u8) {
        let ppu = &mut self.ppu;

        let (x, y, tilemap) = if ppu.wnd_rendering {
            let tilemap = if ppu.lcdc.wnd_map() { 0x9c00 } else { 0x9800 };

            let x = ppu.fetch_coarse_x as u16;
            let y = 32 * (ppu.wlc as u16 / 8);

            (x, y, tilemap)
        } else {
            let tilemap = if ppu.lcdc.bg_map() { 0x9c00 } else { 0x9800 };

            let x = (scx as u16 / 8 + ppu.fetch_coarse_x as u16) % 32;
            let y_with_scroll = (ppu.ly as u16 + scy as u16) % 256; // y with scroll
            let y = 32 * (y_with_scroll / 8);
            (x, y, tilemap)
        };

        let offset = (y + x) % 1024;

        let tile_id = self.dispatch_read(tilemap | offset);

        let ppu = &mut self.ppu;
        let offset = if ppu.wnd_rendering {
            2 * (self.ppu.wlc as u16 % 8)
        } else {
            2 * ((self.ppu.ly as u16 + scy as u16) % 8)
        };

        let tile_start = if self.ppu.lcdc.bg_wnd_tiles() {
            // unsigned mode
            0x8000 | (16 * tile_id as u16)
        } else {
            // signed mode
            let offset = 16 * tile_id as i8 as i32;
            (0x9000 + offset) as u16
        };
        let tile_addr = tile_start | offset;

        let tile_lo = self.dispatch_read(tile_addr);
        let tile_hi = self.dispatch_read(tile_addr + 1);
        (tile_lo, tile_hi)
    }

    fn fetch_obj(&mut self, obj_idx: u8) -> (u8, u8) {
        let obj = &self.ppu.obj_buf[obj_idx as usize];
        let tile_id = obj.tile_id;

        let offset = 2 * (self.ppu.ly + 16 - obj.y) as u16;
        let tile_start = 0x8000 | (16 * tile_id as u16);
        let tile_addr = tile_start | offset;

        let tile_lo = self.dispatch_read(tile_addr);
        let tile_hi = self.dispatch_read(tile_addr + 1);

        (tile_lo, tile_hi)
    }

    fn drawing_tick(&mut self) {
        let ppu = &mut self.ppu;

        if !ppu.wnd_rendering
            && ppu.lcdc.bg_wnd_priority()
            && ppu.lcdc.wnd_enable()
            && ppu.wy_eq_ly
            && ppu.fetch_fine_x + 7 == ppu.wx as i16
        {
            // we've reached the window
            // a 6-dot penalty is incurred while the BG fetcher is being set up for the window.
            ppu.fetch_coarse_x = 0;
            ppu.bg_fetch = BgFetcherState::start(ppu.scx, ppu.scy);
            ppu.bg_fifo.clear();

            ppu.wnd_rendering = true;
        }

        if matches!(ppu.obj_fetch, ObjFetcherState::Idle) {
            if let Some(obj_idx) = ppu.obj_pos_x.get(&(ppu.fetch_fine_x + 8)).copied() {
                ppu.bg_fetch = BgFetcherState::Idle;
                ppu.obj_fetch = ObjFetcherState::Wait { count: 6, obj_idx };
            }
        }

        match ppu.bg_fetch {
            BgFetcherState::Idle => {}
            BgFetcherState::Wait { count, scx, scy } => {
                self.ppu.bg_fetch = if count == 0 {
                    if ppu.fetch_fine_x < 0 {
                        // first tile is discarded
                        ppu.fetch_fine_x += 8;
                        BgFetcherState::start(ppu.scx, ppu.scy)
                    } else {
                        let (tile_lo, tile_hi) = self.fetch_bg(scx, scy);
                        BgFetcherState::Push { tile_lo, tile_hi }
                    }
                } else {
                    BgFetcherState::Wait {
                        count: count - 1,
                        scx,
                        scy,
                    }
                }
            }

            BgFetcherState::Push { tile_lo, tile_hi } => {
                if ppu.bg_fifo.len() <= 8 {
                    // push instantly
                    for i in (0..8).rev() {
                        let lo = (tile_lo >> i) & 1;
                        let hi = (tile_hi >> i) & 1;
                        let color = (hi << 1) | lo;
                        let pixel = FifoPixelBuilder::new().with_color(color).build();

                        ppu.bg_fifo.push_back(pixel);
                    }

                    let scroll = ppu.scx % 8;
                    ppu.bg_fetch = if scroll > 0 && !ppu.fetch_scrolled {
                        // scroll scx % 8 pixels
                        BgFetcherState::Discard { amt: scroll - 1 }
                    } else {
                        BgFetcherState::start(ppu.scx, ppu.scy)
                    };

                    ppu.bg_fetch = BgFetcherState::start(ppu.scx, ppu.scy);

                    ppu.fetch_scrolled = true;
                    ppu.fetch_coarse_x += 1;
                }
            }
            BgFetcherState::Discard { amt } => {
                ppu.bg_fifo.pop_front();

                ppu.bg_fetch = if amt == 0 {
                    BgFetcherState::start(ppu.scx, ppu.scy)
                } else {
                    BgFetcherState::Discard { amt: amt - 1 }
                }
            }
        }

        let ppu = &mut self.ppu;
        match ppu.obj_fetch {
            ObjFetcherState::Idle => {}
            ObjFetcherState::Wait { count, obj_idx } => {
                self.ppu.obj_fetch = if count == 0 {
                    let (tile_lo, tile_hi) = self.fetch_obj(obj_idx);
                    ObjFetcherState::Push {
                        obj_idx,
                        tile_lo,
                        tile_hi,
                    }
                } else {
                    ObjFetcherState::Wait {
                        count: count - 1,
                        obj_idx,
                    }
                }
            }
            ObjFetcherState::Push {
                obj_idx,
                tile_lo,
                tile_hi,
            } => {
                for i in (0..8).rev() {
                    let lo = (tile_lo >> i) & 1;
                    let hi = (tile_hi >> i) & 1;
                    let color = (hi << 1) | lo;
                    let new_pixel = FifoPixelBuilder::new().with_color(color).build();

                    // if let Some(old_pixel) = ppu.obj_fifo.get_mut(i) {
                    //     if old_pixel.color() == 0 {
                    //         // *old_pixel = new_pixel;
                    //     }
                    // } else {
                    ppu.obj_fifo.push_back(new_pixel);
                    // }
                }

                ppu.obj_fetch = ObjFetcherState::Idle;
                ppu.obj_pos_x.remove(&(&ppu.fetch_fine_x + 8));

                ppu.bg_fetch = BgFetcherState::start(ppu.scx, ppu.scy);
            }
        }

        let ppu = &mut self.ppu;
        if ppu.bg_fifo.len() > 8
            && ppu.fetch_fine_x >= 0
            && matches!(ppu.obj_fetch, ObjFetcherState::Idle)
        {
            let bg_pixel = ppu.bg_fifo.pop_front().unwrap();
            let obj_pixel = ppu.obj_fifo.pop_front().unwrap_or(0.into());

            if ppu.lcdc.obj_enable() && obj_pixel.color() > 0 {
                // TODO: obj palette
                self.push_pixel(obj_pixel.color());
            } else if ppu.lcdc.bg_wnd_priority() {
                let color_id = (ppu.bgp >> (2 * bg_pixel.color())) & 0x3;
                self.push_pixel(color_id);
            } else {
                self.push_pixel(0);
            }

            self.ppu.fetch_fine_x += 1;
        }
    }

    fn push_pixel(&mut self, color_id: u8) {
        let color = DMG_PALETTE[color_id as usize];
        self.output.videobuf_back.0[self.ppu.pixel_idx + 0] = color.0;
        self.output.videobuf_back.0[self.ppu.pixel_idx + 1] = color.1;
        self.output.videobuf_back.0[self.ppu.pixel_idx + 2] = color.2;
        self.ppu.pixel_idx += 4;
    }

    fn update_stat_intr(&mut self) {
        let ppu = &mut self.ppu;

        ppu.stat.set_lyc_eq_ly(ppu.ly == ppu.lyc);

        let intr = (ppu.stat.lyc_int() && ppu.stat.lyc_eq_ly())
            || (ppu.stat.mode0_int() && ppu.stat.mode() == Mode::HBlank)
            || (ppu.stat.mode1_int() && ppu.stat.mode() == Mode::VBlank)
            || (ppu.stat.mode2_int() && ppu.stat.mode() == Mode::OAMScan);

        if !ppu.stat_intr && intr {
            self.bus.intf.set_lcd(true);
        }

        self.ppu.stat_intr = intr;
    }

    fn enter_scanline(&mut self) {
        let ppu = &mut self.ppu;

        ppu.stat.set_mode(Mode::OAMScan);
        // we wait for the first 6 steps, then fetch and push on the 7th;
        ppu.bg_fetch = BgFetcherState::start(ppu.scx, ppu.scy);
        ppu.bg_fifo.clear();

        ppu.obj_fetch = ObjFetcherState::Idle;
        ppu.obj_fifo.clear();
        ppu.obj_buf.clear();
        ppu.obj_pos_x.clear();

        ppu.fetch_scrolled = false;
        ppu.fetch_coarse_x = 0;
        ppu.fetch_fine_x = -8;
        ppu.wnd_rendering = false;

        if ppu.wy == ppu.ly {
            ppu.wy_eq_ly = true;
        }
    }

    fn enter_hblank(&mut self) {
        let ppu = &mut self.ppu;
        ppu.stat.set_mode(Mode::HBlank);
    }

    fn enter_vblank(&mut self) {
        let ppu = &mut self.ppu;

        ppu.stat.set_mode(Mode::VBlank);
        self.bus.intf.set_vblank(true);

        std::mem::swap(
            &mut self.output.videobuf_view,
            &mut self.output.videobuf_back,
        );

        self.output.frame_ready = true;
    }

    pub fn ppu_step(&mut self) {
        let ppu = &mut self.ppu;

        match ppu.stat.mode() {
            Mode::OAMScan => {
                self.oam_scan_tick();

                let ppu = &mut self.ppu;
                ppu.dot += 1;
                if ppu.dot >= OAM_SCAN_DOTS {
                    ppu.obj_buf
                        .sort_by(|a, b| a.x.cmp(&b.x).then(a.idx.cmp(&b.idx)));

                    ppu.obj_pos_x.extend(
                        ppu.obj_buf
                            .iter()
                            .enumerate()
                            .map(|(idx, obj)| (obj.x as i16, idx as u8)),
                    );

                    ppu.stat.set_mode(Mode::Drawing);
                }
            }
            Mode::Drawing => {
                self.drawing_tick();

                self.ppu.dot += 1;
                if self.ppu.fetch_fine_x >= SCREEN_WIDTH as i16 {
                    self.enter_hblank();
                }
            }
            Mode::HBlank => {
                ppu.dot += 1;

                if ppu.dot >= SCANLINE_DOTS {
                    ppu.dot = 0;
                    ppu.ly += 1;

                    if ppu.wnd_rendering {
                        ppu.wlc += 1;
                    }

                    if ppu.ly >= 144 {
                        self.enter_vblank();
                    } else {
                        self.enter_scanline();
                    }
                }
            }
            Mode::VBlank => {
                ppu.dot += 1;

                if ppu.dot >= SCANLINE_DOTS {
                    ppu.dot = 0;
                    ppu.ly += 1;

                    if ppu.ly >= SCANLINES {
                        ppu.ly = 0;
                        ppu.wlc = 0;
                        ppu.pixel_idx = 0;
                        ppu.wy_eq_ly = false;

                        self.enter_scanline();
                    }
                }
            }
        }

        self.update_stat_intr();
    }
}
