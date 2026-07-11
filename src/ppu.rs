use crate::{
    bus::Bus,
    emu::{FRAMEBUF_SIZE, GbEmulator, SCREEN_HEIGHT, SCREEN_WIDTH},
};
use bitfields::{bitfield, bitflag};
use std::collections::{HashMap, VecDeque};

pub const DMG_PALETTE_RGB888: [(u8, u8, u8); 4] =
    [(155, 188, 15), (139, 172, 15), (48, 98, 48), (15, 56, 15)];

fn from_rgb888_to_rgb555((r8, g8, b8): (u8, u8, u8)) -> (u8, u8) {
    let (r8, g8, b8) = (r8 as u32, g8 as u32, b8 as u32);

    let r5 = (r8 >> 3) & 0x1f;
    let g5 = (g8 >> 3) & 0x1f;
    let b5 = (b8 >> 3) & 0x1f;

    let res = (b5 << 10) | (g5 << 5) | r5;
    (res as u8, (res >> 8) as u8)
}

fn from_rgb555_to_rgb888(color_lo: u8, color_hi: u8) -> (u8, u8, u8) {
    let color16 = ((color_hi as u32) << 8) | (color_lo as u32);

    let r5 = color16 & 0x1f;
    let g5 = (color16 >> 5) & 0x1f;
    let b5 = (color16 >> 10) & 0x1f;

    let r8 = r5 * 255 / 31;
    let g8 = g5 * 255 / 31;
    let b8 = b5 * 255 / 31;

    (r8 as u8, g8 as u8, b8 as u8)
}

pub const OAM_SCAN_DOTS: u16 = 80;
pub const SCANLINE_DOTS: u16 = 456;
pub const SCANLINES: u8 = 154;

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

    #[bits(1, default = true)]
    _unused: bool,
}

#[bitfield(u8)]
struct FifoPixel {
    #[bits(2)]
    color: u8,
    dmg_palette: bool,
    #[bits(3)]
    cgb_palette: u8,
    priority: bool,
    _unused: bool,
}

#[bitfield(u8)]
struct Attr {
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
    attr: Attr,
    idx: u8,
}
impl Object {
    pub fn new(bytes: &[u8], idx: u8) -> Self {
        Self {
            y: bytes[0],
            x: bytes[1],
            tile_id: bytes[2],
            attr: Attr::from(bytes[3]),
            idx,
        }
    }
}

enum BgFetcherState {
    Idle,
    Wait {
        count: u8,
        scx: u8,
        scy: u8,
    },
    Push {
        tile_lo: u8,
        tile_hi: u8,
        attr: Attr,
    },
    Discard {
        amt: u8,
    },
}

impl Default for BgFetcherState {
    fn default() -> Self {
        Self::start(0, 0)
    }
}

impl BgFetcherState {
    pub fn start(scx: u8, scy: u8) -> Self {
        Self::Wait {
            count: 6,
            scx: scx,
            scy,
        }
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

pub struct PaletteRam(pub [u8; 64]);
impl Default for PaletteRam {
    fn default() -> Self {
        Self([0; _])
    }
}
impl PaletteRam {
    pub fn new(is_cgb: bool) -> Self {
        let mut res = [0; _];

        if !is_cgb {
            for (idx, color) in DMG_PALETTE_RGB888.iter().copied().enumerate() {
                let (lo, hi) = from_rgb888_to_rgb555(color);

                res[2 * idx + 0] = lo;
                res[2 * idx + 1] = hi;
            }
        }

        Self(res)
    }

    pub fn rgba888(&self, idx: u8) -> (u8, u8, u8) {
        let idx = idx * 2;
        from_rgb555_to_rgb888(self.0[idx as usize], self.0[idx as usize | 1])
    }
}

#[bitfield(u8)]
pub struct PaletteIdx {
    #[bits(6)]
    address: u8,
    _unused: bool,
    auto_incr: bool,
}

#[derive(Default)]
pub struct Ppu {
    pub lcdc: Ctrl,
    pub ly: u8,
    pub ly_read: u8,
    pub lyc: u8,
    pub stat: Stat,
    pub scy: u8,
    pub scx: u8,
    pub wy: u8,
    pub wx: u8,
    pub bgp: u8,
    pub obp0: u8,
    pub obp1: u8,

    pub bg_palettes: PaletteRam,
    pub obj_palettes: PaletteRam,
    pub bgpi: PaletteIdx,
    pub obpi: PaletteIdx,

    wlc: u8,
    wy_eq_ly: bool,
    wnd_rendering: bool,

    stat_intr: bool,

    dot: u16,
    pixel_idx: usize,

    fetch_coarse_x: u8,
    fetch_fine_x: u16,
    fetch_scrolled: Option<u8>,

    bg_fifo: VecDeque<FifoPixel>,
    bg_fetch: BgFetcherState,

    obj_buf: Vec<Object>,
    obj_pos_x: HashMap<u16, u8>,
    obj_fetch: ObjFetcherState,
    obj_fifo: VecDeque<FifoPixel>, // TODO: this should always have 8 transparent pixels ready!!
}

impl Ppu {
    pub fn new(is_cgb_model: bool) -> Self {
        Self {
            bg_palettes: PaletteRam::new(false),
            obj_palettes: PaletteRam::new(false),
            ..Default::default()
        }
    }

    // TODO: cache this
    pub fn obj_size(&self) -> u8 {
        if self.lcdc.obj_size() { 16 } else { 8 }
    }

    fn palette_read(&mut self, bg: bool) -> u8 {
        let (ram, pi) = if bg {
            (&self.bg_palettes, self.bgpi)
        } else {
            (&self.obj_palettes, self.obpi)
        };

        if self.stat.mode() != Mode::Drawing {
            ram.0[pi.address() as usize]
        } else {
            0xff
        }
    }

    pub fn bg_palette_read(&mut self) -> u8 {
        self.palette_read(true)
    }

    pub fn obj_palette_read(&mut self) -> u8 {
        self.palette_read(false)
    }

    pub fn palette_write(&mut self, val: u8, bg: bool) {
        let (ram, pi) = if bg {
            (&mut self.bg_palettes, &mut self.bgpi)
        } else {
            (&mut self.obj_palettes, &mut self.obpi)
        };

        let pal_addr = pi.address();
        if self.stat.mode() != Mode::Drawing {
            ram.0[pal_addr as usize] = val;
        }

        // BGPI’s “address” field is automatically incremented (wrapping around from 63 back to 0) after each write to this register, even if the write fails due to CRAM being inaccessible
        if pi.auto_incr() {
            pi.set_address(pal_addr + 1);
        }
    }

    pub fn bg_palette_write(&mut self, val: u8) {
        self.palette_write(val, true)
    }

    pub fn obj_palette_write(&mut self, val: u8) {
        self.palette_write(val, false)
    }
}

impl GbEmulator {
    pub fn lcdc_write(&mut self, val: u8) {
        let ppu = &mut self.ppu;

        let was_enabled = ppu.lcdc.lcd_enable();
        ppu.lcdc = Ctrl::from_bits_with_defaults(val);

        // turn on
        if !was_enabled && ppu.lcdc.lcd_enable() {
            ppu.pixel_idx = 0;
            self.enter_scanline();
        }
        // turn off
        else if was_enabled && !ppu.lcdc.lcd_enable() {
            ppu.stat.set_mode(Mode::HBlank);
            ppu.stat.set_lyc_eq_ly(false);

            ppu.pixel_idx = 0;
            ppu.dot = 0;
            ppu.ly = 0;
            ppu.ly_read = 0;
            ppu.wlc = 0;
            ppu.wy_eq_ly = false;
            ppu.pixel_idx = 0;
            ppu.wnd_rendering = false;
            ppu.fetch_coarse_x = 0;
            ppu.fetch_fine_x = 0;
            ppu.fetch_scrolled = None;
            ppu.bg_fifo.clear();
            ppu.obj_fifo.clear();
            ppu.obj_buf.clear();
            ppu.obj_pos_x.clear();

            for _ in 0..SCREEN_WIDTH * SCREEN_HEIGHT {
                self.push_pixel(0, |p| &p.bg_palettes);
            }
            self.ppu.pixel_idx = 0;
            std::mem::swap(
                &mut self.output.videobuf_back,
                &mut self.output.videobuf_view,
            );
        }
    }

    fn oam_scan_tick(&mut self) {
        let ppu = &mut self.ppu;

        if ppu.dot % 2 == 1 || ppu.obj_buf.len() >= 10 {
            return;
        }

        let obj_idx = ppu.dot as usize / 2;
        let obj_start = 4 * obj_idx;
        let obj = &self.bus.oam[obj_start..obj_start + 4];

        let y = obj[0];
        if y <= ppu.ly + 16 && ppu.ly + 16 < y + ppu.obj_size() {
            let obj = Object::new(obj, obj_idx as u8);
            ppu.obj_buf.push(obj);
        }
    }

    fn fetch_bg_tile(&mut self, scx: u8, scy: u8) -> (u8, u8, Attr) {
        let is_cgb = self.is_cgb();
        let ppu = &mut self.ppu;

        let (x, y, row, tilemap) = if ppu.wnd_rendering {
            let tilemap = if ppu.lcdc.wnd_map() { 0x9c00 } else { 0x9800 };

            let x = ppu.fetch_coarse_x as u16;
            let y = 32 * (ppu.wlc as u16 / 8);
            let row = ppu.wlc as u16 % 8;

            (x, y, row, tilemap)
        } else {
            let tilemap = if ppu.lcdc.bg_map() { 0x9c00 } else { 0x9800 };

            let x = (scx as u16 / 8 + ppu.fetch_coarse_x as u16) % 32;
            let y_with_scroll = ppu.ly as u16 + scy as u16; // y with scroll
            let y = 32 * ((y_with_scroll % 256) / 8);
            let row = y_with_scroll % 8;
            (x, y, row, tilemap)
        };

        let tilemap_offset = (y + x) % 1024;
        let tilemap_addr = tilemap | tilemap_offset;
        let tile_id = self.bus.vram0(tilemap_addr);

        let tile_start = if self.ppu.lcdc.bg_wnd_tiles() {
            // unsigned mode
            0x8000 | (16 * tile_id as u16)
        } else {
            // signed mode
            let offset = 16 * tile_id as i8 as i32;
            (0x9000 + offset) as u16
        };

        let attr = if is_cgb {
            Attr::from_bits(self.bus.vram1(tilemap_addr))
        } else {
            Attr::new()
        };

        let offset = if attr.flip_y() {
            2 * (7 - row)
        } else {
            2 * row
        };
        let tile_addr = tile_start | offset;
        let tile_fetch = if attr.bank() { Bus::vram1 } else { Bus::vram0 };

        let mut tile_lo = tile_fetch(&self.bus, tile_addr);
        let mut tile_hi = tile_fetch(&self.bus, tile_addr + 1);

        if attr.flip_x() {
            tile_lo = tile_lo.reverse_bits();
            tile_hi = tile_hi.reverse_bits();
        }

        (tile_lo, tile_hi, attr)
    }

    fn fetch_obj_tile(&mut self, obj_idx: u8) -> (u8, u8) {
        let is_cgb = self.is_cgb();
        let ppu = &self.ppu;

        let obj = &ppu.obj_buf[obj_idx as usize];
        let mut tile_id = obj.tile_id;
        if ppu.lcdc.obj_size() {
            tile_id &= !1;
        }

        let row = (self.ppu.ly + 16 - obj.y) as u16;
        let offset = if obj.attr.flip_y() {
            2 * (ppu.obj_size() as u16 - 1 - row)
        } else {
            2 * row
        };

        let tile_start = 0x8000 | (16 * tile_id as u16);
        let tile_addr = tile_start | offset;
        let flip_x = obj.attr.flip_x();

        let tile_fetch = if obj.attr.bank() && is_cgb {
            Bus::vram1
        } else {
            Bus::vram0
        };

        let mut tile_lo = tile_fetch(&self.bus, tile_addr);
        let mut tile_hi = tile_fetch(&self.bus, tile_addr + 1);

        if !flip_x {
            tile_lo = tile_lo.reverse_bits();
            tile_hi = tile_hi.reverse_bits();
        }

        (tile_lo, tile_hi)
    }

    fn bg_fetch_tick(&mut self) {
        let ppu = &mut self.ppu;

        match ppu.bg_fetch {
            BgFetcherState::Idle => {}
            BgFetcherState::Wait { count, scx, scy } => {
                self.ppu.bg_fetch = if count == 0 {
                    let (tile_lo, tile_hi, attr) = self.fetch_bg_tile(scx, scy);
                    BgFetcherState::Push {
                        tile_lo,
                        tile_hi,
                        attr,
                    }
                } else {
                    BgFetcherState::Wait {
                        count: count - 1,
                        scx,
                        scy,
                    }
                }
            }

            BgFetcherState::Push {
                tile_lo,
                tile_hi,
                attr,
            } => {
                if ppu.bg_fifo.len() <= 8 {
                    // push instantly
                    for i in (0..8).rev() {
                        let lo = (tile_lo >> i) & 1;
                        let hi = (tile_hi >> i) & 1;
                        let color = (hi << 1) | lo;
                        let pixel = FifoPixelBuilder::new()
                            .with_color(color)
                            .with_priority(attr.priority())
                            .with_cgb_palette(attr.cgb_palette())
                            .build();

                        ppu.bg_fifo.push_back(pixel);
                    }

                    ppu.bg_fetch = if let Some(scroll) = ppu.fetch_scrolled.take() {
                        // scroll scx % 8 pixels
                        BgFetcherState::Discard { amt: scroll - 1 }
                    } else {
                        BgFetcherState::start(ppu.scx, ppu.scy)
                    };

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
    }

    fn obj_fetch_tick(&mut self) {
        let ppu = &mut self.ppu;
        match ppu.obj_fetch {
            ObjFetcherState::Idle => {}
            ObjFetcherState::Wait { count, obj_idx } => {
                self.ppu.obj_fetch = if count == 0 {
                    let (tile_lo, tile_hi) = self.fetch_obj_tile(obj_idx);
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
                let obj = &ppu.obj_buf[obj_idx as usize];

                // if the OAM FIFO doesn’t have at least 8 pixels in it then transparent pixels with the lowest priority are pushed onto the OAM FIFO.
                while ppu.obj_fifo.len() < 8 {
                    ppu.obj_fifo.push_back(0.into());
                }

                for i in (0..8).rev() {
                    let lo = (tile_lo >> i) & 1;
                    let hi = (tile_hi >> i) & 1;
                    let color = (hi << 1) | lo;

                    let new_pixel = FifoPixelBuilder::new()
                        .with_color(color)
                        .with_priority(obj.attr.priority())
                        .with_dmg_palette(obj.attr.dmg_palette())
                        .with_cgb_palette(obj.attr.cgb_palette())
                        .build();

                    if let Some(old_pixel) = ppu.obj_fifo.get_mut(i) {
                        if old_pixel.color() == 0 {
                            *old_pixel = new_pixel;
                        }
                    } else {
                        ppu.obj_fifo.push_back(new_pixel);
                    }
                }

                ppu.obj_fetch = ObjFetcherState::Idle;
                ppu.obj_pos_x.remove(&(&ppu.fetch_fine_x + 8));

                ppu.bg_fetch = BgFetcherState::start(ppu.scx, ppu.scy);
            }
        }
    }

    fn render_pixel(&mut self) {
        let is_cgb = !self.sys.priority_mode;

        let ppu = &mut self.ppu;
        if ppu.bg_fifo.len() > 8 && matches!(ppu.obj_fetch, ObjFetcherState::Idle) {
            let bg_pixel = ppu.bg_fifo.pop_front().unwrap();
            let obj_pixel = ppu.obj_fifo.pop_front().unwrap_or(0.into());

            if is_cgb {
                // CGB rendering
                let obj_has_priority = bg_pixel.color() == 0
                    || !ppu.lcdc.bg_wnd_priority()
                    || (!bg_pixel.priority() && !obj_pixel.priority());

                if ppu.lcdc.obj_enable() && obj_pixel.color() > 0 && obj_has_priority {
                    let palette_idx = 4 * obj_pixel.cgb_palette() + obj_pixel.color();
                    self.push_pixel(palette_idx, |p| &p.obj_palettes);
                } else {
                    let palette_idx = 4 * bg_pixel.cgb_palette() + bg_pixel.color();
                    self.push_pixel(palette_idx, |p| &p.bg_palettes);
                }
            } else {
                // DMG rendering
                if ppu.lcdc.obj_enable()
                    && obj_pixel.color() > 0
                    && (!obj_pixel.priority() || bg_pixel.color() == 0)
                {
                    let pal = if obj_pixel.dmg_palette() {
                        ppu.obp1
                    } else {
                        ppu.obp0
                    };

                    let palette_idx = (pal >> (2 * obj_pixel.color())) & 0x3;
                    self.push_pixel(palette_idx, |p| &p.obj_palettes);
                } else if ppu.lcdc.bg_wnd_priority() {
                    let palette_idx = (ppu.bgp >> (2 * bg_pixel.color())) & 0x3;
                    self.push_pixel(palette_idx, |p| &p.bg_palettes);
                } else {
                    self.push_pixel(0, |p| &p.bg_palettes);
                }
            }

            self.ppu.fetch_fine_x += 1;
        }
    }

    fn push_pixel(&mut self, idx: u8, pal: fn(&Ppu) -> &PaletteRam) {
        let color = pal(&self.ppu).rgba888(idx);

        self.output.videobuf_back.0[self.ppu.pixel_idx + 0] = color.0;
        self.output.videobuf_back.0[self.ppu.pixel_idx + 1] = color.1;
        self.output.videobuf_back.0[self.ppu.pixel_idx + 2] = color.2;
        self.ppu.pixel_idx += 4;
    }

    fn drawing_tick(&mut self) {
        let is_cgb = self.is_cgb();
        let ppu = &mut self.ppu;

        // window collision check
        if !ppu.wnd_rendering
            && (ppu.lcdc.bg_wnd_priority() || is_cgb)
            && ppu.lcdc.wnd_enable()
            && ppu.wy_eq_ly
            && ppu.fetch_fine_x as i16 >= ppu.wx as i16 - 7
        {
            // we've reached the window
            ppu.fetch_coarse_x = 0;
            ppu.bg_fetch = BgFetcherState::start(ppu.scx, ppu.scy);
            ppu.fetch_scrolled = (ppu.wx < 7).then_some(7 - ppu.wx);
            ppu.bg_fifo.clear();

            ppu.wnd_rendering = true;
        }

        // object collision check
        if matches!(ppu.obj_fetch, ObjFetcherState::Idle) {
            if let Some(obj_idx) = ppu.obj_pos_x.get(&(ppu.fetch_fine_x + 8)).copied() {
                ppu.bg_fetch = BgFetcherState::Idle;
                ppu.obj_fetch = ObjFetcherState::Wait { count: 6, obj_idx };
            }
        }

        self.bg_fetch_tick();
        self.obj_fetch_tick();

        self.render_pixel();
    }

    fn update_stat_intr(&mut self) {
        let ppu = &mut self.ppu;

        ppu.stat.set_lyc_eq_ly(ppu.ly_read == ppu.lyc);

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
        ppu.fetch_scrolled = if (ppu.scx % 8) > 0 {
            Some(ppu.scx % 8)
        } else {
            None
        };
        ppu.wnd_rendering = false;

        ppu.obj_fetch = ObjFetcherState::Idle;
        ppu.obj_fifo.clear();
        ppu.obj_buf.clear();
        ppu.obj_pos_x.clear();

        ppu.fetch_coarse_x = 0;
        ppu.fetch_fine_x = 0;

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
        if !ppu.lcdc.lcd_enable() {
            return;
        }

        self.update_stat_intr();

        let ppu = &mut self.ppu;
        match ppu.stat.mode() {
            Mode::OAMScan => {
                self.oam_scan_tick();

                let ppu = &mut self.ppu;
                ppu.dot += 1;
                if ppu.dot >= OAM_SCAN_DOTS {
                    let is_cgb = self.is_cgb();
                    let ppu = &mut self.ppu;

                    if !is_cgb {
                        // DMG priority mode
                        ppu.obj_buf
                            .sort_by(|a, b| a.x.cmp(&b.x).then(b.idx.cmp(&a.idx)));
                    } else {
                        // CGB priority mode
                        ppu.obj_buf.sort_by(|a, b| b.idx.cmp(&a.idx))
                    }

                    ppu.obj_pos_x.extend(
                        ppu.obj_buf
                            .iter()
                            .enumerate()
                            .map(|(idx, obj)| (obj.x as u16, idx as u8)),
                    );

                    ppu.stat.set_mode(Mode::Drawing);
                }
            }
            Mode::Drawing => {
                self.drawing_tick();

                self.ppu.dot += 1;
                if self.ppu.fetch_fine_x >= SCREEN_WIDTH as u16 {
                    self.enter_hblank();
                }
            }
            Mode::HBlank => {
                ppu.dot += 1;

                if ppu.dot >= SCANLINE_DOTS {
                    ppu.dot = 0;
                    ppu.ly += 1;
                    ppu.ly_read += 1;

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

                if ppu.ly_read == 153 && ppu.dot > 4 {
                    // https://github.com/Ashiepaws/GBEDG/blob/master/bugs/index.md#flickering-line-in-intro-sequence
                    // scanline 153 quirk
                    ppu.ly_read = 0;
                }

                if ppu.dot >= SCANLINE_DOTS {
                    ppu.dot = 0;
                    ppu.ly += 1;
                    ppu.ly_read += 1;

                    if ppu.ly >= SCANLINES {
                        ppu.ly = 0;
                        ppu.ly_read = 0;
                        ppu.wlc = 0;
                        ppu.pixel_idx = 0;
                        ppu.wy_eq_ly = false;

                        self.enter_scanline();
                    }
                }
            }
        }
    }
}
