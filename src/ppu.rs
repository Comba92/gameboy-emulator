use crate::emu::GbEmulator;
use bitfields::{bitfield, bitflag};
use std::collections::VecDeque;

pub const DMG_PALETTE: [(u8, u8, u8); 4] =
    [(155, 188, 15), (139, 172, 15), (48, 98, 48), (15, 56, 15)];

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

    #[bits(6)]
    idx: u8,

    priority: bool,

    #[bits(4)]
    _unused: u8,
}

#[derive(Default)]
enum FifoState {
    #[default]
    Init,
    Tile,
    DataLoWait {
        tile_id: u8,
    },
    DataLo {
        tile_id: u8,
    },
    DataHiWait {
        tile_addr: u16,
        tile_lo: u8,
    },
    DataHi {
        tile_addr: u16,
        tile_lo: u8,
    },
    Discard {
        count: u8,
    },
    Push {
        tile_lo: u8,
        tile_hi: u8,
    },
}

#[derive(Default)]
struct Fetcher {
    bg_state: FifoState,
    bg_pos: u8,
    wnd: bool,
    bg_fifo: VecDeque<FifoPixel>,

    obj_state: FifoState,
    obj_pos: u8,
    obj_fifo: VecDeque<FifoPixel>,
}
impl Fetcher {
    pub fn reset_obj(&mut self) {
        self.bg_state = FifoState::Init;
        self.bg_pos = 0;
        self.wnd = false;
        self.bg_fifo.clear();
    }
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
}
impl Object {
    pub fn new(bytes: &[u8]) -> Self {
        Self {
            y: bytes[0],
            x: bytes[1],
            tile_id: bytes[2],
            attr: ObjAttr::from(bytes[3]),
        }
    }
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
    stat_intr: bool,

    pub bgp: u8,
    pub obp0: u8,
    pub obp1: u8,

    pub dot: i16,
    pixel_idx: usize,

    shifter_paused: bool,
    shifter_pos: u8,

    fetcher: Fetcher,
    objs: [Object; 10],
    objs_count: u8,
}

impl Ppu {
    pub fn new() -> Self {
        // TODO: consider keeping default impl
        // Self {
        //     lcdc: Ctrl::new(),
        //     ly: 0,
        //     lyc: 0,
        //     stat: Stat::new(),
        //     scy: 0,
        //     scx: 0,
        //     wy: 0,
        //     wx: 0,
        //     wlc: 0,
        //     wy_eq_ly: false,
        //     stat_intr: false,

        //     bgp: 0,
        //     obp0: 0,
        //     obp1: 0,
        //     dot: 0,
        //     pixel_idx: 0,
        //     shifter_pos: 0,

        //     fetcher: Fetcher::default(),
        //     objs: std::array::from_fn(|_| Object::default()),
        //     objs_count: 0,
        // }
        Self::default()
    }

    pub fn tile_addr(&self, tile_id: u8) -> u16 {
        if self.lcdc.bg_wnd_tiles() {
            // unsigned mode
            0x8000 | (16 * tile_id as u16)
        } else {
            // signed mode
            let offset = 16 * tile_id as i8 as i32;
            (0x9000 + offset) as u16
        }
    }
}

impl GbEmulator {
    fn oam_scan_tick(&mut self) {
        let ppu = &mut self.ppu;

        if ppu.dot % 2 == 0 {
            return;
        }

        let obj_idx = ppu.dot as usize / 2;
        let obj = &self.bus.oam[obj_idx..obj_idx + 4];

        let y = obj[0] as i16;
        let obj_size = if ppu.lcdc.obj_size() { 16 } else { 8 };
        let dist = (ppu.ly as i16 + 16) - y;
        // if y <= ppu.ly + 16 && ppu.ly + 16 < y + obj_size {
        if 0 <= dist && dist < obj_size && (ppu.objs_count as usize) < ppu.objs.len() {
            ppu.objs[ppu.objs_count as usize] = Object::new(obj);
            ppu.objs_count += 1;
        }
    }

    fn bg_fetcher_tick(&mut self) {
        use FifoState::*;
        let fetcher = &mut self.ppu.fetcher;

        match fetcher.bg_state {
            Init => fetcher.bg_state = Tile,
            Tile => {
                let (x, y, tilemap) = if fetcher.wnd {
                    let tilemap = if self.ppu.lcdc.wnd_map() {
                        0x9c00
                    } else {
                        0x9800
                    };

                    let x = fetcher.bg_pos;
                    let y = 32 * (self.ppu.wlc as u16 / 8);

                    (x, y, tilemap)
                } else {
                    let tilemap = if self.ppu.lcdc.bg_map() {
                        0x9c00
                    } else {
                        0x9800
                    };

                    let x = (self.ppu.scx / 8 + fetcher.bg_pos) % 32;
                    let y_with_scroll = (self.ppu.ly as u16 + self.ppu.scy as u16) % 256; // y with scroll
                    let y = 32 * (y_with_scroll / 8);
                    (x, y, tilemap)
                };

                let offset = (y + x as u16) % 1024;

                let tile_id = self.dispatch_read(tilemap | offset);
                self.ppu.fetcher.bg_state = DataLoWait { tile_id }
            }

            DataLoWait { tile_id } => fetcher.bg_state = DataLo { tile_id },
            DataLo { tile_id } => {
                let offset = if fetcher.wnd {
                    2 * (self.ppu.wlc as u16 % 8)
                } else {
                    2 * ((self.ppu.ly as u16 + self.ppu.scy as u16) % 8)
                };
                let tile_start = self.ppu.tile_addr(tile_id);
                let tile_addr = tile_start | offset;

                let tile_lo = self.dispatch_read(tile_addr);
                self.ppu.fetcher.bg_state = DataHiWait { tile_addr, tile_lo };
            }

            DataHiWait { tile_addr, tile_lo } => fetcher.bg_state = DataHi { tile_addr, tile_lo },
            DataHi { tile_addr, tile_lo } => {
                let tile_hi = self.dispatch_read(tile_addr + 1);

                if self.ppu.dot <= 86 {
                    // discard first fetched tile
                    self.ppu.fetcher.bg_state = Init;
                } else {
                    self.ppu.fetcher.bg_state = Push { tile_lo, tile_hi };
                }
            }

            Discard { count } => {
                if count > 0 {
                    fetcher.bg_fifo.pop_front();
                    fetcher.bg_state = Discard { count: count - 1 };
                } else {
                    fetcher.bg_state = Init;
                }
            }

            Push { tile_lo, tile_hi } => {
                if fetcher.bg_fifo.len() <= 8 {
                    for i in (0..8).rev() {
                        let lo = (tile_lo >> i) & 1;
                        let hi = (tile_hi >> i) & 1;
                        let color = (hi << 1) | lo;
                        let pixel = FifoPixelBuilder::new()
                            .with_color(color)
                            .with_palette(self.ppu.bgp)
                            .build();

                        fetcher.bg_fifo.push_back(pixel);
                    }

                    if self.ppu.scx % 8 > 0 && self.ppu.dot <= 92 {
                        // discard scx % 8 pixels of first tile pushed
                        fetcher.bg_state = Discard {
                            count: self.ppu.scx % 8,
                        };
                    } else {
                        fetcher.bg_state = Init;
                    }

                    fetcher.bg_pos += 1;
                }
            }
        }
    }

    fn obj_fetcher_tick(&mut self) {
        use FifoState::*;
        let fetcher = &mut self.ppu.fetcher;

        match fetcher.obj_state {
            Init => fetcher.obj_state = Tile,
            Tile => {
                let (x, y, tilemap) = if fetcher.wnd {
                    let tilemap = if self.ppu.lcdc.wnd_map() {
                        0x9c00
                    } else {
                        0x9800
                    };

                    let x = fetcher.bg_pos;
                    let y = 32 * (self.ppu.wlc as u16 / 8);

                    (x, y, tilemap)
                } else {
                    let tilemap = if self.ppu.lcdc.bg_map() {
                        0x9c00
                    } else {
                        0x9800
                    };

                    let x = (self.ppu.scx / 8 + fetcher.bg_pos) % 32;
                    let y_with_scroll = (self.ppu.ly as u16 + self.ppu.scy as u16) % 256; // y with scroll
                    let y = 32 * (y_with_scroll / 8);
                    (x, y, tilemap)
                };

                let offset = (y + x as u16) % 1024;

                let tile_id = self.dispatch_read(tilemap | offset);
                self.ppu.fetcher.obj_state = DataLoWait { tile_id }
            }

            DataLoWait { tile_id } => fetcher.obj_state = DataLo { tile_id },
            DataLo { tile_id } => {
                let offset = if fetcher.wnd {
                    2 * (self.ppu.wlc as u16 % 8)
                } else {
                    2 * ((self.ppu.ly as u16 + self.ppu.scy as u16) % 8)
                };
                let tile_start = self.ppu.tile_addr(tile_id);
                let tile_addr = tile_start | offset;

                let tile_lo = self.dispatch_read(tile_addr);
                self.ppu.fetcher.obj_state = DataHiWait { tile_addr, tile_lo };
            }

            DataHiWait { tile_addr, tile_lo } => fetcher.obj_state = DataHi { tile_addr, tile_lo },
            DataHi { tile_addr, tile_lo } => {
                let tile_hi = self.dispatch_read(tile_addr + 1);

                if self.ppu.dot <= 86 {
                    // discard first fetched tile
                    self.ppu.fetcher.obj_state = Init;
                } else {
                    self.ppu.fetcher.obj_state = Push { tile_lo, tile_hi };
                }
            }

            Discard { count } => {
                if count > 0 {
                    fetcher.bg_fifo.pop_front();
                    fetcher.obj_state = Discard { count: count - 1 };
                } else {
                    fetcher.obj_state = Init;
                }
            }

            Push { tile_lo, tile_hi } => {
                if fetcher.bg_fifo.len() <= 8 {
                    for i in (0..8).rev() {
                        let lo = (tile_lo >> i) & 1;
                        let hi = (tile_hi >> i) & 1;
                        let color = (hi << 1) | lo;
                        let pixel = FifoPixelBuilder::new()
                            .with_color(color)
                            .with_palette(self.ppu.bgp)
                            .build();

                        fetcher.bg_fifo.push_back(pixel);
                    }

                    if self.ppu.scx % 8 > 0 && self.ppu.dot <= 92 {
                        // discard scx % 8 pixels of first tile pushed
                        fetcher.obj_state = Discard {
                            count: self.ppu.scx % 8,
                        };
                    } else {
                        fetcher.obj_state = Init;
                    }

                    fetcher.obj_pos += 1;
                }
            }
        }
    }

    fn push_pixel(&mut self, color_id: u8) {
        let color = DMG_PALETTE[color_id as usize];
        self.output.videobuf_back.0[self.ppu.pixel_idx + 0] = color.0;
        self.output.videobuf_back.0[self.ppu.pixel_idx + 1] = color.1;
        self.output.videobuf_back.0[self.ppu.pixel_idx + 2] = color.2;
        self.ppu.pixel_idx += 4;
    }

    fn render_pixel(&mut self) {
        if self.ppu.fetcher.bg_fifo.len() <= 8 {
            return;
        }

        if self.ppu.lcdc.bg_wnd_priority() {
            let pixel = self.ppu.fetcher.bg_fifo.pop_front().unwrap();
            let color_id = (self.ppu.bgp >> (2 * pixel.color())) & 0x3;
            self.push_pixel(color_id);
        } else {
            self.push_pixel(0);
        }

        self.ppu.shifter_pos += 1;
    }

    fn enter_scanline(&mut self) {
        let ppu = &mut self.ppu;

        ppu.stat.set_mode(Mode::OAMScan);
        ppu.fetcher.reset_obj();
        ppu.shifter_pos = 0;
        ppu.objs_count = 0;

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

    pub fn ppu_step(&mut self) {
        let ppu = &mut self.ppu;

        match ppu.stat.mode() {
            Mode::OAMScan => {
                if ppu.dot >= 80 {
                    ppu.stat.set_mode(Mode::Drawing);
                } else {
                    self.oam_scan_tick();
                }
            }

            Mode::Drawing => {
                if !ppu.fetcher.wnd
                    && ppu.lcdc.bg_wnd_priority()
                    && ppu.lcdc.wnd_enable()
                    && ppu.wy_eq_ly
                    && ppu.shifter_pos + 7 == ppu.wx
                {
                    // we've reached the window
                    ppu.fetcher.reset_obj();
                    // a 6-dot penalty is incurred while the BG fetcher is being set up for the window.
                    // ppu.fetcher.state = FifoState::Discard { count: 6 };
                    ppu.fetcher.wnd = true;
                }

                // if ppu.objs.iter().any(|s| s.x <= ppu.shifter_pos + 8) {
                //     ppu.fetcher.bg_state = FifoState::Init;
                //     ppu.shifter_paused = true;
                // }

                self.bg_fetcher_tick();
                // self.obj_fetcher_tick();
                self.render_pixel();

                if self.ppu.shifter_pos >= 160 {
                    self.enter_hblank();
                }
            }

            Mode::HBlank => {
                if ppu.dot >= 456 {
                    ppu.dot = 0;
                    ppu.ly += 1;

                    if ppu.fetcher.wnd {
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
                if ppu.dot >= 456 {
                    ppu.dot = 0;
                    ppu.ly += 1;

                    if ppu.ly >= 154 {
                        ppu.ly = 0;
                        ppu.wlc = 0;
                        ppu.pixel_idx = 0;
                        ppu.wy_eq_ly = false;

                        self.enter_scanline();
                    }
                }
            }
        }

        // TODO: warning: we are increasing after setting to 0
        self.ppu.dot += 1;
        self.update_stat_intr();
    }
}
