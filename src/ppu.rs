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
enum Mode {
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
    TileWait,
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
    state: FifoState,
    wnd: bool,
    pos: u8,
    fifo: VecDeque<FifoPixel>,
}
impl Fetcher {
    pub fn reset(&mut self) {
        self.state = FifoState::TileWait;
        self.pos = 0;
        self.wnd = false;
        self.fifo.clear();
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
    wly: u8,
    wy_eq_ly: bool,
    wnd_rendering: bool,

    pub bgp: u8,
    pub obp0: u8,
    pub obp1: u8,

    pub dot: u16,
    pixel_idx: usize,
    shifer_pos: u8,

    fetcher: Fetcher,
}

impl Ppu {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn tile_addr(&self, tile_id: u8) -> u16 {
        if self.lcdc.bg_wnd_tiles() {
            // unsigned mode
            0x8000 | (tile_id as u16 * 16)
        } else {
            // signed mode
            let offset = tile_id as i8 as i32;
            (0x9000 + offset) as u16
        }
    }

    pub fn is_wnd_visible(&self) -> bool {
        self.lcdc.wnd_enable() && self.wx <= 166 && self.wy <= 143
    }
}

impl GbEmulator {
    fn fetcher_tick(&mut self) {
        use FifoState::*;
        let fetcher = &mut self.ppu.fetcher;

        match fetcher.state {
            TileWait => fetcher.state = Tile,
            Tile => {
                let x = (self.ppu.scx / 8 + fetcher.pos) % 32;
                let (y, tilemap) = if fetcher.wnd {
                    let tilemap = if self.ppu.lcdc.wnd_map() {
                        0x9c00
                    } else {
                        0x9800
                    };

                    (32 * (self.ppu.wly as u16 / 8), tilemap)
                } else {
                    let tilemap = if self.ppu.lcdc.bg_map() {
                        0x9c00
                    } else {
                        0x9800
                    };

                    let y = (self.ppu.ly as u16 + self.ppu.scy as u16) % 256; // y with scroll
                    (32 * (y / 8), tilemap)
                };

                let offset = (y + x as u16) % 1024;

                let tile_id = self.dispatch_read(tilemap | offset);
                self.ppu.fetcher.state = DataLoWait { tile_id }
            }

            DataLoWait { tile_id } => fetcher.state = DataLo { tile_id },
            DataLo { tile_id } => {
                let offset = if fetcher.wnd {
                    2 * (self.ppu.wly as u16 % 8)
                } else {
                    2 * ((self.ppu.ly as u16 + self.ppu.scy as u16) % 8)
                };
                let tile_start = self.ppu.tile_addr(tile_id);
                let tile_addr = tile_start | offset;

                let tile_lo = self.dispatch_read(tile_addr);
                self.ppu.fetcher.state = DataHiWait { tile_addr, tile_lo };
            }

            DataHiWait { tile_addr, tile_lo } => fetcher.state = DataHi { tile_addr, tile_lo },
            DataHi { tile_addr, tile_lo } => {
                let tile_hi = self.dispatch_read(tile_addr + 1);

                if self.ppu.dot <= 86 {
                    // discard first fetched tile
                    self.ppu.fetcher.state = TileWait;
                } else {
                    self.ppu.fetcher.state = Push { tile_lo, tile_hi };
                }
            }

            Discard { count } => {
                if count > 0 {
                    fetcher.fifo.pop_front();
                    fetcher.state = Discard { count: count - 1 };
                } else {
                    fetcher.state = TileWait;
                }
            }

            Push { tile_lo, tile_hi } => {
                if fetcher.fifo.len() <= 8 {
                    for i in 0..8 {
                        let lo = (tile_lo >> i) & 1;
                        let hi = (tile_hi >> i) & 1;
                        let color = (hi << 1) | lo;
                        let pixel = FifoPixelBuilder::new()
                            .with_color(color)
                            .with_palette(self.ppu.bgp)
                            .build();

                        fetcher.fifo.push_back(pixel);
                    }

                    if self.ppu.dot <= 92 {
                        // discard scx % 8 pixels of first tile pushed
                        fetcher.state = Discard {
                            count: self.ppu.scx % 8,
                        };
                    } else {
                        fetcher.state = TileWait;
                    }

                    fetcher.pos += 1;
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
        if self.ppu.fetcher.fifo.len() <= 8 {
            return;
        }

        let pixel = self.ppu.fetcher.fifo.pop_front().unwrap();
        self.push_pixel(pixel.color());
        self.ppu.shifer_pos += 1;
    }

    pub fn ppu_step(&mut self) {
        let ppu = &mut self.ppu;

        match ppu.stat.mode() {
            Mode::OAMScan => {
                if ppu.dot >= 80 {
                    ppu.stat.set_mode(Mode::Drawing);
                }
            }

            Mode::Drawing => {
                if self.ppu.lcdc.wnd_enable()
                    && self.ppu.wy_eq_ly
                    && self.ppu.shifer_pos + 7 >= self.ppu.wx
                {
                    // we've reached the window
                    self.ppu.fetcher.reset();
                    self.ppu.fetcher.wnd = true;
                }

                self.fetcher_tick();
                self.render_pixel();
                if self.ppu.shifer_pos >= 160 {
                    self.ppu.fetcher.reset();
                    self.ppu.shifer_pos = 0;
                    self.ppu.stat.set_mode(Mode::HBlank);
                }
            }

            Mode::HBlank => {
                if ppu.dot >= 456 {
                    ppu.dot = 0;
                    ppu.ly += 1;
                    if ppu.ly >= 144 {
                        ppu.stat.set_mode(Mode::VBlank);
                        ppu.pixel_idx = 0;
                        ppu.wnd_rendering = false;

                        self.bus.intf.set_vblank(true);
                        std::mem::swap(
                            &mut self.output.videobuf_view,
                            &mut self.output.videobuf_back,
                        );

                        self.output.frame_ready = true;
                    } else {
                        ppu.stat.set_mode(Mode::OAMScan);
                    }
                }
            }

            Mode::VBlank => {
                if ppu.dot >= 456 {
                    ppu.dot = 0;
                    ppu.ly += 1;
                    if ppu.ly >= 154 {
                        ppu.ly = 0;
                        ppu.stat.set_mode(Mode::OAMScan);
                    }
                }
            }
        }

        self.ppu.dot += 1;
    }
}
