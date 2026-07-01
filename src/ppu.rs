use bitfields::{bitfield, bitflag};

use crate::emu::GbEmulator;

pub const DMG_PALETTE: [(u8, u8, u8); 4] =
    [(155, 188, 15), (139, 172, 15), (48, 98, 48), (15, 56, 15)];

#[bitfield(u8)]
pub struct Ctrl {
    bg_wnd_enable: bool,
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

#[derive(Default)]
pub struct Ppu {
    pub lcdc: Ctrl,
    pub ly: u8,
    pub lyc: u8,
    pub stat: Stat,
    pub scy: u8,
    pub scx: u8,
    pub bgp: u8,
    pub obp0: u8,
    pub obp1: u8,
    pub wy: u8,
    pub wx: u8,

    pub dot: i16,
    pub line: i16,
}

impl Ppu {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }
}

impl GbEmulator {
    pub fn ppu_step(&mut self) {
        let ppu = &mut self.ppu;
        ppu.dot += 1;
        if ppu.dot >= 456 {
            ppu.dot = 0;
            ppu.line += 1;
            ppu.ly += 1;
            if ppu.line == 144 {
                self.bus.intf.set_vblank(true);
                self.output.frame_ready = true;
            }

            if ppu.line >= 154 {
                ppu.line = 0;
                ppu.ly = 0;
            }
        }
    }
}
