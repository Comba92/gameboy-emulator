use crate::{
    emu::GbEmulator,
    ppu,
    rom::{Cart, RomData},
    serial,
};
use bitfields::bitfield;

enum Handler {
    Rom,
    Vram, // Video RAM
    Sram, // Cartridge RAM
    Wram, // Work RAM
    IO,
    Debug,
}

enum IOHandler {
    EchoRam,
    OAM,
    Unused,
    HRam,
    IE,
}

const DEFAULT_MAP: [Handler; 16] = [
    Handler::Rom,
    Handler::Rom,
    Handler::Rom,
    Handler::Rom,
    Handler::Rom,
    Handler::Rom,
    Handler::Rom,
    Handler::Rom,
    Handler::Vram,
    Handler::Vram,
    Handler::Sram,
    Handler::Sram,
    Handler::Wram,
    Handler::Wram,
    Handler::IO,
    Handler::IO,
];

#[bitfield(u8)]
pub(crate) struct IntFlags {
    vblank: bool,
    lcd: bool,
    timer: bool,
    serial: bool,
    joypad: bool,
    #[bits(3)]
    _unused: u8,
}

pub(crate) struct Bus {
    rom: Box<[u8]>,
    bios: Option<Box<[u8]>>,
    pub header: RomData,

    pub intf: IntFlags,
    pub inte: IntFlags,

    pub(crate) hram: [u8; 128],
    pub(crate) vram: Box<[u8]>,
    pub(crate) sram: Box<[u8]>,
    pub(crate) wram: Box<[u8]>,

    map: [Handler; 16],
}

impl Bus {
    pub fn with_ram_64kb() -> Self {
        Self {
            rom: Default::default(),
            bios: Default::default(),
            header: Default::default(),

            intf: IntFlags::new(),
            inte: IntFlags::new(),

            hram: [0; _],
            vram: Default::default(),
            sram: Default::default(),
            wram: vec![0; 64 * 1024].into_boxed_slice(),

            map: std::array::from_fn(|_| Handler::Debug),
        }
    }

    pub fn new(mut cart: Cart, bios: Vec<u8>) -> Self {
        let mut res = Self {
            rom: std::mem::take(&mut cart.rom).into_boxed_slice(),
            bios: None,
            header: cart.header,

            intf: IntFlags::new(),
            inte: IntFlags::new(),

            hram: [0; _],
            vram: vec![0; 8 * 1024].into_boxed_slice(),
            sram: Default::default(),
            wram: vec![0; 8 * 1024].into_boxed_slice(),

            map: DEFAULT_MAP,
        };

        println!("{:?}", res.header);

        // at boot bios is mapped to the first 0x100 bytes
        // swap bios out with the first 0x100, set them back later
        // let tmp = res.rom[..0x0100].to_vec();
        // res.rom[..0x0100].copy_from_slice(&bios);
        // res.bios = Some(tmp.into_boxed_slice());

        res
    }
}

impl GbEmulator {
    pub fn debug_read(&self, addr: u16) -> u8 {
        let bus = &self.bus;

        let handler = addr >> 12;
        match bus.map[handler as usize] {
            Handler::Rom => bus.rom[addr as usize],
            Handler::Vram => bus.vram[addr as usize - 0x8000],
            Handler::Sram => 0,
            Handler::Wram => bus.wram[addr as usize - 0xa000],
            Handler::IO => 0xff,
            Handler::Debug => bus.wram[addr as usize],
        }
    }

    pub fn dispatch_read(&mut self, addr: u16) -> u8 {
        let bus = &mut self.bus;

        let handler = addr >> 12;
        match bus.map[handler as usize] {
            Handler::Rom => bus.rom[addr as usize],
            Handler::Vram => bus.vram[addr as usize - 0x8000],
            Handler::Sram => 0,
            Handler::Wram => bus.wram[addr as usize - 0xc000],
            Handler::IO => self.io_read(addr),
            Handler::Debug => bus.wram[addr as usize],
        }
    }

    pub fn dispatch_write(&mut self, addr: u16, val: u8) {
        let bus = &mut self.bus;

        let handler = addr >> 12;
        match bus.map[handler as usize] {
            Handler::Rom => {}
            Handler::Vram => bus.vram[addr as usize - 0x8000] = val,
            Handler::Sram => {}
            Handler::Wram => bus.wram[addr as usize - 0xc000] = val,
            Handler::IO => self.io_write(addr, val),
            Handler::Debug => bus.wram[addr as usize] = val,
        }
    }

    fn io_read(&mut self, addr: u16) -> u8 {
        if addr <= 0xfdff {
            // echo ram
            return 0xff;
        } else if addr <= 0xfe9f {
            // OAM
            return 0xff; // TODO
        } else if addr >= 0xff80 && addr <= 0xffee {
            return self.bus.hram[addr as usize - 0xff80];
        }

        match addr {
            0xff00 => self.joy.read(),
            0xff01 => self.serial.data,
            0xff02 => self.serial.ctrl.into_bits(),

            0xff0f => self.bus.intf.into_bits(),
            0xff40 => self.ppu.lcdc.into_bits(),
            0xff41 => self.ppu.stat.into_bits(),
            0xff42 => self.ppu.scy,
            0xff43 => self.ppu.scx,
            0xff44 => self.ppu.ly,

            0xff47 => self.ppu.bgp,
            0xff4a => self.ppu.wy,
            0xff4b => self.ppu.wx,

            0xffff => self.bus.inte.into_bits(),
            _ => 0xff,
        }
    }

    fn io_write(&mut self, addr: u16, val: u8) {
        match addr {
            0xff00 => self.joy.write(val),
            0xff01 => self.serial.data = val,
            0xff02 => {
                self.serial.ctrl = serial::Ctrl::from(val);
                if self.serial.ctrl.transfer_enable() {
                    self.serial.out_buffer.push(self.serial.data);
                    let str = String::from_utf8_lossy(&self.serial.out_buffer);
                    println!("{str}");
                }
            }
            0xff0f => self.bus.intf = IntFlags::from(val),

            0xff40 => self.ppu.lcdc = ppu::Ctrl::from(val),

            0xff41 => {
                self.ppu.stat.set_bits_range(3, 4, val); // only bits from 6 to 3 are writable
            }

            0xff42 => self.ppu.scy = val,
            0xff43 => self.ppu.scx = val,
            0xff47 => self.ppu.bgp = val,
            0xff4a => self.ppu.wy = val,
            0xff4b => self.ppu.wx = val,

            0xff50 => {
                if let Some(boot) = self.bus.bios.take() {
                    self.bus.rom[..0x100].copy_from_slice(&boot);
                }
            }

            0xffff => self.bus.inte = IntFlags::from(val),

            _ => {}
        }
    }
}
