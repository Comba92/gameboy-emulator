use crate::{
    emu::GbEmulator,
    rom::{Cart, RomData},
    serial, timer,
};
use bitfields::bitfield;

pub const ROM0_START: u16 = 0x0000;
pub const ROM1_START: u16 = 0x4000;
pub const VRAM_START: u16 = 0x8000;
pub const SRAM_START: u16 = 0xa000;
pub const WRAM0_START: u16 = 0xc000;
pub const WRAM1_START: u16 = 0xd000;

pub const ROM_BANK_SIZE: u16 = 16 * 1024;
pub const VRAM_BANK_SIZE: u16 = 8 * 1024;
pub const SRAM_BANK_SIZE: u16 = 8 * 1024;
pub const WRAM_BANK_SIZE: u16 = 4 * 1024;

#[derive(Clone)]
enum Handler {
    Rom0,
    Rom1,
    Vram, // Video RAM
    Sram, // Cartridge RAM
    Wram, // Work RAM
    IO,
    OpenBus,
    Debug,
}

const DEFAULT_MAP: [Handler; 16] = [
    Handler::Rom0,
    Handler::Rom0,
    Handler::Rom0,
    Handler::Rom0,
    Handler::Rom1,
    Handler::Rom1,
    Handler::Rom1,
    Handler::Rom1,
    Handler::Vram,
    Handler::Vram,
    Handler::OpenBus,
    Handler::OpenBus,
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
    #[bits(3, default = 0x7)]
    _unused: u8,
}

pub(crate) struct Bus {
    rom: Box<[u8]>,
    bios: Option<Box<[u8]>>,
    pub header: RomData,

    pub intf: IntFlags,
    pub inte: IntFlags,

    pub(crate) hram: [u8; 128],
    pub(crate) oam: [u8; 160],
    pub(crate) vram: Box<[u8]>,
    pub(crate) sram: Box<[u8]>,
    pub(crate) wram: Box<[u8]>,

    rom0_banking: Banker,
    rom1_banking: Banker,
    sram_banking: Banker,

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
            oam: [0; _],
            vram: Default::default(),
            sram: Default::default(),
            wram: vec![0; 64 * 1024].into_boxed_slice(),

            rom0_banking: Banker::new(ROM_BANK_SIZE, 0),
            rom1_banking: Banker::new(ROM_BANK_SIZE, 0),
            sram_banking: Banker::new(SRAM_BANK_SIZE, 0),

            map: std::array::from_fn(|_| Handler::Debug),
        }
    }

    pub fn new(mut cart: Cart, bios: Option<Vec<u8>>) -> Self {
        let mut res = Self {
            rom: std::mem::take(&mut cart.rom).into_boxed_slice(),
            bios: None,

            intf: IntFlags::new(),
            inte: IntFlags::new(),

            hram: [0xff; _],
            oam: [0xff; _],
            vram: vec![0xff; 8 * 1024].into_boxed_slice(),
            sram: Default::default(),
            wram: vec![0xff; 8 * 1024].into_boxed_slice(),

            rom0_banking: Banker::new(ROM_BANK_SIZE, cart.header.rom_size),
            rom1_banking: Banker::new(ROM_BANK_SIZE, cart.header.rom_size),
            sram_banking: Banker::new(SRAM_BANK_SIZE, cart.header.ram_size),

            header: cart.header,
            map: DEFAULT_MAP,
        };

        res.rom0_banking.map(0);
        res.rom1_banking.map(1);

        if res.header.ram_size > 0 {
            res.sram = vec![0xff; res.header.ram_size as usize].into_boxed_slice();
        }

        // TODO: hardcoded for now
        if [0x1, 0x2, 0x3].contains(&res.header.mapper) {
            res.rom0_banking = Banker::new(ROM_BANK_SIZE, res.header.rom_size);
            res.rom1_banking = Banker::new(ROM_BANK_SIZE, res.header.rom_size);
            res.sram_banking = Banker::new(SRAM_BANK_SIZE, res.header.ram_size);

            res.rom0_banking.map(0); // default to bank 0
            res.rom1_banking.map(1); // default to bank 1
            res.sram_banking.map(0);
        }

        if let Some(bios) = bios {
            // at boot bios is mapped to the first 0x100 bytes
            // swap bios out with the first 0x100, set them back later
            let tmp = res.rom[..0x0100].to_vec();
            res.rom[..0x0100].copy_from_slice(&bios);
            res.bios = Some(tmp.into_boxed_slice());
        }

        res
    }
}

impl GbEmulator {
    pub fn debug_read(&self, addr: u16) -> u8 {
        let bus = &self.bus;

        let handler = addr >> 12;
        match bus.map[handler as usize] {
            Handler::Rom0 => bus.rom[bus.rom0_banking.translate(addr)],
            Handler::Rom1 => bus.rom[bus.rom1_banking.translate(addr)],
            Handler::OpenBus => 0xff,
            Handler::Vram => bus.vram[addr as usize - 0x8000],
            Handler::Sram => bus.sram[bus.sram_banking.translate(addr)],
            Handler::Wram => bus.wram[addr as usize - 0xa000],
            Handler::IO => 0xff,
            Handler::Debug => bus.wram[addr as usize],
        }
    }

    pub fn dispatch_read(&mut self, addr: u16) -> u8 {
        let bus = &mut self.bus;

        let handler = addr >> 12;
        let res = match bus.map[handler as usize] {
            Handler::Rom0 => bus.rom[bus.rom0_banking.translate(addr)],
            Handler::Rom1 => bus.rom[bus.rom1_banking.translate(addr)],
            Handler::OpenBus => 0xff,

            Handler::Vram => {
                // if self.ppu.stat.mode() != ppu::Mode::Drawing {
                bus.vram[addr as usize - 0x8000]
                // } else {
                // 0xff
                // }
            }
            Handler::Sram => bus.sram[bus.sram_banking.translate(addr)],
            Handler::Wram => bus.wram[addr as usize - 0xc000],
            Handler::IO => self.io_read(addr),
            Handler::Debug => bus.wram[addr as usize],
        };

        res
    }

    pub fn dispatch_write(&mut self, addr: u16, val: u8) {
        let bus = &mut self.bus;

        let handler = addr >> 12;
        match bus.map[handler as usize] {
            Handler::Rom0 | Handler::Rom1 => self.prg_write(addr, val),
            Handler::OpenBus => {}

            Handler::Vram => {
                // if self.ppu.stat.mode() != ppu::Mode::Drawing {
                bus.vram[addr as usize - 0x8000] = val
                // }
            }
            Handler::Sram => bus.sram[bus.sram_banking.translate(addr)] = val,
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
            return self.bus.oam[addr as usize - 0xfe00];
        } else if 0xff80 <= addr && addr <= 0xfffe {
            // HRAM
            return self.bus.hram[addr as usize - 0xff80];
        }

        match addr {
            0xff00 => self.joy.read(),
            0xff01 => self.serial.data,
            0xff02 => self.serial.ctrl.into_bits(),
            0xff04 => self.timer.div,
            0xff05 => self.timer.tima,
            0xff06 => self.timer.tma,
            0xff07 => self.timer.tac.into_bits(),

            0xff0f => self.bus.intf.into_bits(),
            0xff40 => self.ppu.lcdc.into_bits(),
            0xff41 => self.ppu.stat.into_bits(),
            0xff42 => self.ppu.scy,
            0xff43 => self.ppu.scx,
            0xff44 => self.ppu.ly,
            // 0xff44 => 144,
            0xff45 => self.ppu.lyc,
            0xff46 => self.dma.read(),
            0xff47 => self.ppu.bgp,
            0xff48 => self.ppu.obp0,
            0xff49 => self.ppu.obp1,
            0xff4a => self.ppu.wy,
            0xff4b => self.ppu.wx,

            0xffff => self.bus.inte.into_bits(),
            _ => 0xff,
        }
    }

    fn io_write(&mut self, addr: u16, val: u8) {
        if addr >= 0xfe00 && addr <= 0xfe9f {
            // OAM
            self.bus.oam[addr as usize - 0xfe00] = val;
        } else if 0xff80 <= addr && addr <= 0xfffe {
            // HRAM
            self.bus.hram[addr as usize - 0xff80] = val;
        }

        match addr {
            0xff00 => self.joy.write(val),
            0xff01 => self.serial.data = val,
            0xff02 => {
                self.serial.ctrl = serial::Ctrl::from(val);
                // if self.serial.ctrl.transfer_enable() {
                //     self.serial.out_buffer.push(self.serial.data);
                //     let str = String::from_utf8_lossy(&self.serial.out_buffer);
                //     println!("{str}");
                // }
            }
            0xff04 => self.timer.div = 0,
            0xff05 => self.timer.tima = val,
            0xff06 => self.timer.tma = val,
            0xff07 => self.timer.tac = timer::Ctrl::from_bits(val),

            0xff0f => {
                self.bus.intf = IntFlags::from(val);
            }

            0xff40 => {
                self.lcdc_write(val);
            }

            0xff41 => {
                // only bits from 6 to 3 are writable
                self.ppu.stat.set_lyc_int(val & 0x40 != 0);
                self.ppu.stat.set_mode2_int(val & 0x20 != 0);
                self.ppu.stat.set_mode1_int(val & 0x10 != 0);
                self.ppu.stat.set_mode0_int(val & 0x8 != 0);
            }

            0xff42 => self.ppu.scy = val,
            0xff43 => self.ppu.scx = val,
            0xff45 => self.ppu.lyc = val,
            0xff46 => self.dma.write(val),
            0xff47 => self.ppu.bgp = val,
            0xff48 => self.ppu.obp0 = val,
            0xff49 => self.ppu.obp1 = val,
            0xff4a => self.ppu.wy = val,
            0xff4b => self.ppu.wx = val,
            0xff50 => {
                if let Some(boot) = self.bus.bios.take() {
                    self.bus.rom[..0x100].copy_from_slice(&boot);
                }
            }

            0xffff => {
                self.bus.inte = IntFlags::from(val);
            }

            _ => {}
        }
    }

    pub fn prg_write(&mut self, addr: u16, val: u8) {
        match &mut self.mbc {
            Mbc::None => {}
            Mbc::MBC1(m) => {
                if addr <= 0x1fff {
                    m.ram_enable = val & 0xf == 0xa;
                    if m.ram_enable {
                        self.bus.map[0xa] = Handler::Sram;
                        self.bus.map[0xb] = Handler::Sram;
                    } else {
                        self.bus.map[0xa] = Handler::OpenBus;
                        self.bus.map[0xb] = Handler::OpenBus;
                    }
                } else if addr <= 0x3fff {
                    m.rom_bank = (val & 0x1f).max(1);
                } else if addr <= 0x5fff {
                    m.ram_bank = val & 0x3;
                } else {
                    m.mode = val & 1 != 0;
                }

                m.update(&mut self.bus);
            }
        }
    }
}

#[derive(Debug)]
struct Banker {
    banks_count_mask: u16,
    bank_size_mask: u16,
    bank_size_shift: u8,
    real_addr_start: u32,
}

impl Banker {
    pub fn new(virt_size: u16, real_size: usize) -> Self {
        // https://stackoverflow.com/questions/25787613/division-and-multiplication-by-power-of-2
        let bank_size_shift = virt_size.ilog2() as u8;
        // TODO: this doesn't work if bankSize is odd!
        let bank_size_mask = virt_size.saturating_sub(1);

        // TODO: if realSize < virtSize and there banks arent big enough, this becomes 0!!
        // TODO: handle unconvetional realSizes (less tha 8KiB)
        let banks_count = (real_size / virt_size as usize) as u16;
        // TODO: this doesn't work if banksCount is odd! (shouldnt happen as it
        // depends on realSize, and it should always be power of two)
        let banks_count_mask = banks_count.saturating_sub(1);

        let res = Self {
            banks_count_mask,
            bank_size_mask,
            bank_size_shift,
            real_addr_start: 0,
        };

        res
    }

    pub fn map(&mut self, bank: u8) {
        // some games might write bigger bank numbers than really avaible
        // let bank = bank % self.banks_count;
        // let bank = bank & (self.banks_count-1);
        let bank = bank as u16 & self.banks_count_mask;

        // we precompute the real index instead of keeping the bank number
        // self.real_addr_start = bank * self.bank_size;
        self.real_addr_start = (bank as u32) << self.bank_size_shift;
    }

    pub fn translate(&self, addr: u16) -> usize {
        // i do not expect to write outside the slots array here either.
        // self.real_addr_start + (addr % self.bank_size)
        // real index + offset
        self.real_addr_start as usize | (addr & self.bank_size_mask) as usize
    }
}

pub enum Mbc {
    None,
    MBC1(Mbc1),
}

impl Mbc {
    pub fn new(header: &RomData) -> Result<Self, &'static str> {
        let res = match header.mapper {
            0x00 | 0x08 | 0x09 => Mbc::None,
            0x01 | 0x02 | 0x03 => Mbc::MBC1(Mbc1::new(header)),
            _ => return Err("mapper not implemented"),
        };

        Ok(res)
    }
}

#[derive(Default)]
pub(crate) struct Mbc1 {
    is_big_cart: bool,
    ram_enable: bool,
    mode: bool,
    rom_bank: u8,
    ram_bank: u8,
}
impl Mbc1 {
    pub fn new(header: &RomData) -> Self {
        Self {
            is_big_cart: header.rom_size > 512 * 1024 && header.ram_size > 8 * 1024,
            ..Default::default()
        }
    }

    pub fn update(&self, bus: &mut Bus) {
        // only big carts care about mode
        if self.is_big_cart {
            if self.mode {
                bus.rom0_banking.map(self.ram_bank << 5);
                bus.sram_banking.map(self.ram_bank);
            } else {
                bus.rom0_banking.map(0);
                bus.sram_banking.map(0);
            }

            bus.rom1_banking.map((self.ram_bank << 5) | self.rom_bank);
        } else {
            bus.rom0_banking.map(0);
            bus.rom1_banking.map(self.rom_bank);
            bus.sram_banking.map(self.ram_bank);
        }
    }
}
