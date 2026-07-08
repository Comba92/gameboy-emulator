use crate::{
    clock,
    emu::GbEmulator,
    ppu,
    rom::{self, Cart, RomData},
    serial, timer,
};
use bitfields::bitfield;

pub const ROM0_START: u16 = 0x0000;
pub const ROM1_START: u16 = 0x4000;
pub const VRAM_START: u16 = 0x8000;
pub const SRAM_START: u16 = 0xa000;
pub const WRAM0_START: u16 = 0xc000;
pub const WRAM1_START: u16 = 0xd000;

pub const ROM_BANK_SIZE: usize = 16 * 1024;
pub const VRAM_BANK_SIZE: usize = 8 * 1024;
pub const SRAM_BANK_SIZE: usize = 8 * 1024;
pub const WRAM_BANK_SIZE: usize = 4 * 1024;

#[derive(Clone, Copy)]
pub(crate) enum Handler {
    Rom0,
    Rom1,
    Vram, // Video RAM
    Sram, // Cartridge RAM
    SramReadOnly,
    Wram0,
    Wram1,
    IO,
    OpenBus,
    Mbc2Ram,
    // Mbc6Flash,
    // Mbc6Ram,
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
    Handler::Wram0,
    Handler::Wram1,
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
    boot_sector0: Option<Box<[u8]>>,
    boot_sector1: Option<Box<[u8]>>,
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
    wram_banking: Banker,
    wram_bank_select: u8,
    vram_banking: Banker,
    vram_bank_select: u8,

    map: [Handler; 16],
}

impl Bus {
    pub fn with_ram_64kb() -> Self {
        Self {
            rom: Default::default(),
            boot_sector0: Default::default(),
            boot_sector1: Default::default(),
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
            wram_banking: Banker::new(WRAM_BANK_SIZE, 0),
            vram_banking: Banker::new(VRAM_BANK_SIZE, 0),
            wram_bank_select: 1,
            vram_bank_select: 0,

            map: std::array::from_fn(|_| Handler::Debug),
        }
    }

    pub fn new(mut cart: Cart, bios: Option<Vec<u8>>) -> Self {
        let vram_size = if cart.header.is_cgb() {
            VRAM_BANK_SIZE * 2
        } else {
            VRAM_BANK_SIZE
        };

        let wram_size = if cart.header.is_cgb() {
            WRAM_BANK_SIZE * 8
        } else {
            WRAM_BANK_SIZE * 2
        };

        let mut res = Self {
            rom: std::mem::take(&mut cart.rom).into_boxed_slice(),
            boot_sector0: None,
            boot_sector1: None,

            intf: IntFlags::new(),
            inte: IntFlags::new(),

            hram: [0xff; _],
            oam: [0xff; _],
            vram: vec![0xff; vram_size].into_boxed_slice(),
            sram: vec![0xff; cart.header.ram_size as usize].into_boxed_slice(),
            wram: vec![0xff; wram_size].into_boxed_slice(),

            rom0_banking: Banker::new(ROM_BANK_SIZE, cart.header.rom_size),
            rom1_banking: Banker::new(ROM_BANK_SIZE, cart.header.rom_size),
            sram_banking: Banker::new(SRAM_BANK_SIZE, cart.header.ram_size),
            wram_banking: Banker::new(WRAM_BANK_SIZE, wram_size),
            vram_banking: Banker::new(VRAM_BANK_SIZE, vram_size),
            wram_bank_select: 1,
            vram_bank_select: 0,

            header: cart.header,
            map: DEFAULT_MAP,
        };

        res.rom0_banking.map(0);
        res.rom1_banking.map(1);
        res.vram_banking.map(0);
        res.sram_banking.map(0);
        res.wram_banking.map(1);

        if let Some(bios) = bios {
            // at boot bios is mapped to the first 0x100 bytes
            // swap bios out with the first 0x100, set them back later
            let tmp = res.rom[..0x0100].to_vec();
            res.rom[..0x0100].copy_from_slice(&bios[..0x100]);
            res.boot_sector0 = Some(tmp.into_boxed_slice());

            // cgb boot is bigger than 256 bytes, has a second sector
            if bios.len() > 0x100 {
                let tmp = res.rom[0x200..0x900].to_vec();
                res.rom[0x200..0x900].copy_from_slice(&bios[0x200..]);
                res.boot_sector1 = Some(tmp.into_boxed_slice());
            }
        }

        res
    }

    pub fn sram_enable(&mut self, cond: bool) {
        if cond {
            self.sram_set(Handler::Sram);
        } else {
            self.sram_set(Handler::OpenBus);
        }
    }

    pub fn sram_set(&mut self, handler: Handler) {
        if self.header.ram_size > 0 {
            self.map[0xa] = handler;
            self.map[0xb] = handler;
        }
    }

    pub fn vram_direct_read(&self, addr: u16) -> u8 {
        self.vram[self.vram_banking.translate(addr)]
    }

    pub fn vram0_read(&self, addr: u16) -> u8 {
        self.vram[addr as usize - 0x8000]
    }

    pub fn vram1_read(&self, addr: u16) -> u8 {
        self.vram[(8 * 1024) | (addr as usize - 0x8000)]
    }

    pub fn oam_direct_read(&self, addr: u16) -> u8 {
        self.oam[addr as usize - 0xfe00]
    }

    pub fn oam_direct_write(&mut self, addr: u16, val: u8) {
        self.oam[addr as usize - 0xfe00] = val;
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
            Handler::Vram => bus.vram_direct_read(addr),
            Handler::Sram | Handler::SramReadOnly => bus.sram[bus.sram_banking.translate(addr)],
            Handler::Wram0 => bus.wram[addr as usize - WRAM0_START as usize],
            Handler::Wram1 => bus.wram[bus.wram_banking.translate(addr)],
            Handler::Mbc2Ram => bus.sram[addr as usize % 512],
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
                if self.ppu.stat.mode() != ppu::Mode::Drawing {
                    bus.vram_direct_read(addr)
                } else {
                    0xff
                }
            }
            Handler::Sram | Handler::SramReadOnly => bus.sram[bus.sram_banking.translate(addr)],
            Handler::Mbc2Ram => bus.sram[addr as usize % 512],
            Handler::Wram0 => bus.wram[addr as usize - WRAM0_START as usize],
            Handler::Wram1 => bus.wram[bus.wram_banking.translate(addr)],
            Handler::IO => self.io_read(addr),
            Handler::Debug => bus.wram[addr as usize],
        };

        res
    }

    pub fn dispatch_write(&mut self, addr: u16, val: u8) {
        let bus = &mut self.bus;

        let handler = addr >> 12;
        match bus.map[handler as usize] {
            Handler::Rom0 | Handler::Rom1 => self.mbc(addr, val),
            Handler::OpenBus | Handler::SramReadOnly => {}

            Handler::Vram => {
                if self.ppu.stat.mode() != ppu::Mode::Drawing {
                    bus.vram[bus.vram_banking.translate(addr)] = val
                }
            }
            Handler::Sram => bus.sram[bus.sram_banking.translate(addr)] = val,
            Handler::Wram0 => bus.wram[addr as usize - WRAM0_START as usize] = val,
            Handler::Wram1 => bus.wram[bus.wram_banking.translate(addr)] = val,
            Handler::IO => self.io_write(addr, val),

            Handler::Mbc2Ram => bus.sram[addr as usize % 512] = (val & 0x0f) | 0xf0,
            Handler::Debug => bus.wram[addr as usize] = val,
        }
    }

    fn io_read(&mut self, addr: u16) -> u8 {
        if 0xfe00 <= addr && addr <= 0xfe9f {
            // OAM
            return if self.ppu.stat.mode().into_bits() < 2 {
                self.bus.oam_direct_read(addr)
            } else {
                0xff
            };
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
            0xff44 => self.ppu.ly_read,
            // 0xff44 => 144,
            0xff45 => self.ppu.lyc,
            0xff46 => self.dma.read(),
            0xff47 => self.ppu.bgp,
            0xff48 => self.ppu.obp0,
            0xff49 => self.ppu.obp1,
            0xff4a => self.ppu.wy,
            0xff4b => self.ppu.wx,

            0xff4c => {
                if self.is_cgb() {
                    self.clock.sys.into_bits()
                } else {
                    0xff
                }
            }
            0xff4d => {
                if self.is_cgb() {
                    self.clock.speed.into_bits()
                } else {
                    0xff
                }
            }
            0xff4f => {
                if self.is_cgb() {
                    self.bus.vram_bank_select | !0x1
                } else {
                    0xff
                }
            }

            0xff70 => {
                if self.is_cgb() {
                    self.bus.wram_bank_select
                } else {
                    0xff
                }
            }

            0xffff => self.bus.inte.into_bits(),
            _ => 0, // careful: most games don't expect a 0xff from uninmplemented IO ports. 0 seems to fix it
        }
    }

    fn io_write(&mut self, addr: u16, val: u8) {
        if 0xfe00 <= addr && addr <= 0xfe9f {
            // OAM
            if self.ppu.stat.mode().into_bits() < 2 {
                self.bus.oam[addr as usize - 0xfe00] = val;
            }
            return;
        } else if 0xff80 <= addr && addr <= 0xfffe {
            // HRAM
            self.bus.hram[addr as usize - 0xff80] = val;
            return;
        }

        match addr {
            0xff00 => self.joy.write(val),
            0xff01 => self.serial.data = val,
            0xff02 => {
                self.serial.ctrl = serial::Ctrl::from_bits_with_defaults(val);
                // if self.serial.ctrl.transfer_enable() {
                //     self.serial.out_buffer.push(self.serial.data);
                //     let str = String::from_utf8_lossy(&self.serial.out_buffer);
                //     println!("{str}");
                // }
            }
            0xff04 => self.timer.div = 0,
            0xff05 => self.timer.tima = val,
            0xff06 => self.timer.tma = val,
            0xff07 => self.timer.tac = timer::Ctrl::from_bits_with_defaults(val),

            0xff0f => {
                self.bus.intf = IntFlags::from_bits_with_defaults(val);
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

            0xff4c => {
                if self.is_cgb() && self.bus.boot_sector0.is_some() {
                    self.clock.sys = clock::Sys::from_bits_with_defaults(val)
                }
            }
            0xff4d => {
                if self.is_cgb() {
                    self.clock.speed.set_armed(val & 1 != 0);
                }
            }
            0xff4f => {
                if self.is_cgb() {
                    self.bus.vram_banking.map((val & 1) as u16);
                    self.bus.vram_bank_select = val & 1;
                }
            }

            0xff50 => {
                // restore boot sectors to original rom contents
                if let Some(boot0) = self.bus.boot_sector0.take() {
                    self.bus.rom[..0x100].copy_from_slice(&boot0);

                    if let Some(boot1) = self.bus.boot_sector1.take() {
                        self.bus.rom[0x200..0x900].copy_from_slice(&boot1);
                    }
                }
            }

            0xff70 => {
                if self.is_cgb() {
                    self.bus.wram_banking.map((val & 0x3).max(1) as u16);
                    self.bus.wram_bank_select = val;
                }
            }

            0xffff => {
                self.bus.inte = IntFlags::from_bits_with_defaults(val);
            }

            _ => {}
        }
    }

    pub fn mbc(&mut self, addr: u16, val: u8) {
        match &mut self.mbc {
            Mbc::None => {}
            Mbc::Mbc1(m) => {
                match addr {
                    0x0000..0x2000 => self.bus.sram_enable(val & 0x0f == 0x0a),
                    0x2000..0x4000 => m.rom_bank = (val & 0x1f).max(1),
                    0x4000..0x6000 => m.ram_bank = val & 0x3,
                    0x6000..0x8000 => m.mode = val & 1 != 0,
                    _ => {}
                }

                m.update(&mut self.bus);
            }

            Mbc::Mbc2 => {
                if addr >= 0x4000 {
                    return;
                }

                if addr & 0x100 == 0 {
                    if val & 0x0f == 0x0a {
                        self.bus.sram_set(Handler::Mbc2Ram);
                    } else {
                        self.bus.sram_set(Handler::OpenBus);
                    }
                } else {
                    let bank = (val & 0x0f).max(1);
                    self.bus.rom1_banking.map(bank as u16);
                }
            }

            Mbc::Mbc3(m) => {
                match addr {
                    0x0000..0x2000 => {
                        self.bus.sram_enable(val & 0xf == 0xa);
                        m.timer_enable = val & 0xf == 0xa;
                    }
                    0x2000..0x4000 => self.bus.rom1_banking.map(val.max(1) as u16),
                    0x4000..0x6000 => {
                        self.bus.sram_banking.map(val as u16);
                        m.timer_reg = val;
                    }
                    0x6000..0x8000 => {} // TODO: Latch Clock
                    0xa000..0xc000 => {} // TODO: RTC
                    _ => {}
                }
            }

            Mbc::Mbc5(m) => match addr {
                0x0000..0x2000 => self.bus.sram_enable(val & 0xf == 0xa),
                0x2000..0x3000 => {
                    m.rom_bank = (m.rom_bank & 0xff00) | (val as u16);
                    self.bus.rom1_banking.map(m.rom_bank);
                }
                0x3000..0x4000 => {
                    m.rom_bank = (m.rom_bank & 0x00ff) | ((val as u16 & 1) << 8);
                    self.bus.rom1_banking.map(m.rom_bank);
                }
                0x4000..0x6000 => {
                    self.bus.sram_banking.map(val as u16);
                    m.rumble_enable = val & 0x08 != 0;
                }
                _ => {}
            },

            Mbc::Mbc6 => {
                match addr {
                    0x0000..0x0400 => self.bus.sram_enable(val & 0xf == 0xa),
                    0x0400..0x800 => {
                        // ram A
                    }
                    0x0800..0x0c00 => {
                        // ram B
                    }
                    0x0c00..0x1000 => {
                        // flash enable
                    }
                    0x1000 => {
                        // flash write enable
                    }

                    0x2000..0x2800 => {
                        // flash A number
                    }
                    0x2800..0x3000 => {
                        // flash A select
                    }

                    0x3000..0x3800 => {
                        // flash B number
                    }
                    0x3800..0x4000 => {
                        // flash B select
                    }

                    _ => {}
                }
            }

            Mbc::Mbc7(m) => {
                match addr {
                    0x0000..0x2000 => {
                        m.ram_enable0 = val & 0x0f == 0x0a;
                        self.bus.sram_enable(m.ram_enable0 && m.ram_enable1);
                    }
                    0x2000..0x4000 => self.bus.rom1_banking.map(val as u16),
                    0x4000..0x6000 => {
                        m.ram_enable1 = val == 40;
                        self.bus.sram_enable(m.ram_enable0 && m.ram_enable1);
                    }

                    0xa000..0xb000 => {
                        // TODO: accel and eeprom
                    }
                    _ => {}
                }
            }

            Mbc::Mmm01 => todo!(),

            Mbc::M161(bankswitched) => {
                if addr >= 0x8000 || *bankswitched {
                    return;
                }
                self.bus.rom1_banking.map(val as u16);
                *bankswitched = true;
            }

            Mbc::HuC1 => {
                match addr {
                    0x0000..0x2000 => {
                        if val == 0xe {
                            // TODO: IR enable
                            self.bus.sram_enable(false);
                        } else {
                            self.bus.sram_enable(true);
                        }
                    }
                    0x2000..0x4000 => self.bus.rom1_banking.map(val as u16),
                    0x4000..0x6000 => self.bus.sram_banking.map(val as u16),
                    0xa000..0xc000 => {
                        // TODO: IR mode
                    }
                    _ => {}
                }
            }

            Mbc::HuC3 => {
                match addr {
                    0x0000..0x2000 => {
                        match val {
                            0x0 => self.bus.sram_set(Handler::SramReadOnly),
                            0xa => self.bus.sram_set(Handler::Sram),
                            // TODO: RTC and IR
                            _ => self.bus.sram_set(Handler::OpenBus),
                        }
                    }

                    0x2000..0x4000 => self.bus.rom1_banking.map(val as u16),
                    0x4000..0x6000 => self.bus.sram_banking.map(val as u16),
                    0xa000..0xc000 => {
                        // RTC or IR mode
                    }
                    _ => {}
                }
            }
        }
    }
}

pub enum Mbc {
    None,
    Mbc1(Mbc1),
    Mbc2,
    Mbc3(Mbc3),
    Mbc5(Mbc5),
    Mbc6,
    Mbc7(Mbc7),
    Mmm01,
    M161(bool),
    HuC1,
    HuC3,
}

impl Mbc {
    pub fn new(bus: &mut Bus) -> Result<Self, String> {
        let res = match bus.header.mbc {
            0x00 | 0x08 | 0x09 => Mbc::None,
            0x01 | 0x02 | 0x03 => Mbc::Mbc1(Mbc1::new(bus)),
            0x05 | 0x06 => {
                bus.header.ram_size = 512;
                bus.sram = vec![0x0f; 512].into_boxed_slice();
                Mbc::Mbc2
            }
            0x0f..=0x13 => Mbc::Mbc3(Mbc3::default()),
            0x19..=0x1e => Mbc::Mbc5(Mbc5::default()),

            // 0x0b | 0x0c | 0x0d => {
            //     if false {
            //         // TODO: detect M161
            //         // maps a single bank to the whole rom address range, ????
            //         // double check this. might refer only to the usual rom1 banking
            //         bus.map[..0x8].fill(Handler::Rom1);
            //         bus.rom1_banking = Banker::new(0x8000, bus.header.rom_size);
            //         Mbc::M161(false)
            //     } else {
            //         Mbc::Mmm01
            //     }
            // }

            // 0x20 => Mbc::Mbc6,
            0x22 => Mbc::Mbc7(Mbc7::default()),

            0xfe => Mbc::HuC3,
            0xff => Mbc::HuC1,
            _ => return Err(format!("mapper {} not implemented", bus.header.mbc)),
        };

        Ok(res)
    }
}

#[derive(Default)]
pub(crate) struct Mbc1 {
    is_multicart: bool,
    mode: bool,
    rom_bank: u8,
    ram_bank: u8,
}
impl Mbc1 {
    pub fn new(bus: &mut Bus) -> Self {
        // detect MBC1M: it has a nintendo logo at bank 0x10
        let bank10_start = ROM_BANK_SIZE * 0x10;
        let logo_start = bank10_start + 0x104;
        let logo_end = logo_start + rom::NINTENDO_LOGO.len();
        let is_multicart = if logo_end >= bus.rom.len() {
            false
        } else {
            let nintendo_logo = &bus.rom[logo_start..logo_end];
            nintendo_logo == rom::NINTENDO_LOGO
        };

        Self {
            is_multicart,
            ..Default::default()
        }
    }

    pub fn update(&self, bus: &mut Bus) {
        if self.is_multicart {
            if self.mode {
                bus.rom0_banking.map((self.ram_bank as u16) << 4);
            } else {
                bus.rom0_banking.map(0);
            }

            bus.rom1_banking
                .map(((self.ram_bank << 4) | (self.rom_bank & 0x0f)) as u16);
        } else {
            if self.mode {
                bus.rom0_banking.map((self.ram_bank as u16) << 5);
                bus.sram_banking.map(self.ram_bank as u16);
            } else {
                bus.rom0_banking.map(0);
                bus.sram_banking.map(0);
            }

            bus.rom1_banking
                .map(((self.ram_bank << 5) | self.rom_bank) as u16);
        }
    }
}

#[derive(Default)]
pub(crate) struct Mbc3 {
    timer_enable: bool,
    timer_reg: u8,
}

#[derive(Default)]
pub(crate) struct Mbc5 {
    rom_bank: u16,
    rumble_enable: bool,
}

#[derive(Default)]
pub(crate) struct Mbc7 {
    ram_enable0: bool,
    ram_enable1: bool,
}

#[derive(Debug)]
struct Banker {
    banks_count_mask: u16,
    bank_size_mask: u16,
    bank_size_shift: u8,
    real_addr_start: u32,
}

impl Banker {
    pub fn new(virt_size: usize, real_size: usize) -> Self {
        // https://stackoverflow.com/questions/25787613/division-and-multiplication-by-power-of-2
        let bank_size_shift = virt_size.ilog2() as u8;
        // TODO: this doesn't work if bankSize is odd!
        let bank_size_mask = virt_size.saturating_sub(1) as u16;

        // TODO: if realSize < virtSize and there banks arent big enough, this becomes 0!!
        // TODO: handle unconvetional realSizes (less tha 8KiB)
        let banks_count = (real_size / virt_size) as u16;
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

    pub fn map(&mut self, bank: u16) {
        // some games might write bigger bank numbers than really avaible
        // let bank = bank % self.banks_count;
        // let bank = bank & (self.banks_count-1);
        let bank = bank & self.banks_count_mask;

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
