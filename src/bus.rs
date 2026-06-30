use crate::{
    emu::GbEmulator,
    rom::{Cart, RomData},
};

enum Handler {
    Rom,
    Vram, // Video RAM
    Sram, // Cartridge RAM
    Wram, // Work RAM
    IO,
    Debug,
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

pub(crate) struct Bus {
    rom: Box<[u8]>,
    bios: Box<[u8]>,
    pub(crate) header: RomData,

    hram: [u8; 128],
    vram: Box<[u8]>,
    sram: Box<[u8]>,
    wram: Box<[u8]>,

    map: [Handler; 16],
}

impl Bus {
    pub fn with_ram_64kb() -> Self {
        Self {
            rom: Default::default(),
            bios: Default::default(),
            header: Default::default(),

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
            bios: bios.into_boxed_slice(),
            header: cart.header,

            hram: [0; _],
            vram: vec![0; 8 * 1024].into_boxed_slice(),
            sram: Default::default(),
            wram: vec![0; 8 * 1024].into_boxed_slice(),

            map: DEFAULT_MAP,
        };

        // at boot bios is mapped to the first 0x100 bytes
        // swap bios out with the first 0x100, set them back later
        let tmp = res.rom[..0x100].to_vec();
        res.rom[..0x100].copy_from_slice(&res.bios);
        res.bios = tmp.into_boxed_slice();

        res
    }
}

impl GbEmulator {
    pub fn dispatch_read(&mut self, addr: u16) -> u8 {
        let bus = &mut self.bus;

        let handler = addr >> 12;
        match bus.map[handler as usize] {
            Handler::Rom => bus.rom[addr as usize],
            Handler::Vram => bus.vram[addr as usize - 0x8000],
            Handler::Sram => 0,
            Handler::Wram => bus.wram[addr as usize - 0xa000],
            Handler::IO => self.io_read(addr),
            Handler::Debug => bus.wram[addr as usize],
        }
    }

    pub fn dispatch_write(&mut self, addr: u16, val: u8) {
        let bus = &mut self.bus;

        let handler = addr >> 12;
        match bus.map[handler as usize] {
            Handler::Rom => bus.rom[addr as usize] = val,
            Handler::Vram => bus.vram[addr as usize - 0x8000] = val,
            Handler::Sram => {}
            Handler::Wram => bus.wram[addr as usize - 0xa000] = val,
            Handler::IO => self.io_write(addr, val),
            Handler::Debug => bus.wram[addr as usize] = val,
        }
    }

    fn io_read(&mut self, addr: u16) -> u8 {
        todo!()
    }

    fn io_write(&mut self, addr: u16, val: u8) {
        todo!()
    }
}
