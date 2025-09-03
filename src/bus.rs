use crate::emu::Emu;

#[derive(Clone, Copy)]
pub(crate) enum Handler {
  Rom, Vram, Sram, Wram, IO, Debug
}

pub(crate) struct Bus {
  // TODO: consider make this const, as handlers should not change
  pub(crate) handlers: [Handler; 16],
  boot_sector: Option<Vec<u8>>,
  hram: [u8; 127],
  rom: Vec<u8>,
  pub(crate) sram: Vec<u8>,
  vram: [u8; 16 * 1024],
  wram: [u8; 32 * 1024],
  oam: [u8; 160],
}

impl Bus {
  pub fn new(mut rom: Vec<u8>) -> Self {
    let handlers = [
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

    // TODO: dynamic bootrom load
    let bootrom = include_bytes!("../bootroms/dmg_boot.bin");

    // we save the rom which is to be overlapped with the bootrom, and restore it later
    let boot_sector = Some(rom[..256].to_vec());
    rom[..256].copy_from_slice(bootrom);

    Self {
      handlers,
      boot_sector,
      hram: [0xff; 127],
      rom,
      sram: Vec::new(),
      vram: [0xff; 16 * 1024],
      wram: [0xff; 32 * 1024],
      oam: [0xff; 160],
    }
  }
}

impl Emu {
  pub fn dispatch_read(&mut self, addr: u16) -> u8 {
    let bus = &mut self.bus;
    let addr = addr as usize;

    let handler = &bus.handlers[addr >> 12];
    match handler {
      Handler::Rom => bus.rom[addr],
      Handler::Vram => bus.vram[addr - 0x8000],
      Handler::Sram => bus.sram[addr - 0xa000],
      Handler::Wram => bus.wram[addr - 0xc000],
      Handler::IO => if addr <= 0xfdff {
        bus.wram[addr - 0xe000]
      } else if addr <= 0xfe9f {
        bus.oam[addr - 0xfe00]
      } else if addr <= 0xfeff {
        todo!("unusable area")
      } else if addr <= 0xff7f {
        self.handle_io_read(addr)
      } else if addr <= 0xfffe {
        bus.hram[addr - 0xff80]
      } else {
        self.inte.into_bits() | 0xe0
      }

      Handler::Debug => bus.sram[addr],
    }
  }

  pub fn dispatch_write(&mut self, addr: u16, val: u8) {
    let bus = &mut self.bus;
    let addr = addr as usize;

    let handler = &bus.handlers[(addr as usize) >> 12];
    match handler {
      Handler::Rom => {}
      Handler::Vram => bus.vram[addr - 0x8000] = val,
      Handler::Sram => bus.sram[addr - 0xa000] = val,
      Handler::Wram => bus.wram[addr - 0xc000] = val,
      Handler::IO => if addr <= 0xfdff {
        bus.wram[addr - 0xe000] = val;
      } else if addr <= 0xfe9f {
        bus.oam[addr - 0xfe00] = val;
      } else if addr <= 0xfeff {
        todo!("unusable area")
      } else if addr <= 0xff7f {
        self.handle_io_write(addr, val)
      } else if addr <= 0xfffe {
        bus.hram[addr - 0xff80] = val;
      } else {
        self.inte.set_bits(val);
      }

      Handler::Debug => bus.sram[addr] = val,
    }
  }

  fn handle_io_read(&mut self, addr: usize) -> u8 {
    match addr {
      0xff0f => self.intf.into_bits() | 0xe0,
      _ => 0xff
    }
  }

  fn handle_io_write(&mut self, addr: usize, val: u8) {
    match addr {
      0xff0f => self.intf.set_bits(val),
      0xff50 => if let Some(boot_sector) = self.bus.boot_sector.take() {
        self.bus.rom[..256].copy_from_slice(&boot_sector);
      }
      _ => {}
    }
  }
}