use crate::{emu::Emu, ppu};

#[derive(Clone, Copy)]
pub(crate) enum Handler {
  Rom, Vram, Sram, Wram, IO, OpenBus, Debug
}

struct Dma {
  src: u8,
  transfering: bool,
  count: u8,
}
impl Default for Dma {
  fn default() -> Self {
    Self {
      src: 0xff,
      transfering: false,
      count: 0,
    }
  }
}

pub(crate) struct Bus {
  pub(crate) handlers: [Handler; 16],
  boot_sector: Option<Vec<u8>>,
  hram: [u8; 127],
  rom: Vec<u8>,
  pub(crate) sram: Vec<u8>,
  pub(crate) vram: [u8; 16 * 1024],
  wram: [u8; 32 * 1024],
  pub(crate) oam: [u8; 160],

  dma: Dma,
}

impl Bus {
  pub fn new(mut rom: Vec<u8>, sram_size: usize) -> Self {
    let sram_handler = if sram_size > 0 { Handler::Sram } else { Handler::OpenBus };

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
      sram_handler,
      sram_handler,
      Handler::Wram,
      Handler::Wram,
      Handler::IO,
      Handler::IO,
    ];

    // TODO: dynamic bootrom load
    let bootrom = include_bytes!("../bootroms/dmg_boot.bin");

    // // we save the rom which is to be overlapped with the bootrom, and restore it later
    // let boot_sector = Some(rom[..256].to_vec());
    // rom[..256].copy_from_slice(bootrom);

    let boot_sector = None;

    Self {
      handlers,
      boot_sector,
      hram: [0xff; 127],
      rom,
      sram: vec![0; sram_size],
      vram: [0xff; 16 * 1024],
      wram: [0xff; 32 * 1024],
      oam: [0xff; 160],

      dma: Default::default(),
    }
  }
}

impl Emu {
  pub fn dma_step(&mut self) {
    if self.bus.dma.transfering {
      let src_addr = ((self.bus.dma.src as u16) << 8) | self.bus.dma.count as u16;
      let val = self.dispatch_read(src_addr);
      self.dispatch_write(0xfe00 | self.bus.dma.count as u16, val);
      self.bus.dma.count += 1;

      if self.bus.dma.count >= 160 {
        self.bus.dma.transfering = false;
      }
    }
  }

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
        // TODO: value returned depends on OAM blocked or not
        0xff
      } else if addr <= 0xff7f {
        self.handle_io_read(addr)
      } else if addr <= 0xfffe {
        bus.hram[addr - 0xff80]
      } else {
        self.inte.into_bits() | 0xe0
      }
      Handler::OpenBus => 0xff, 

      Handler::Debug => bus.sram[addr],
    }
  }

  pub fn dispatch_write(&mut self, addr: u16, val: u8) {
    let bus = &mut self.bus;
    let addr = addr as usize;

    let handler = &bus.handlers[(addr as usize) >> 12];
    match handler {
      Handler::Rom | Handler::OpenBus => {}
      Handler::Vram => bus.vram[addr - 0x8000] = val,
      Handler::Sram => bus.sram[addr - 0xa000] = val,
      Handler::Wram => bus.wram[addr - 0xc000] = val,
      Handler::IO => if addr <= 0xfdff {
        bus.wram[addr - 0xe000] = val;
      } else if addr <= 0xfe9f {
        bus.oam[addr - 0xfe00] = val;
      } else if addr <= 0xfeff {
        // Do nothing here
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
      0xff00 => self.joypad.read(),
      0xff01 => 0,
      0xff02 => self.serial.into_bits(),

      0xff04 => (self.timer.div >> 8) as u8,
      0xff05 => self.timer.tima,
      0xff06 => self.timer.tma,
      0xff07 => self.timer.tac,

      0xff0f => self.intf.into_bits() | 0xe0,

      0xff40 => self.ppu.ctrl.into_bits(),
      0xff41 => self.ppu.stat.into_bits(),
      0xff42 => self.ppu.scy,
      0xff43 => self.ppu.scx,
      0xff44 => self.ppu.ly,
      0xff45 => self.ppu.lyc,
      0xff46 => self.bus.dma.src,
      0xff47 => self.ppu.bgp,
      0xff48 => self.ppu.obp0,
      0xff49 => self.ppu.obp1,
      0xff4a => self.ppu.wy,
      0xff4b => self.ppu.wx,

      _ => 0xff
    }
  }

  fn handle_io_write(&mut self, addr: usize, val: u8) {
    match addr {
      0xff00 => self.joypad.write(val),
      0xff02 => self.serial.set_bits(val),

      0xff04 => self.timer.div = 0,
      0xff05 => self.timer.tima = val,
      0xff06 => self.timer.tma = val,
      0xff07 => {
        self.timer.tac = 0xf8 | (val & 0x7);
        self.timer.clock_mask = match val & 0x3 {
          0 => 255,
          1 => 3,
          2 => 15,
          _ => 63
        };
      }

      0xff0f => self.intf.set_bits(val),

      0xff40 => {
        let new_ctrl = ppu::Ctrl::from_bits(val);
        
        // handle LCD enable/disable
        if self.ppu.ctrl.lcd_enable() != new_ctrl.lcd_enable() {
          if new_ctrl.lcd_enable() { self.lcd_set_enabled(); } else { self.lcd_set_disabled(); }
        }
        
        let ppu = &mut self.ppu;
        ppu.ctrl = new_ctrl;
        ppu.obj_size = if ppu.ctrl.obj_size() { 16 } else { 8 };
        ppu.bg_tilemap  = if ppu.ctrl.bg_tilemap() { 0x9c00 } else { 0x9800 };
        ppu.win_tilemap = if ppu.ctrl.win_tileamp() { 0x9c00 } else { 0x9800 };
      }
      0xff41 => {
        let stat = self.ppu.stat.into_bits();
        self.ppu.stat.set_bits((val & 0b0111_1000) | (stat & 0b1000_0111));
        self.handle_stat_int();
      }
      0xff42 => self.ppu.scy = val,
      0xff43 => self.ppu.scx = val,
      0xff45 => {
        self.ppu.lyc = val;
        if self.ppu.ctrl.lcd_enable() {
          self.handle_lyc();
        }
      }
      0xff46 => {
        self.bus.dma.src = val;
        self.bus.dma.transfering = true;
        self.bus.dma.count = 0;
      }
      0xff47 => {
        self.ppu.bgp = val;
        dbg!(self.ppu.bgp);
      }
      0xff48 => self.ppu.obp0 = val,
      0xff49 => self.ppu.obp1 = val,
      0xff4a => self.ppu.wy = val,
      0xff4b => self.ppu.wx = val,

      0xff50 => if let Some(boot_sector) = self.bus.boot_sector.take() {
        self.bus.rom[..256].copy_from_slice(&boot_sector);
      }
      _ => {}
    }
  }
}