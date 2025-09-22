use crate::{emu::Emu, mbc::Mbc, ppu};

#[derive(Debug)]
pub (crate) struct Banking {
  // 4 kb banks
  banks: Vec<usize>,
  bank_size: u16,
  bank_size_shift: u16,
  banks_count: usize,
}

impl Banking {
  pub(crate) fn new(data_size: usize, adressable_size: u16, pages_count: u8) -> Self {
    let bank_size = adressable_size / pages_count as u16; 
    let banks = (0..pages_count as usize)
      .map(|i| i * bank_size as usize)
      .collect();
    
    Self {
      banks,
      bank_size,
      bank_size_shift: bank_size.ilog2() as u16,
      banks_count: data_size / bank_size as usize,
    }
  }

  pub (crate) fn set_page(&mut self, page: u8, bank: usize) {
    let bank = bank & (self.banks_count-1);
    // 12 is log2 of 4096 (the size of a single bank)
    // this means an offset is 12bit, and a bank select is in the upper bits
    // same as (bank as usize) * 4096
    self.banks[page as usize] = (bank as usize) << self.bank_size_shift;
  }

  fn translate(&self, addr: usize) -> usize {
    let page = (addr >> self.bank_size_shift) & (self.banks.len()-1);
    self.banks[page] + (addr & (self.bank_size as usize-1))
  }
}

#[derive(Clone, Copy)]
pub(crate) enum Handler {
  Rom, Vram, Sram, MBC, Wram, IO, OpenBus, Debug
}

pub(crate) struct Dma {
  pub(crate) src: u8,
  pub(crate) transfering: bool,
  pub(crate) count: u8,
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

pub struct Bus {
  pub(crate) handlers: [Handler; 16],
  boot_sector: Option<Vec<u8>>,
  pub(crate) hram: [u8; 127],
  pub(crate) rom: Vec<u8>,
  pub sram: Vec<u8>,
  pub vram: [u8; 16 * 1024],
  pub(crate) wram: [u8; 32 * 1024],
  pub(crate) oam: [u8; 160],
  pub(crate) oam_enable: bool,

  pub(crate) rom_banks: Banking,
  pub(crate) sram_banks: Banking,
  pub(crate) dma: Dma,

  dobule_speed: bool,
}

impl Bus {
  pub fn new(rom: &[u8], sram_size: usize) -> Self {
    let sram_handler = if sram_size > 0 { Handler::Sram } else { Handler::OpenBus };

    // 4kb handlers
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

    let mut rom = rom.to_vec();

    // // TODO: dynamic bootrom load
    // let bootrom = include_bytes!("../bootroms/dmg_boot.bin");

    // // we save the rom which is to be overlapped with the bootrom, and restore it later
    // let boot_sector = Some(rom[..256].to_vec());
    // rom[..256].copy_from_slice(bootrom);

    let boot_sector = None;

    Self {
      handlers,
      rom_banks: Banking::new(rom.len(), 0x8000, 2),
      sram_banks: Banking::new(sram_size, 0x2000, 1),

      boot_sector,
      hram: [0; 127],
      rom,
      sram: vec![0; sram_size],
      vram: [0; 16 * 1024],
      wram: [0; 32 * 1024],
      oam: [0; 160],
      oam_enable: true,
      dobule_speed: false,

      dma: Default::default(),
    }
  }

  pub(crate) fn set_sram_handlers(&mut self, handler: Handler) {
    self.handlers[10] = handler;
    self.handlers[11] = handler;
  }

  fn set_vram_handlers(&mut self, handler: Handler) {
    self.handlers[8] = handler;
    self.handlers[9] = handler;
  }

  pub(crate) fn sram_enable(&mut self, cond: bool) {
    if !self.sram.is_empty() && cond {
      self.set_sram_handlers(Handler::Sram);
    } else {
      self.set_sram_handlers(Handler::OpenBus);
    }
  }

  pub(crate) fn vram_enable(&mut self, cond: bool) {
    if cond {
      self.set_vram_handlers(Handler::Vram);
    } else {
      self.set_vram_handlers(Handler::OpenBus);
    }
  }
}

impl Emu {
  pub fn dispatch_read(&mut self, addr: u16) -> u8 {
    let bus = &mut self.bus;
    let addr: usize = addr as usize;

    let handler = &bus.handlers[addr >> 12];
    match handler {
      Handler::Rom => bus.rom[bus.rom_banks.translate(addr)],
      Handler::Vram => bus.vram[addr - 0x8000],
      Handler::Sram => bus.sram[bus.sram_banks.translate(addr - 0xa000)],
      Handler::Wram => bus.wram[addr - 0xc000],
      Handler::IO => if addr <= 0xfdff {
        bus.wram[addr - 0xe000]
      } else if addr <= 0xfe9f {
        if bus.oam_enable {
          bus.oam[addr - 0xfe00]
        } else { 0xff }
      } else if addr <= 0xfeff {
        // TODO: value returned depends on OAM blocked or not
        0xff
      } else if addr <= 0xff7f {
        self.handle_io_read(addr)
      } else if addr <= 0xfffe {
        bus.hram[addr - 0xff80]
      } else {
        self.inte.into_bits()
      }
      Handler::OpenBus => 0xff,
      Handler::MBC => self.mbc.read(),

      Handler::Debug => bus.sram[addr],
    }
  }

  pub fn dispatch_write(&mut self, addr: u16, val: u8) {
    let bus = &mut self.bus;
    let addr = addr as usize;

    let handler = &bus.handlers[(addr as usize) >> 12];
    match handler {
      Handler::Rom => self.handle_mbc_write(addr, val),
      Handler::Vram => bus.vram[addr - 0x8000] = val,
      Handler::Sram => bus.sram[bus.sram_banks.translate(addr - 0xa000)] = val,
      Handler::Wram => bus.wram[addr - 0xc000] = val,
      Handler::IO => if addr <= 0xfdff {
        bus.wram[addr - 0xe000] = val;
      } else if addr <= 0xfe9f {
        if bus.oam_enable {
          bus.oam[addr - 0xfe00] = val;
        }
      } else if addr <= 0xfeff {
        // Do nothing here
      } else if addr <= 0xff7f {
        self.handle_io_write(addr, val)
      } else if addr <= 0xfffe {
        bus.hram[addr - 0xff80] = val;
      } else {
        self.inte.set_bits(val);
      }
      Handler::OpenBus => {}
      Handler::MBC => self.mbc.write(val),

      Handler::Debug => bus.sram[addr] = val,
    }
  }

  fn handle_io_read(&mut self, addr: usize) -> u8 {    
    match addr {
      0xff00 => {
        // println!("Joypad read: {:02x}", self.joypad.read());
        self.joypad.read()
      }
      0xff01 => {
        // println!("Serial data read: {} {:02x}", self.serial.data, self.serial.data);
        self.serial.data
      }
      0xff02 => {
        // println!("Serial ctrl read: {:02x}", self.serial.flags.into_bits());
        self.serial.flags.into_bits()
      }

      // div is a 14bit register
      0xff04 => (self.timer.div >> 6) as u8,
      0xff05 => self.timer.tima,
      0xff06 => self.timer.tma,
      0xff07 => self.timer.tac,

      0xff0f => self.intf.into_bits(),

      0xff40 => self.ppu.ctrl.into_bits(),
      0xff41 => {
        // println!("READ STAT {:?}", self.ppu.stat);
        self.ppu.stat.into_bits()
      }
      0xff42 => self.ppu.scy,
      0xff43 => self.ppu.scx,
      0xff44 => {
        // println!("READ LY {}", self.ppu.ly);
        self.ppu.ly
      }
      0xff45 => {
        // println!("READ LYC {}", self.ppu.lyc);
        self.ppu.lyc
      }
      0xff46 => self.bus.dma.src,
      0xff47 => {
        // println!("READ BGP {}", self.ppu.bgp);
        self.ppu.bgp
      }
      0xff48 => self.ppu.obp0,
      0xff49 => self.ppu.obp1,
      0xff4a => self.ppu.wy,
      0xff4b => self.ppu.wx,

      _ => 0xff
    }
  }

  fn handle_io_write(&mut self, addr: usize, val: u8) {
    match addr {
      0xff00 => {
        self.joypad.write(val);
        // println!("Joypad write: {} {:02x}", val, val);
      }
      0xff01 => {
        self.serial.data = val;
        // println!("Serial data write: {} {:02x}", val, val);
      }
      0xff02 => {
        self.serial.flags.set_bits(val);
        if self.serial.flags.tx_enable() && self.serial.flags.clock_select() {
          self.serial.count = 8;
        } else {
          self.serial.count = 0;
        }

        // println!("Serial ctrl write: {:02x}", val);
      }

      0xff04 => {
        self.timer.div = 0;
        // self.timer_tima_step();
      }
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
        // self.timer_tima_step();
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

        // println!("WRITE CTRL {:?}", self.ppu.ctrl);
      }
      0xff41 => {
        let stat = self.ppu.stat.into_bits();
        self.ppu.stat.set_bits((val & 0b0111_1000) | (stat & 0b1000_0111));
        if self.ppu.ctrl.lcd_enable() {
          self.handle_stat_int();
        }
        // println!("WRITE STAT {:?}", self.ppu.stat);
      }
      0xff42 => self.ppu.scy = val,
      0xff43 => self.ppu.scx = val,
      0xff45 => {
        self.ppu.lyc = val;
        // println!("WRITE LYC {}", self.ppu.lyc);
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
        // println!("WRITE BGP {} at {} {} {}", self.ppu.bgp, self.ppu.ly, self.ppu.dots, self.ppu.fetcher.pixel_x);
      }
      0xff48 => self.ppu.obp0 = val,
      0xff49 => self.ppu.obp1 = val,
      0xff4a => {
        self.ppu.wy = val;
        // println!("WRITE WY {}", self.ppu.wy);
      }
      0xff4b => self.ppu.wx = val,

      0xff50 => if let Some(boot_sector) = self.bus.boot_sector.take() {
        self.bus.rom[..256].copy_from_slice(&boot_sector);
      }
      _ => {}
    }
  }

  fn handle_mbc_write(&mut self, addr: usize, val: u8) {
    match &mut self.mbc {
      Mbc::None => {}
      Mbc::MBC1(mbc) => match addr >> 12 {
        0 | 1 => self.bus.sram_enable(val & 0xf == 0xa),
        2 | 3 => {
          mbc.rom_select = (val & 0x1f).max(1);
          mbc.update_banks(&mut self.bus);
        }
        4 | 5 => {
          mbc.ram_select = val & 0x3;
          mbc.update_banks(&mut self.bus);
        }
        6 | 7 => {
          mbc.mode = val & 1 > 0;
          mbc.update_banks(&mut self.bus);
        }
        _ => {}
      }

      Mbc::MBC2 => match addr >> 12 {
        0..=3 => if addr & 0x100 > 0 {
          self.bus.rom_banks.set_page(1, (val & 0xf).max(1) as usize);
        } else {
          self.bus.sram_enable(val & 0xf == 0xa);
        }
        _ => {}
      }

      Mbc::MBC3(mbc) => match addr >> 12 {
        0 | 1 => if val == 0x0a {
          mbc.ext_enabled = true;
          let handler = if mbc.rtc_mode { Handler::MBC } else { Handler::Sram };
          self.bus.set_sram_handlers(handler);
        } else {
          mbc.ext_enabled = false;
          self.bus.set_sram_handlers(Handler::OpenBus);
        }
        2 | 3 => self.bus.rom_banks.set_page(1, (val & 0x7f).max(1) as usize),
        4 | 5 => if val <= 0x7 {
          // ram mode
          self.bus.sram_banks.set_page(0, val as usize);
          self.bus.set_sram_handlers(Handler::Sram);
          mbc.rtc_mode = false;
        } else if val <= 0xc {
          // rtc mode
          mbc.rtc_select = val;
          self.bus.set_sram_handlers(Handler::MBC);
          mbc.rtc_mode = true;
        }
        6 | 7 => {
          if mbc.rtc_latch == 0 && val == 1 {
            // TODO: latch current date
          }
          mbc.rtc_latch = val;
        }
        _ => {}
      }

      Mbc::MBC5(mbc) => match addr >> 12 {
        0 | 1 => if val == 0x0a {
          self.bus.sram_enable(true);
        } else if val == 0 {
          self.bus.sram_enable(false);
        }

        2 => {
          mbc.rom_select = (mbc.rom_select & 0xff00) | val as u16;
          self.bus.rom_banks.set_page(1, mbc.rom_select as usize);
        }
        3 => {
          mbc.rom_select = (mbc.rom_select & 0x00ff) | ((val as u16 & 1) << 8);
          self.bus.rom_banks.set_page(1, mbc.rom_select as usize);
        }
        4 | 5 => if val <= 0xf {
          self.bus.rom_banks.set_page(0, val as usize);
          mbc.rumble_enabled = val & 0x8 > 0;
        }
        _ => {}
      }
    }
  }
}