use crate::bus::{Banking, Bus, Handler};

pub (crate) enum Mbc {
  None,
  MBC1(MBC1),
  MBC2,
  MBC3(MBC3),
  MBC5(MBC5),
}
impl Mbc {
  pub(crate) fn new(mapper: u8) -> Result<Mbc, String> {
    let res = match mapper {
      0x0 | 0x8 | 0x9 => Mbc::None,
      0x1 | 0x2 | 0x3 => Mbc::MBC1(MBC1::default()),
      0x5 | 0x6 => Mbc::MBC2,
      0x0f..=0x13 => Mbc::MBC3(MBC3::default()),
      0x19..=0x1e => Mbc::MBC5(MBC5::default()),
      // 0xb | 0xc | 0xd => todo!("mmm01"),
      // 0x20 => todo!("mbc6"),
      // 0x22 => todo!("mbc7"),
      // 0xfc => todo!("pocket camera"),
      // 0xfd => todo!("bandai tama5"),
      // 0xfe => todo!("huc3"),
      // 0xff => todo!("huc1"),
      _ => return Err(format!("mapper {mapper:x} not implemented")),
    };
  
    Ok(res)
  }

  pub(crate) fn init(&self, bus: &mut Bus) {
    match self {
      Mbc::MBC1(mbc) => {
        mbc.update_banks(bus);
        bus.sram_enable(false);
      }
      Mbc::MBC2 => {
        bus.sram.resize(512, 0);
        bus.sram_banks = Banking::new(512, 0x2000, 16);
        for i in 0..16 { bus.sram_banks.set_page(i, 0); }
        bus.sram_enable(false);
      }
      _ => {}
    };
  }

  pub(crate) fn read(&mut self) -> u8 {
    match self {
      Mbc::MBC3(mbc) => {
        0xff
      }
      _ => 0xff
    }
  }

  pub(crate) fn write(&mut self, val: u8) {
    match self {
      Mbc::MBC3(mbc) => {

      }
      _ => {}
    }
  }
}

#[derive(Debug)]
pub(crate) struct MBC1 {
  pub(crate) mode: bool,
  pub(crate) rom_select: u8,
  pub(crate) ram_select: u8,
}
impl Default for MBC1 {
  fn default() -> Self {
    Self {
      mode: false,
      rom_select: 1,
      ram_select: 0,
    }
  }
}

impl MBC1 {
  pub fn update_banks(&self, bus: &mut Bus) {
    if self.mode {
      bus.rom_banks.set_page(0, (self.ram_select as usize) << 5);
      bus.rom_banks.set_page(1, ((self.ram_select as usize) << 5) | self.rom_select as usize);
      bus.sram_banks.set_page(0, self.ram_select as usize);
    } else {
      bus.rom_banks.set_page(0, 0);
      bus.rom_banks.set_page(1, self.rom_select as usize);
      bus.sram_banks.set_page(0, 0);
    }
  }
}

#[derive(Default)]
pub(crate) struct MBC3 {
  pub ext_enabled: bool,
  pub rtc_mode: bool,
  pub rtc_select: u8,
  pub rtc_latch: u8,
}
impl MBC3 {
  fn rtc_read(&self, reg: u8) -> u8 {
    todo!()
  }

  fn rtc_write(&mut self, reg: u8, val: u8) {
    todo!()
  }
}

#[derive(Default)]
pub(crate) struct MBC5 {
  pub rom_select: u16,
  pub rumble_enabled: bool,
}