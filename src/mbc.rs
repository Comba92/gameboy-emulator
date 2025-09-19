use crate::bus::Bus;

pub (crate) enum Mbc {
  None,
  MBC1(MBC1),
}
impl Mbc {
  pub(crate) fn new(mapper: u8) -> Result<Mbc, String> {
    let res = match mapper {
      0x0 | 0x8 | 0x9 => Mbc::None,
      0x1 | 0x2 | 0x3 => Mbc::MBC1(MBC1::default()),
      _ => return Err(format!("mapper {mapper:x} not implemented")),
    };
  
    Ok(res)
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
      bus.rom_banks.set_page(1, ((self.ram_select as usize) << 5) + self.rom_select as usize);
      bus.sram_banks.set_page(0, self.ram_select as usize);
    } else {
      bus.rom_banks.set_page(0, 0);
      bus.rom_banks.set_page(1, self.rom_select as usize);
      bus.sram_banks.set_page(0, 0);
    }
  }
}