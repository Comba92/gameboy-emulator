use crate::emu::CGBMode;

#[derive(Debug, Default)]
pub(crate) struct CartHeader {
  pub title: String,
  pub mapper: u8,
  pub rom_size: usize,
  pub ram_size: usize,
  pub has_battery: bool,
  pub sgb_mode: bool,
  pub cgb_mode: CGBMode,
  pub region: Region,
  pub version: u8,
}

#[derive(Debug, Default)]
pub(crate) enum Region { Japan, #[default] Overseas }

impl CartHeader {
  pub fn parse(bytes: &[u8]) -> Result<Self, &'static str> {
    if bytes.len() < 0x14f {
      return Err("not a valid gameboy rom");
    }

    let mut header = Self::default();
    header.title = String::from_utf8_lossy(&bytes[0x134..0x143])
      .trim_matches(|c: char| c.is_control() || c.is_whitespace())
      .to_string();

    header.cgb_mode = match bytes[0x143] {
      0x80 => CGBMode::ColorEnhanced,
      0xc0 => CGBMode::ColorOnly,
      _ => CGBMode::Monochrome
    };
    header.sgb_mode = bytes[0x146] == 0x3;
    header.mapper = bytes[0x147];
    header.has_battery = [0x3, 0x6, 0x9, 0xd, 0xf, 0x10, 0x13, 0x1b, 0x1e, 0x22, 0xff].contains(&header.mapper);
    header.rom_size = 32 * 1024 * (1 << bytes[0x148]); 
    header.ram_size = 1024 * match bytes[0x149] {
      2 => 8,
      3 => 32,
      4 => 128,
      5 => 64,
      _ => 0
    };
    header.region = if bytes[0x14a] == 0 {
      Region::Japan
    } else { Region::Overseas };
    header.version = bytes[0x14c];

    println!("==[GAME LOADED]==\n{header:?}");

    Ok(header)
  }
}