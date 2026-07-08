use crate::emu::LoadError;

pub(crate) struct Cart {
    pub header: RomData,
    pub rom: Vec<u8>,
}
impl Default for Cart {
    fn default() -> Self {
        // empty cart with zeroed rom
        Self {
            header: RomData {
                rom_size: 32 * 1024,
                ..Default::default()
            },
            rom: vec![0; 32 * 1024],
        }
    }
}

impl Cart {
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, LoadError> {
        Ok(Self {
            header: RomData::parse(bytes)?,
            rom: bytes.to_vec(),
        })
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub enum ConsoleMode {
    #[default]
    DMG,
    Compat,
    CGB,
}

#[derive(Default, Debug, Clone)]
pub enum Region {
    #[default]
    World,
    Oversea,
}

pub const NINTENDO_LOGO: [u8; 48] = [
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

#[derive(Default, Debug, Clone)]
pub struct RomData {
    pub title: String,
    pub mbc_name: &'static str,
    pub revision: u8,
    pub mode: ConsoleMode,
    pub sgb: bool,
    pub mbc: u8,
    pub rom_size: usize,
    pub ram_size: usize,
    pub region: Region,

    pub battery: bool,
    pub timer: bool,
    pub rumble: bool,
    pub sensor: bool,
}
impl RomData {
    pub fn is_cgb(&self) -> bool {
        self.mode != ConsoleMode::DMG
    }

    pub fn parse(bytes: &[u8]) -> Result<Self, &'static str> {
        if !is_valid_rom(bytes) {
            return Err("invalid GameBoy rom");
        }

        let mut header = RomData::default();
        header.title = String::from_utf8_lossy(&bytes[0x134..0x144])
            .trim()
            .trim_matches(|c: char| c.is_control())
            .to_string();

        header.mode = match bytes[0x143] {
            0x80 => ConsoleMode::Compat,
            0xc0 => ConsoleMode::CGB,
            _ => ConsoleMode::DMG,
        };

        header.sgb = bytes[0x146] == 0x03;
        header.mbc = bytes[0x147];

        header.rom_size = match bytes[0x148] {
            0x52 => 1100 * 1024,
            0x53 => 1200 * 1024,
            0x54 => 1500 * 1024,
            (0x00..=0x08) => 32 * 1024 * (1 << bytes[0x148]),
            _ => 32 * 1024,
        };

        header.ram_size = match bytes[0x149] {
            0x2 => 8 * 1024,
            0x3 => 32 * 1024,
            0x4 => 128 * 1024,
            0x5 => 64 * 1024,
            _ => 0,
        };
        header.region = match bytes[0x14a] {
            0x1 => Region::Oversea,
            _ => Region::World,
        };

        header.battery = [
            0x03, 0x06, 0x09, 0x0d, 0x0f, 0x10, 0x13, 0x1b, 0x1d, 0x1e, 0x22, 0xff,
        ]
        .contains(&header.mbc);

        header.timer = [0x0f, 0x10].contains(&header.mbc);
        header.rumble = [0x1c, 0x1d, 0x1e, 0x22].contains(&header.mbc);
        header.sensor = [0x22].contains(&header.mbc);

        header.revision = bytes[0x14c];
        header.mbc_name = MBC_NAMES
            .iter()
            .find(|x| x.0 == header.mbc)
            .map(|x| x.1)
            .unwrap_or("Unknown");

        println!("{header:?}");

        Ok(header)
    }
}

pub fn is_valid_rom(bytes: &[u8]) -> bool {
    bytes.len() > 0x14f && &bytes[0x104..0x104 + NINTENDO_LOGO.len()] == NINTENDO_LOGO
}

// TODO: use bios CRCs
pub fn is_valid_bios(bios: &[u8]) -> bool {
    bios.len() == 0x100
}

const MBC_NAMES: &[(u8, &'static str)] = &[
    (0x00, "ROM Only"),
    (0x01, "MBC1"),
    (0x02, "MBC1"),
    (0x03, "MBC1"),
    (0x05, "MBC2"),
    (0x06, "MBC2"),
    (0x08, "ROM Only"),
    (0x09, "ROM Only"),
    (0x0B, "MMM01"),
    (0x0C, "MMM01"),
    (0x0D, "MMM01"),
    (0x0F, "MBC3"),
    (0x10, "MBC3"),
    (0x11, "MBC3"),
    (0x12, "MBC3"),
    (0x13, "MBC3"),
    (0x19, "MBC5"),
    (0x1A, "MBC5"),
    (0x1B, "MBC5"),
    (0x1C, "MBC5"),
    (0x1D, "MBC5"),
    (0x1E, "MBC5"),
    (0x20, "MBC6"),
    (0x22, "MBC7"),
    (0xFC, "Pocket Camera"),
    (0xFD, "BANDAI TAMA5"),
    (0xFE, "HuC3"),
    (0xFF, "HuC1"),
    // TODO: these require different methods to detect
    (0xA0, "M161"),
    (0xA1, "EMS"),
    (0xA2, "Bung"),
    (0xA3, "WisdomTree"),
];
