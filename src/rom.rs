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
    pub fn from(bytes: &[u8]) -> Result<Self, &'static str> {
        Ok(Self {
            header: RomData::parse(bytes)?,
            rom: bytes.to_vec(),
        })
    }
}

#[derive(Default, Debug, Clone)]
enum ConsoleMode {
    #[default]
    DMG,
    Compat,
    CGB,
}

#[derive(Default, Debug, Clone)]
enum Region {
    #[default]
    World,
    Oversea,
}

#[derive(Default, Debug, Clone)]
pub struct RomData {
    title: String,
    revision: u8,
    mode: ConsoleMode,
    sgb: bool,
    mapper: u8,
    rom_size: usize,
    ram_size: usize,
    region: Region,
}
impl RomData {
    const MAGIC: [u8; 48] = [
        0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00,
        0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD,
        0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB,
        0xB9, 0x33, 0x3E,
    ];

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
        header.mapper = bytes[0x147];

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

        header.revision = bytes[0x14c];

        Ok(header)
    }
}

pub fn is_valid_rom(bytes: &[u8]) -> bool {
    bytes.len() > 0x14f && &bytes[0x104..0x104 + RomData::MAGIC.len()] == RomData::MAGIC
}

// TODO: use bios CRC
pub fn is_valid_bios(bios: &[u8]) -> bool {
    bios.len() == 0x100
}
