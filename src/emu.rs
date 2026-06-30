use crate::{
    bus::Bus,
    cpu::CpuSm83,
    ppu::Ppu,
    rom::{Cart, is_valid_bios},
};
use std::path::Path;

const DMG_CLOCK_RATE: usize = 4194304;
const CBG_CLOCK_RATE: usize = 2 * DMG_CLOCK_RATE;
const FRAME_RATE: f32 = 59.73;

const SCREEN_WIDTH: isize = 160;
const SCREEN_HEIGHT: isize = 144;

pub const FRAMEBUF_SIZE: usize = SCREEN_WIDTH as usize * SCREEN_HEIGHT as usize * 4;
pub const AUDIO_FRAMES_BUFFERED: usize = 8;

pub(crate) type LoadError = Box<dyn std::error::Error>;

#[derive(Default)]
struct GbOutput {
    pub(crate) frame_ready: bool,
    pub(crate) frame_number: usize,
    pub(crate) videobuf_back: Box<Framebuf>,
    pub(crate) videobuf_view: Box<Framebuf>,
}

pub(crate) struct Framebuf(pub [u8; FRAMEBUF_SIZE]);
impl Default for Framebuf {
    fn default() -> Self {
        Self([255; _])
    }
}

pub struct GbEmulator {
    pub cpu: CpuSm83,
    pub(crate) bus: Bus,
    ppu: Ppu,

    output: GbOutput,
}

impl GbEmulator {
    pub fn debug() -> Self {
        Self {
            cpu: CpuSm83::new(),
            bus: Bus::with_ram_64kb(),
            ppu: Ppu::new(),
            output: Default::default(),
        }
    }

    fn new<B: AsRef<[u8]>>(game: Cart, bios: Option<B>) -> Result<Self, LoadError> {
        if let Some(bios) = &bios {
            if !is_valid_bios(bios.as_ref()) {
                return Err("invalid GameBoy boot rom".into());
            }
        }

        Ok(Self {
            cpu: CpuSm83::new(),
            ppu: Ppu::new(),
            bus: Bus::new(
                game,
                bios.map(|x| x.as_ref().to_vec())
                    .unwrap_or_else(|| vec![0; 0x100]),
            ),
            output: GbOutput::default(),
        })
    }

    pub fn load_rom_from_unzipped_bytes<R: AsRef<[u8]>, B: AsRef<[u8]>>(
        rom: R,
        bios: Option<B>,
    ) -> Result<Self, LoadError> {
        let game = Cart::from(rom.as_ref())?;
        Self::new(game, bios)
    }

    pub fn load_rom_from_bytes<R: AsRef<[u8]>, B: AsRef<[u8]>>(
        rom_bytes: R,
        bios: Option<B>,
    ) -> Result<Self, LoadError> {
        match read_zip_file_from_bytes(rom_bytes.as_ref()) {
            Ok(unzipped_bytes) => Self::load_rom_from_unzipped_bytes(unzipped_bytes, bios), // it is a zip file
            Err(_) => Self::load_rom_from_unzipped_bytes(rom_bytes, bios), // not a zip file
        }
    }

    pub fn load_rom_from_file<R: AsRef<Path>, B: AsRef<Path>>(
        rom_path: R,
        bios: Option<B>,
    ) -> Result<Self, LoadError> {
        use std::{
            fs,
            io::{Read, Seek},
        };

        let rom_file = fs::File::open(rom_path)?;
        let mut bytes = Vec::new();
        let mut reader = std::io::BufReader::new(rom_file);
        reader.read_to_end(&mut bytes)?;

        let bios = bios
            .and_then(|bios_path| fs::File::open(bios_path).ok())
            .and_then(|bios_file| {
                let mut bytes = Vec::new();
                let mut reader = std::io::BufReader::new(bios_file);
                if let Ok(_) = reader.read_to_end(&mut bytes) {
                    Some(bytes)
                } else {
                    None
                }
            });

        let res = Cart::from(&bytes);
        match res {
            Ok(game) => Self::new(game, bios),
            Err(e) => {
                reader.rewind()?;
                bytes.clear();

                if let Ok(mut archive) = zip::read::ZipArchive::new(&mut reader) {
                    // it is a zip file
                    let mut zip = archive.by_index(0)?;
                    zip.read_to_end(&mut bytes)?;
                    GbEmulator::load_rom_from_unzipped_bytes(&bytes, bios)
                } else {
                    // not a zip file either
                    Err(e.into())
                }
            }
        }
    }
}

pub fn read_zip_file_from_bytes<B: AsRef<[u8]>>(input: B) -> std::io::Result<Vec<u8>> {
    use std::io::{self, Read};

    let mut reader = std::io::BufReader::new(input.as_ref());
    let unzipped = zip::read::read_zipfile_from_stream(&mut reader)?;

    match unzipped {
        Some(mut zipfile) => {
            let mut buf = Vec::new();
            zipfile.read_to_end(&mut buf)?;
            io::Result::Ok(buf)
        }
        None => {
            let err = io::Error::new(
                io::ErrorKind::IsADirectory,
                "file was not present at root of zip directory",
            );
            io::Result::Err(err)
        }
    }
}
