use crate::{
    bus::{Bus, Mbc},
    clock::Clock,
    cpu::CpuSm83,
    dma::Dma,
    joypad::Joypad,
    ppu::{DMG_PALETTE, Ppu},
    rom::{self, Cart, RomData, is_valid_bios},
    serial::Serial,
    timer::Timer,
};
use std::{io, path::Path};

pub const FRAME_RATE: f32 = 59.73;

pub const SCREEN_WIDTH: isize = 160;
pub const SCREEN_HEIGHT: isize = 144;

pub const FRAMEBUF_SIZE: usize = SCREEN_WIDTH as usize * SCREEN_HEIGHT as usize * 4;
pub const AUDIO_FRAMES_BUFFERED: usize = 8;

pub(crate) type LoadError = Box<dyn std::error::Error>;

pub struct GbEmulator {
    pub cpu: CpuSm83,
    pub(crate) bus: Bus,
    pub(crate) ppu: Ppu,
    pub(crate) mbc: Mbc,
    pub(crate) dma: Dma,
    pub(crate) serial: Serial,
    pub(crate) timer: Timer,
    pub(crate) joy: Joypad,
    pub(crate) clock: Clock,

    pub(crate) output: GbOutput,
}

impl GbEmulator {
    pub fn debug() -> Self {
        Self {
            cpu: CpuSm83::default(),
            bus: Bus::with_ram_64kb(),
            ppu: Ppu::new(),
            mbc: Mbc::None,
            timer: Timer::new(),
            serial: Serial::new(),
            dma: Dma::new(),
            joy: Joypad::new(),
            clock: Clock::new(),
            output: GbOutput::default(),
        }
    }

    pub fn empty() -> Self {
        Self::new(Cart::default(), None).unwrap()
    }

    fn new(game: Cart, bios: Option<Vec<u8>>) -> Result<Self, LoadError> {
        if let Some(bios) = &bios {
            if !is_valid_bios(bios.as_ref()) {
                return Err("invalid GameBoy boot rom".into());
            }
        }

        let cpu = if bios.is_some() {
            CpuSm83::default()
        } else {
            CpuSm83::new_bootless_dmg()
        };

        let mut bus = Bus::new(game, bios);
        let mbc = Mbc::new(&mut bus)?;

        Ok(Self {
            cpu,
            bus,
            mbc,
            ppu: Ppu::new(),
            timer: Timer::new(),
            serial: Serial::new(),
            dma: Dma::new(),
            joy: Joypad::new(),
            clock: Clock::new(),
            output: GbOutput::default(),
        })
    }

    pub fn with_rom<R: AsRef<[u8]>>(rom: R) -> Result<Self, LoadError> {
        Self::builder().build_with_rom(rom)
    }

    pub fn builder<'a>() -> GbBuilder<'a> {
        GbBuilder::default()
    }

    pub fn rom_info(&self) -> &RomData {
        &self.bus.header
    }

    pub fn is_cgb(&self) -> bool {
        self.rom_info().is_cgb()
    }

    pub fn step(&mut self) {
        self.cpu_step();
    }

    pub fn step_until_frame_ready(&mut self) {
        self.output.frame_ready = false;

        while !self.output.frame_ready {
            self.step();
        }
    }

    pub fn get_video_rgba(&self) -> &[u8; FRAMEBUF_SIZE] {
        &self.output.videobuf_view.0
    }

    pub fn get_tileset_rgba(&self, buf: &mut [u8]) {
        for i in 0..384 {
            let x = i % 32;
            let y = i / 32;
            let tile_start = i * 16;
            let tile = &self.bus.vram[tile_start..tile_start + 16];
            let fx = x * 8;
            let fy = y * 8;

            for row in 0..8 {
                let plane0 = tile[row * 2];
                let plane1 = tile[row * 2 + 1];
                for bit in 0..8 {
                    let bit0 = (plane0 >> bit) & 1;
                    let bit1 = ((plane1 >> bit) & 1) << 1;
                    let color_idx = bit1 | bit0;
                    let px = fx + 7 - bit;
                    let py = fy + row;

                    let color = &DMG_PALETTE[color_idx as usize];
                    let idx = (py * 256 as usize + px) * 4;
                    buf[idx + 0] = color.0;
                    buf[idx + 1] = color.1;
                    buf[idx + 2] = color.2;
                    buf[idx + 3] = 255;
                }
            }
        }
    }

    pub fn get_tilemap_rgba(&self, buf: &mut [u8]) {
        for i in 0..32 * 32 {
            let x = i % 32;
            let y = i / 32;
            let tile_id = self.bus.vram[0x1800 | (y * 32 + x)];
            let tile_start = tile_id as usize * 16;
            let tile = &self.bus.vram[tile_start..tile_start + 16];

            let fx = x * 8;
            let fy = y * 8;

            for row in 0..8 {
                let plane0 = tile[row * 2];
                let plane1 = tile[row * 2 + 1];
                for bit in 0..8 {
                    let bit0 = (plane0 >> bit) & 1;
                    let bit1 = ((plane1 >> bit) & 1) << 1;
                    let color_idx = bit1 | bit0;
                    let px = fx + 7 - bit;
                    let py = fy + row;

                    let color = &DMG_PALETTE[color_idx as usize];
                    let idx = (py * 256 as usize + px) * 4;
                    buf[idx + 0] = color.0;
                    buf[idx + 1] = color.1;
                    buf[idx + 2] = color.2;
                    buf[idx + 3] = 255;
                }
            }
        }
    }
}

#[derive(Default)]
pub(crate) struct GbOutput {
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

pub fn read_file_maybe_zipped<P: AsRef<Path>>(path: P) -> io::Result<Vec<u8>> {
    use io::Read;

    let file = std::fs::File::open(path)?;
    let mut reader = std::io::BufReader::new(file);
    let mut bytes = Vec::new();

    match zip::read::ZipArchive::new(&mut reader) {
        Ok(mut archive) => {
            // it is a zip file
            let mut zip = archive.by_index(0)?;
            zip.read_to_end(&mut bytes)?;
            io::Result::Ok(bytes)
        }

        Err(_) => {
            // not a zip file
            use std::io::Seek;

            reader.rewind()?;
            bytes.clear();
            reader.read_to_end(&mut bytes)?;
            io::Result::Ok(bytes)
        }
    }
}

pub fn read_zip_file_from_bytes<B: AsRef<[u8]>>(input: B) -> io::Result<Vec<u8>> {
    let mut reader = io::BufReader::new(input.as_ref());
    let unzipped = zip::read::read_zipfile_from_stream(&mut reader)?;

    match unzipped {
        Some(mut zipfile) => {
            use io::Read;

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

pub fn read_bytes_maybe_zipped<B: AsRef<[u8]>>(input: B) -> Vec<u8> {
    read_zip_file_from_bytes(&input).unwrap_or(input.as_ref().to_owned())
}

enum RomSource<'a> {
    Bytes(&'a [u8]),
    FilePath(&'a Path),
}

#[derive(Default)]
pub struct GbBuilder<'a> {
    rom: Option<RomSource<'a>>,
    bios: Option<RomSource<'a>>,
    skip_bios: bool,
}

impl<'a> GbBuilder<'a> {
    pub fn with_rom<R: 'a + AsRef<[u8]>>(mut self, rom: &'a R) -> Self {
        self.rom = Some(RomSource::Bytes(rom.as_ref()));
        self
    }

    pub fn with_rom_file<P: 'a + AsRef<Path>>(mut self, rom_path: &'a P) -> Self {
        self.rom = Some(RomSource::FilePath(rom_path.as_ref()));
        self
    }

    pub fn with_bios<B: 'a + AsRef<[u8]>>(mut self, bios: Option<&'a B>) -> Self {
        self.bios = bios.map(|bios| RomSource::Bytes(bios.as_ref()));
        self
    }

    pub fn with_bios_file<P: 'a + AsRef<Path>>(mut self, bios_path: Option<&'a P>) -> Self {
        self.bios = bios_path.map(|path| RomSource::FilePath(path.as_ref()));
        self
    }

    pub fn skip_boot(mut self, cond: bool) -> Self {
        self.skip_bios = cond;
        self
    }

    pub fn build_empty(self) -> GbEmulator {
        GbEmulator::empty()
    }

    pub fn build_with_rom<R: 'a + AsRef<[u8]>>(self, rom: R) -> Result<GbEmulator, LoadError> {
        Self::default().with_rom(&rom).build()
    }

    pub fn build(self) -> Result<GbEmulator, LoadError> {
        // games might be zipped!

        let game = if let Some(rom) = self.rom {
            let res = match rom {
                RomSource::Bytes(bytes) => read_zip_file_from_bytes(bytes).map_or_else(
                    |_| Cart::from_bytes(bytes),
                    |unzipped| Cart::from_bytes(&unzipped),
                ),
                RomSource::FilePath(path) => read_file_maybe_zipped(path)
                    .map_err(|e| e.into())
                    .and_then(|res| Cart::from_bytes(&res)),
            };
            res.map_err(|e| format!("error reading ROM: {e}"))?
        } else {
            Cart::default()
        };

        match self.bios {
            Some(bios_src) if !self.skip_bios => {
                let bios = match bios_src {
                    RomSource::Bytes(bytes) => read_zip_file_from_bytes(bytes)
                        .map_or_else(|_| bytes.to_owned(), |unzipped| unzipped),
                    RomSource::FilePath(path) => read_file_maybe_zipped(path)
                        .map_err(|e| format!("error reading BIOS: {e}"))?,
                };

                GbEmulator::new(game, Some(bios))
            }

            _ => GbEmulator::new(game, None),
        }
    }
}
