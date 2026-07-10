use crate::{bus::Handler, emu::GbEmulator};

mod bus;
pub mod cpu;
pub mod emu;
mod ppu;
mod rom;

mod clock {
    use bitfields::bitfield;

    pub const DMG_CLOCK_RATE: usize = 4194304;
    pub const CBG_CLOCK_RATE: usize = 2 * DMG_CLOCK_RATE;

    #[bitfield(u8)]
    pub struct Speed {
        armed: bool,
        #[bits(6, default = 0x3f)]
        _unused: u8,
        speed: bool,
    }

    pub struct System {
        pub compat_mode: bool,
        pub priority_mode: bool,
        pub clock: Speed,
    }

    impl System {
        pub fn new() -> Self {
            Self {
                compat_mode: true, // we default to compatibility mode
                priority_mode: true,
                clock: Speed::new(),
            }
        }

        pub fn rate(&self) -> usize {
            if self.clock.speed() {
                CBG_CLOCK_RATE
            } else {
                DMG_CLOCK_RATE
            }
        }
    }
}

mod timer {
    use bitfields::{bitfield, bitflag};

    #[bitflag(u8)]
    pub enum Mode {
        #[base]
        M256 = 0,
        M4 = 1,
        M16 = 2,
        M64 = 3,
    }

    #[bitfield(u8)]
    pub struct Ctrl {
        #[bits(2)]
        clock_select: Mode,
        tima_enabled: bool,

        #[bits(5, default = 0x1f)]
        _unused: u8,
    }

    pub struct Timer {
        pub div: u8,
        pub tima: u8,
        pub tma: u8,
        pub tac: Ctrl,

        pub div_counter: u32,
        pub tima_counter: u32,
    }

    impl Timer {
        pub fn new() -> Self {
            Self {
                div: 0,
                tima: 0,
                tma: 0,
                tac: Ctrl::new(),
                div_counter: 0,
                tima_counter: 0,
            }
        }
    }
}

mod serial {
    use bitfields::bitfield;

    #[bitfield(u8)]
    pub struct Ctrl {
        clock_master: bool,
        clock_speed: bool,
        #[bits(5, default = 0x1f)]
        _unused: u8,
        transfer_enable: bool,
    }

    pub struct Serial {
        pub data: u8,
        pub ctrl: Ctrl,
        pub out_buffer: Vec<u8>,

        pub clock_count: u32,
        pub sent: u8,
    }
    impl Serial {
        pub fn new() -> Self {
            Self {
                data: 0xff,
                ctrl: Ctrl::new(),
                out_buffer: Vec::new(),
                clock_count: 0,
                sent: 0,
            }
        }
    }
}

mod dma {
    pub struct OamDma {
        pub addr: Option<u16>,
        pub read: u8,
        pub init_cycle: bool,
    }

    impl OamDma {
        pub fn new() -> Self {
            Self {
                init_cycle: false,
                addr: None,
                read: 0xff,
            }
        }

        pub fn read(&self) -> u8 {
            self.read
        }

        pub fn write(&mut self, val: u8) {
            self.addr = Some((val as u16 % 0xe0) << 8);
            self.read = val % 0xe0;
            self.init_cycle = true;
        }
    }

    pub struct Hdma {
        pub src: u16,
        pub dst: u16,
        pub read: u8,
        pub len: u16,
        pub mode: bool,

        count: Option<u8>,
    }
    impl Hdma {
        pub fn new() -> Self {
            Self {
                src: 0xffff,
                dst: 0xffff,
                read: 0xff,
                len: 0,
                mode: false,
                count: None,
            }
        }

        pub fn read(&self) -> u8 {
            // ((self.mode as u8) << 7) | self.len
            self.read
        }

        pub fn write(&mut self, val: u8) {
            self.read = val;
            self.mode = val & 0x80 != 0;
            self.len = ((val as u16 & 0x7f) + 1) * 16;
            self.count = Some(0);
        }
    }
}

pub mod joypad {
    use bitfields::bitfield;

    #[derive(Clone, Copy)]
    pub enum Input {
        Right = 0,
        Left = 1,
        Up = 2,
        Down = 3,
        A = 4,
        B = 5,
        Select = 6,
        Start = 7,
    }

    #[bitfield(u8)]
    pub(crate) struct Pressed {
        right: bool,
        left: bool,
        up: bool,
        down: bool,
        a: bool,
        b: bool,
        select: bool,
        start: bool,
    }

    pub(crate) struct Joypad {
        pub dpad_select: bool,
        pub btns_select: bool,
        pub pressed: Pressed, // pressed should be inverted when reading
    }
    impl Joypad {
        pub fn new() -> Self {
            Self {
                dpad_select: false,
                btns_select: false,
                pressed: Pressed::new(),
            }
        }

        pub fn read(&self) -> u8 {
            // Note that, rather unconventionally for the Game Boy, a button being pressed is seen as the corresponding bit being 0, not 1.
            let pressed = match (self.btns_select, self.dpad_select) {
                (true, false) => (!self.pressed.0) >> 4,
                (false, true) => (!self.pressed.0) & 0x0f,
                // https://github.com/Ashiepaws/GBEDG/blob/master/bugs/index.md#bomberman-gb
                (false, false) => ((!self.pressed.0) >> 4) | ((!self.pressed.0) & 0x0f), // both selected
                (true, true) => 0xf, // both unselected
            };

            ((!self.btns_select) as u8) << 5 | ((!self.dpad_select) as u8) << 4 | pressed | 0xc0
        }

        pub fn write(&mut self, val: u8) {
            self.dpad_select = val & 0x10 == 0;
            self.btns_select = val & 0x20 == 0;
        }
    }
}

impl GbEmulator {
    pub(crate) fn in_double_speed(&self) -> bool {
        self.sys.clock.speed()
    }

    pub(crate) fn clock_rate(&self) -> usize {
        self.sys.rate()
    }

    pub fn set_button(&mut self, input: joypad::Input, state: bool) {
        let was_down = self.joy.pressed.get_bit(input as u32);

        if self.joy.btns_select && input as u8 <= 3 {
            if !was_down && state {
                self.bus.intf.set_joypad(true);
            }
        } else if self.joy.dpad_select && input as u8 >= 4 {
            if !was_down && state {
                self.bus.intf.set_joypad(true);
            }
        }

        self.joy.pressed.set_bit(input as u32, state);
    }

    pub fn clear_buttons(&mut self) {
        self.joy.pressed = joypad::Pressed::new();
    }

    pub(crate) fn timer_step(&mut self) {
        // TODO: cache clock speed
        let div_clock_target = self.clock_rate() as u32 / 16384;
        let timer = &mut self.timer;

        if timer.div_counter >= div_clock_target {
            timer.div_counter -= div_clock_target;
            timer.div = timer.div.wrapping_add(1);
        }
        timer.div_counter += 4;

        let tima_inc = match timer.tac.clock_select() {
            timer::Mode::M256 => 4096,
            timer::Mode::M4 => 262144,
            timer::Mode::M16 => 65536,
            timer::Mode::M64 => 16384,
        };

        // TODO: cache clock speed
        let tima_clock_target = self.clock_rate() as u32 / tima_inc;
        let timer = &mut self.timer;

        if timer.tima_counter >= tima_clock_target {
            timer.tima_counter -= tima_clock_target;

            if timer.tac.tima_enabled() {
                let (tima, ovfl) = timer.tima.overflowing_add(1);
                self.timer.tima = if ovfl {
                    self.bus.intf.set_timer(true);
                    self.timer.tma
                } else {
                    tima
                };
            }
        }
        self.timer.tima_counter += 4;
    }

    pub(crate) fn serial_step(&mut self) {
        // TODO: cache clock speed...
        let clock_target = if self.is_cgb() {
            let hz = if self.serial.ctrl.clock_speed() {
                262144
            } else {
                8192
            };
            clock::CBG_CLOCK_RATE as u32 / hz
        } else {
            clock::DMG_CLOCK_RATE as u32 / 8192
        };
        let serial = &mut self.serial;

        if serial.ctrl.clock_master() {
            if serial.clock_count >= clock_target {
                serial.clock_count -= clock_target;
                serial.sent += 1;
                serial.data = (serial.data << 1) | 1;

                if serial.sent >= 8 {
                    serial.sent = 0;
                    serial.ctrl.set_transfer_enable(false);
                    self.bus.intf.set_serial(true);
                }
            }
            serial.clock_count += 4; // 1 mcycle is 4 tcycles
        }
    }

    pub(crate) fn oam_dma_step(&mut self) {
        let dma = &mut self.dma;
        // TODO: should block whole bus both for CPU and PPU

        if let Some(addr) = dma.addr {
            if dma.init_cycle {
                dma.init_cycle = false;
                dma.addr = Some(addr);

                // let is_cgb = self.is_cgb();

                // // disable cpu memory bus
                // let bus = &mut self.bus;
                // bus.map.fill(Handler::OpenBus);
                // bus.tmp_map.copy_from_slice(&bus.map);
                // bus.oam_dma_map.copy_from_slice(&bus.map[..0xe]);

                // bus.map[0xe] = Handler::HramOnly;
                // bus.map[0xf] = Handler::HramOnly;

                // if is_cgb {
                //     // On CGB, the cartridge and WRAM are on separate buses. This means that the CPU can access ROM or cartridge SRAM during OAM DMA from WRAM, or WRAM during OAM DMA from ROM or SRAM. However, because a call writes a return address to the stack, and the stack and variables are usually in WRAM, it’s still recommended to busy-wait in HRAM for DMA to finish even on CGB.
                //     if matches!(addr, bus::WRAM0_START..bus::WRAM_END) {
                //         // keep rom and sram enabled
                //         bus.map[..=0xa].copy_from_slice(&bus.tmp_map[..0xa]);
                //         bus.map[0x8] = bus.tmp_map[0x8];
                //     } else if matches!(addr, bus::SRAM_START..bus::SRAM_END) || addr < bus::ROM_END
                //     {
                //         // keep wram enabled
                //         bus.map[0xc..=0xd].copy_from_slice(&bus.tmp_map[0xc..=0xd]);
                //     }
                // }
                return;
            }

            // let val = self.read_with_map(addr, |bus| &bus.oam_dma_map);
            let val = self.dispatch_read(addr);
            self.bus.oam_direct_write(0xfe00 | (addr & 0x00ff), val);

            if addr & 0x00ff == 0x9f {
                // restore cpu memory bus
                // self.bus.map.copy_from_slice(&self.bus.tmp_map);

                self.dma.addr = None;
                self.dma.read = 0xff;
            } else {
                self.dma.addr = Some(addr + 1);
            }
        }
    }

    // pub(crate) fn hdma_step(&mut self) {
    //     let hdma = &mut self.hdma;

    //     if let Some(len) = hdma.len {
    //         if hdma.mode {
    //             // hblank dma
    //         } else {
    //             // general purpose dma
    //             let val = self.dispatch_read(addr);
    //             self.dispatch_write(addr, val);

    //             if len == 1 {
    //                 hdma.len = None;
    //             } else {
    //                 hdma.len = Some(len - 1);
    //             }
    //         }
    //     }
    // }
}
