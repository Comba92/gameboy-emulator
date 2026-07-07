use crate::emu::GbEmulator;

mod bus;
pub mod cpu;
pub mod emu;
mod ppu;
mod rom;

mod timer {
    pub struct Timer {

    }
}

mod serial {
    use bitfields::bitfield;

    #[bitfield(u8)]
    pub struct Ctrl {
        clock_master: bool,
        clock_speed: bool,
        #[bits(5)]
        _unused: u8,
        transfer_enable: bool,
    }

    pub struct Serial {
        pub data: u8,
        pub ctrl: Ctrl,
        pub out_buffer: Vec<u8>,

        pub clock_count: f32,
        pub sent: u8,
    }
    impl Serial {
        pub fn new() -> Self {
            Self {
                data: 0xff,
                ctrl: Ctrl::new(),
                out_buffer: Vec::new(),
                clock_count: 0.0,
                sent: 0,
            }
        }
    }
}

mod dma {
    pub struct Dma {
        pub init_cycle: bool,
        pub addr: Option<u16>,
    }

    impl Dma {
        pub fn new() -> Self {
            Self {
                init_cycle: false,
                addr: None,
            }
        }

        pub fn write(&mut self, val: u8) {
            self.init_cycle = true;
            self.addr = Some((val as u16) << 8);
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
            let pressed = if self.btns_select {
                (!self.pressed.0) >> 4
            } else if self.dpad_select {
                (!self.pressed.0) & 0x0f
            } else {
                0x0f
            };

            ((!self.btns_select) as u8) << 5 | ((!self.dpad_select) as u8) << 4 | pressed
        }

        pub fn write(&mut self, val: u8) {
            self.dpad_select = val & 0x10 == 0;
            self.btns_select = val & 0x20 == 0;
        }
    }
}

impl GbEmulator {
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

    pub(crate) fn serial_step(&mut self) {
        let clock_target = self.clock_rate() as f32 / 8192.0;
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
            serial.clock_count += 4.0; // 1 mcycle is 4 tcycles
        }
    }

    pub(crate) fn dma_step(&mut self) {
        let dma = &mut self.dma;

        if let Some(addr) = dma.addr {
            if dma.init_cycle {
                dma.init_cycle = false;
                dma.addr = Some(addr);
                return;
            }

            let val = self.dispatch_read(addr);
            let offset = addr & 0x00ff;
            self.dispatch_write(0xfe00 | offset, val);

            if offset == 255 {
                self.dma.addr = None;
            } else {
                self.dma.addr = Some(addr + 1);
            }
        }
    }
}
