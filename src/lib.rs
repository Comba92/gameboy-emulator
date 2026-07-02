use crate::emu::GbEmulator;

mod bus;
pub mod cpu;
pub mod emu;
mod ppu;
mod rom;

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

mod joypad {
    use bitfields::bitfield;

    #[bitfield(u8)]
    pub struct JoypadInput {
        right: bool,
        left: bool,
        up: bool,
        down: bool,
        a: bool,
        b: bool,
        select: bool,
        start: bool,
    }

    pub struct Joypad {
        dpad_select: bool,
        btns_select: bool,
        pressed: JoypadInput,
    }
    impl Joypad {
        pub fn new() -> Self {
            Self {
                dpad_select: false,
                btns_select: false,
                pressed: JoypadInput::from(0xff),
            }
        }

        pub fn read(&self) -> u8 {
            (self.btns_select as u8) << 5 | (self.dpad_select as u8) << 4 | 0xf
        }

        pub fn write(&mut self, val: u8) {
            self.dpad_select = val & 0x10 > 0;
            self.btns_select = val & 0x20 > 0;
        }
    }
}

impl GbEmulator {
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
}
