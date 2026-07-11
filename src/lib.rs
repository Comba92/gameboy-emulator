use crate::{bus::Handler, emu::GbEmulator};

mod apu;
mod bus;
pub mod cpu;
pub mod emu;
mod ppu;
mod rom;

mod sys {
    use crate::emu;
    use bitfields::bitfield;

    #[bitfield(u8)]
    pub struct Speed {
        armed: bool,
        #[bits(6, default = 0x3f)]
        _unused: u8,
        speed: bool,
    }

    pub struct System {
        pub is_cgb_model: bool,
        pub is_cgb_game: bool,

        pub compat_mode: bool,
        pub priority_mode: bool,
        pub clock: Speed,
    }

    impl System {
        pub fn new(is_cgb_model: bool) -> Self {
            Self {
                is_cgb_model,
                is_cgb_game: is_cgb_model, // if cgb model, boot rom has to access the cgb registers.
                compat_mode: !is_cgb_model,
                priority_mode: !is_cgb_model,
                clock: Speed::new(),
            }
        }

        pub fn rate(&self) -> usize {
            if self.clock.speed() {
                emu::CBG_CLOCK_RATE
            } else {
                emu::DMG_CLOCK_RATE
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
    }

    impl Timer {
        pub fn new() -> Self {
            Self {
                div: 0,
                tima: 0,
                tma: 0,
                tac: Ctrl::new(),
            }
        }
    }
}

impl GbEmulator {
    pub(crate) fn timer_step(&mut self) {
        // 4194304Hz / 16384Hz = 256 Tcycles = 256 / 4 = 64 Mccyles
        // Same in Double speed: 8388608Hz / 32768Hz = 256 Tcycles = 64 Mcycles
        // Div is in reality a 16bit counter, incremented every Mcycle.
        // Only the upper part is visibile, hence why it increases every 256 Tcycles!
        let cycles = self.cpu.mcycles;

        let timer = &mut self.timer;
        if cycles % 64 == 0 {
            timer.div = timer.div.wrapping_add(1);
        }

        let tima_inc = match timer.tac.clock_select() {
            timer::Mode::M256 => 256,
            timer::Mode::M4 => 4,
            timer::Mode::M16 => 16,
            timer::Mode::M64 => 64,
        };

        if cycles % tima_inc == 0 {
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

        pub sent: u8,
    }
    impl Serial {
        pub fn new() -> Self {
            Self {
                data: 0xff,
                ctrl: Ctrl::new(),
                out_buffer: Vec::new(),
                sent: 0,
            }
        }
    }
}

impl GbEmulator {
    pub(crate) fn serial_step(&mut self) {
        let cycles = self.cpu.mcycles;
        let serial = &mut self.serial;

        if serial.ctrl.clock_master() {
            // 4194304Hz / 8192Hz = 512 Tcycles = 512 / 4 = 128 Mccyles
            // Same in Double speed: 8388608Hz / 16384Hz = 512 Tcycles = 128 Mcycles
            // or
            // 4194304Hz / 262144Hz = 16 Tcycles = 16 / 4 = 4 Mccyles

            let freq = if serial.ctrl.clock_speed() { 4 } else { 128 };

            if cycles % freq == 0 {
                serial.sent += 1;
                serial.data = (serial.data << 1) | 1;

                if serial.sent >= 8 {
                    serial.sent = 0;
                    serial.ctrl.set_transfer_enable(false);
                    self.bus.intf.set_serial(true);
                }
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

impl GbEmulator {
    pub(crate) fn oam_dma_step(&mut self) {
        let dma = &mut self.dma;

        if let Some(addr) = dma.addr {
            if dma.init_cycle {
                dma.init_cycle = false;
                dma.addr = Some(addr);

                // let is_cgb = self.is_cgb();

                // // disable cpu memory bus
                // let bus = &mut self.bus;
                // bus.tmp_map.copy_from_slice(&bus.map);
                // bus.oam_dma_map[..0xe].copy_from_slice(&bus.map[..0xe]);

                // bus.map.fill(Handler::OpenBus);
                // bus.map[0xe] = Handler::HramOnly;
                // bus.map[0xf] = Handler::HramOnly;

                // if is_cgb {
                //     // On CGB, the cartridge and WRAM are on separate buses. This means that the CPU can access ROM or cartridge SRAM during OAM DMA from WRAM, or WRAM during OAM DMA from ROM or SRAM. However, because a call writes a return address to the stack, and the stack and variables are usually in WRAM, it’s still recommended to busy-wait in HRAM for DMA to finish even on CGB.
                //     if matches!(addr, bus::WRAM0_START..bus::WRAM_END) {
                //         // keep rom and sram enabled
                //         bus.map[..0x8].copy_from_slice(&bus.tmp_map[..0x8]);
                //         bus.map[0xa] = bus.tmp_map[0xa];
                //         bus.map[0xb] = bus.tmp_map[0xb];
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
        let mut new_pressed = self.joy.pressed.clone();
        new_pressed.set_bit(input as u32, state);

        if (new_pressed.left() && new_pressed.right()) || (new_pressed.up() && new_pressed.down()) {
            // illegal input
            return;
        }

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
}

mod utils {
    use crate::emu;

    #[derive(Default, Debug)]
    pub struct RingBuffer<T> {
        pub(crate) data: Box<[T]>,
        read_pos: usize,
        write_pos: usize,
        queued: usize,
    }
    impl<T: Default + Clone> RingBuffer<T> {
        pub fn new(size: usize) -> Self {
            Self::new_with(size, Default::default())
        }
    }

    impl<T: Clone> RingBuffer<T> {
        pub fn new_with(size: usize, default: T) -> Self {
            Self {
                data: vec![default; size].into_boxed_slice(),
                read_pos: 0,
                write_pos: 0,
                queued: 0,
            }
        }
    }

    impl<T> RingBuffer<T> {
        pub fn clear(&mut self) {
            self.read_pos = 0;
            self.write_pos = 0;
        }

        pub fn read_pos(&self) -> usize {
            self.read_pos
        }

        pub fn write_pos(&self) -> usize {
            self.write_pos
        }

        pub fn push(&mut self, val: T) {
            self.data[self.write_pos] = val;
            self.write_pos = (self.write_pos + 1) % self.data.len();
            self.queued = (self.queued + 1).min(self.data.len());
        }

        pub fn pop(&mut self) -> &T {
            self.pop_mut()
        }

        pub fn pop_mut(&mut self) -> &mut T {
            let head = self.read_pos;
            self.read_pos = (self.read_pos + 1) % self.data.len();
            self.queued = self.queued.saturating_sub(1);
            let res = &mut self.data[head];
            res
        }

        pub fn capacity(&self) -> usize {
            self.data.len()
        }

        pub fn is_queued_all_contiguos(&self) -> bool {
            // tail is right of head, consecutive data
            self.write_pos >= self.read_pos
        }

        pub fn queued(&self) -> usize {
            if self.is_queued_all_contiguos() {
                // tail is right of head, consecutive
                self.write_pos - self.read_pos
            } else {
                // tail is left of head, not consecutive
                self.write_pos + self.queued_contiguos()
            }
            // self.queued
        }

        pub fn queued_contiguos(&self) -> usize {
            self.data.len() - self.read_pos
        }

        pub fn available_contiguos(&self) -> usize {
            self.data.len() - self.write_pos
        }

        pub fn available(&self) -> usize {
            self.data.len() - self.queued()
        }

        pub fn take(&mut self, amount: usize) -> (&[T], Option<&[T]>) {
            let amount = amount.min(self.queued());

            let right_amount = amount.min(self.queued_contiguos());
            let right = &self.data[self.read_pos..self.read_pos + right_amount];

            let left = if right_amount < amount {
                let left_amount = amount - right_amount;
                Some(&self.data[..left_amount])
            } else {
                None
            };

            self.read_pos = (self.read_pos + amount) % self.data.len();
            self.queued = self.queued.saturating_sub(amount);

            (right, left)
        }

        pub fn take_iter(&mut self, amount: usize) -> impl Iterator<Item = &T> {
            let (right, left) = self.take(amount);
            right.iter().chain(left.unwrap_or_default().iter())
        }
    }

    pub struct AvgResampler {
        left_sample_avg: f32,
        right_sample_avg: f32,
        samples_count: usize,

        sample_timer: f64,
        input_rate: f64,
        output_rate: f64,
        cycles_per_sample: f64,
    }
    impl Default for AvgResampler {
        fn default() -> Self {
            Self::new(emu::DMG_CLOCK_RATE as f64, 48000.0)
        }
    }

    impl AvgResampler {
        pub fn new(input_rate: f64, output_rate: f64) -> Self {
            Self {
                left_sample_avg: 0.0,
                right_sample_avg: 0.0,
                samples_count: 0,

                sample_timer: 0.0,
                input_rate,
                output_rate,
                cycles_per_sample: input_rate / output_rate,
            }
        }

        pub fn clear(&self) -> Self {
            Self {
                left_sample_avg: 0.0,
                right_sample_avg: 0.0,
                samples_count: 0,

                sample_timer: 0.0,
                input_rate: self.input_rate,
                output_rate: self.output_rate,
                cycles_per_sample: self.cycles_per_sample,
            }
        }

        pub fn set_rate(&mut self, input_rate: f64, output_rate: f64) {
            self.input_rate = input_rate;
            self.output_rate = output_rate;
            self.cycles_per_sample = input_rate / output_rate;
        }

        pub fn add_sample(&mut self, left: f32, right: f32) -> Option<(f32, f32)> {
            self.left_sample_avg += left;
            self.right_sample_avg += right;
            self.samples_count += 1;

            self.sample_timer += 1.0;

            if self.sample_timer >= self.cycles_per_sample {
                self.sample_timer -= self.cycles_per_sample;
                let left = self.left_sample_avg / self.samples_count as f32;
                let right = self.right_sample_avg / self.samples_count as f32;

                self.left_sample_avg = 0.0;
                self.right_sample_avg = 0.0;
                self.samples_count = 0;

                Some((left, right))
            } else {
                None
            }
        }
    }
}
