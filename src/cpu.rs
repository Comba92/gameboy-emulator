use crate::emu::{Emu, Interrupt};
use std::ops::{BitAnd, BitOr, BitXor, Not, Shl, Shr};

#[bitfields::bitfield(u16)]
pub struct Register {
  lo: u8,
  hi: u8,
}

#[bitfields::bitfield(u8)]
#[derive(Clone)]
pub struct Flags {
	#[bits(4)]
	_unused: u8,
	carry: bool,
	hcarry: bool,
	neg: bool,
	zero: bool,
}

#[derive(Default, Debug)]
pub struct CpuSM83 {
  pub a: u8,
  pub f: Flags,
  pub bc: Register,
  pub de: Register,
  pub hl: Register,  
  pub sp: u16,
  pub pc: u16,
	pub ime: bool,
	pub ei: bool,

	halted: bool,
  pub mcycles: usize,
	// TODO: used for debug only, consider REMOVING
	pub instr_count: usize,
}
impl CpuSM83 {
	pub fn new() -> Self {
		let mut cpu = Self::default();
		cpu.a = 0x1;
    cpu.pc = 0x0100;
		cpu.sp = 0xfffe;

		// cpu.bc.set_lo(19);
		// cpu.de.set_lo(216);
		// cpu.hl.set_hi(1);
		// cpu.hl.set_lo(77);
		// cpu.f.0 = 176;

		cpu
	}
}

impl Emu {
  fn set_carry(&mut self, val: u16) {
		self.cpu.f.set_carry(val > u8::MAX as u16);
	}

	fn set_carry16(&mut self, val: u32) {
		self.cpu.f.set_carry(val > u16::MAX as u32);
	}

	fn set_hcarry(&mut self, a: u8, b: u8, res: u8) {
		// https://www.reddit.com/r/EmuDev/comments/692n59/comment/dh3bu6t/
		// trick to do it faster
		// naive way is:
		// (a & 0xf) + (b & 0xf) & 0x10 == 0x10
		// should be different for subtraction
		self.cpu.f.set_hcarry((a ^ b ^ res) & 0x10 > 0)
	}

	fn set_hcarry16(&mut self, a: u16, b: u16, res: u16) {
		let res = (a ^ b ^ res) & 0x1000 > 0;
		self.cpu.f.set_hcarry(res);
	}

	fn set_z(&mut self, val: u8) {
		self.cpu.f.set_zero(val == 0);
	}

	pub fn cpu_step(&mut self) {
		self.cpu.instr_count += 1;
		self.handle_interrupts();
		if self.cpu.ei {
			self.cpu.ime = true;
			self.cpu.ei = false;
		}
		if self.cpu.halted {
			self.tick();
			return;
		}

		let opcode = self.pc_fetch();
		self.decode_n_execute(opcode);
	}

	fn handle_interrupts(&mut self) {
		let inte = self.inte.into_bits();
		let intf = self.intf.into_bits();

		if !self.cpu.ime {
			// If IME is not set, there are two distinct cases, depending on whether an interrupt is pending as the halt instruction is first executed.
			// If no interrupt is pending, halt executes as normal, and the CPU resumes regular execution as soon as an interrupt becomes pending. However, since IME=0, the interrupt is not handled.
			if inte & intf & 0x1f > 0 {
				self.cpu.halted = false;
			}

			return;
		}

		for bit in 0..5 {
			let mask = 1 << bit;
			if inte & intf & mask > 0 {
				// Most commonly, IME is set. In this case, the CPU simply wakes up, and before executing the instruction after the halt, the interrupt handler is called normally.
				self.cpu.halted = false;
				self.cpu.ime = false;
				self.intf.set_bits(intf & !mask);

				// we can easily get the ISR (0x40, 048, 0x50, 0x58, 0x60) like this
				let isr = 0x40 | (bit << 3);

				// Two wait states are executed (2 M-cycles pass while nothing happens; presumably the CPU is executing nops during this time).
				self.tick();
				self.tick();

				self.rst(isr);
				break;
			}
		}
	}

  fn read8(&mut self, addr: u16) -> u8 {
		self.tick();
		self.dispatch_read(addr)
	}
	fn read16(&mut self, addr: u16) -> u16 {
		u16::from_le_bytes([self.read8(addr), self.read8(addr.wrapping_add(1))])
	}

	fn write8(&mut self, addr: u16, val: u8) {
		self.tick();
		self.dispatch_write(addr, val);
	}
	fn write16(&mut self, addr: u16, val: u16){
		let [lo, hi] = val.to_le_bytes();
		self.write8(addr, lo);
		self.write8(addr.wrapping_add(1), hi);
	}
	fn pc_fetch(&mut self) -> u8 {
		let res = self.read8(self.cpu.pc);
		self.cpu.pc = self.cpu.pc.wrapping_add(1);
		res
	}
	fn pc_fetch16(&mut self) -> u16 {
		let hi = self.pc_fetch();
		let lo = self.pc_fetch();
		u16::from_le_bytes([hi, lo])
	}

	fn stack_push(&mut self, val: u16) {
		let sp = self.cpu.sp.wrapping_sub(2);
		self.write16(sp, val);
		self.cpu.sp = sp;
	}
	fn stack_pop(&mut self) -> u16 {
		let value = self.read16(self.cpu.sp);
		self.cpu.sp = self.cpu.sp.wrapping_add(2);
		value
	}

	const fn hram(&self, offset: u8) -> u16 {
		0xFF00 | offset as u16
	}

	fn immediate8(&mut self) -> u8 { self.pc_fetch() }
	fn immediate16(&mut self) -> u16 { self.pc_fetch16() }

	fn indirect_zero8(&mut self) -> u8 {
		let offset = self.pc_fetch();
		self.read8(self.hram(offset))
	}
	fn indirect_abs8(&mut self) -> u8 {
		let addr = self.pc_fetch16();
		self.read8(addr)
	}

	fn a(&mut self) -> u8 { self.cpu.a }
	fn b(&mut self) -> u8 { self.cpu.bc.hi() }
	fn c(&mut self) -> u8 { self.cpu.bc.lo() }
	fn c_indirect(&mut self) -> u8 {
		let offset = self.c();
		self.read8(self.hram(offset))
	}

	fn d(&mut self) -> u8 { self.cpu.de.hi() }
	fn e(&mut self) -> u8 { self.cpu.de.lo() }
	fn h(&mut self) -> u8 { self.cpu.hl.hi() }
	fn l(&mut self) -> u8 { self.cpu.hl.lo() }

	fn bc_indirect(&mut self) -> u8 { self.read8(self.cpu.bc.0) }
	fn de_indirect(&mut self) -> u8 { self.read8(self.cpu.de.0) }
	fn hl_indirect8(&mut self) -> u8 { self.read8(self.cpu.hl.0) }
	fn hl_inc_indirect8(&mut self) -> u8 {
		let res = self.hl_indirect8();
		self.cpu.hl.0 = self.cpu.hl.0.wrapping_add(1);
		res
	}
	fn hl_dec_indirect8(&mut self) -> u8 {
		let res = self.hl_indirect8();
		self.cpu.hl.0 = self.cpu.hl.0.wrapping_sub(1);
		res
	}

	fn sp(&mut self) -> u16 { self.cpu.sp }
	fn af(&mut self) -> u16 { ((self.cpu.a as u16) << 8) | self.cpu.f.0 as u16 }
	fn bc(&mut self) -> u16 { self.cpu.bc.0 }
	fn de(&mut self) -> u16 { self.cpu.de.0 }
	fn hl(&mut self) -> u16 { self.cpu.hl.0 }

	fn set_indirect_zero8(&mut self, val: u8) {
		let offset = self.pc_fetch();
		self.write8(self.hram(offset), val);
	}
	fn set_indirect_abs8(&mut self, val: u8) {
		let addr = self.pc_fetch16();
		self.write8(addr, val);
	}
	fn set_indirect_abs16(&mut self, val: u16) {
		let addr = self.pc_fetch16();
		self.write16(addr, val);
	}

	fn set_a(&mut self, val: u8) { self.cpu.a = val; }
	fn set_f(&mut self, val: u8) { self.cpu.f = Flags::from_bits(val & 0xF0) }
	fn set_b(&mut self, val: u8) { self.cpu.bc.set_hi(val) }
	fn set_c(&mut self, val: u8) { self.cpu.bc.set_lo(val) }
	fn set_c_indirect(&mut self, val: u8) {
		let offset = self.c();
		self.write8(self.hram(offset), val);
	}

	fn set_d(&mut self, val: u8) { self.cpu.de.set_hi(val) }
	fn set_e(&mut self, val: u8) { self.cpu.de.set_lo(val) }
	fn set_h(&mut self, val: u8) { self.cpu.hl.set_hi(val) }
	fn set_l(&mut self, val: u8) { self.cpu.hl.set_lo(val) }

	fn set_bc_indirect8(&mut self, val: u8) { self.write8(self.cpu.bc.0, val); }
	fn set_de_indirect8(&mut self, val: u8) { self.write8(self.cpu.de.0, val); }
	fn set_hl_indirect8(&mut self, val: u8) { self.write8(self.cpu.hl.0, val); }
	fn set_hl_inc_indirect8(&mut self, val: u8) {
		self.set_hl_indirect8(val);
		self.cpu.hl.0 = self.cpu.hl.0.wrapping_add(1);
	}
	fn set_hl_dec_indirect8(&mut self, val: u8) {
		self.set_hl_indirect8(val);
		self.cpu.hl.0 = self.cpu.hl.0.wrapping_sub(1);
	}

	fn set_sp(&mut self, val: u16) { self.cpu.sp = val; }
	fn set_af(&mut self, val: u16) {
		self.cpu.a = (val >> 8) as u8;
		self.set_f(val as u8);
	}
	fn set_bc(&mut self, val: u16) { self.cpu.bc.0 = val; }
	fn set_de(&mut self, val: u16) { self.cpu.de.0 = val; }
	fn set_hl(&mut self, val: u16) { self.cpu.hl.0 = val; }

	fn z(&mut self) -> bool { self.cpu.f.zero() }
	fn carry(&mut self) -> bool { self.cpu.f.carry() }
	fn nz(&mut self) -> bool { !self.cpu.f.zero() }
	fn ncarry(&mut self) -> bool { !self.cpu.f.carry() }
}

type OpGet<T> = fn(&mut Emu) -> T;
type OpSet<T> = fn(&mut Emu, T);

impl Emu {
  fn nop(&mut self) {}

	fn ld<T>(&mut self, set: OpSet<T>, get: OpGet<T>) {
		let val = get(self);
		set(self, val);
	}

	fn push(&mut self, get: OpGet<u16>) {
		let val = get(self);
		self.tick();
		self.stack_push(val);
	}

	fn pop(&mut self, set: OpSet<u16>) {
		let val = self.stack_pop();
		set(self, val);
	}

	// 0xf8
	// https://stackoverflow.com/questions/5159603/gbz80-how-does-ld-hl-spe-affect-h-and-c-flags
	fn ldsp(&mut self, set: OpSet<u16>, get: OpGet<u8>) {
		let val = get(self);
		let offset = val as i8;
		let res = self.cpu.sp.wrapping_add_signed(offset as i16);
		
		self.cpu.f.set_zero(false);
		self.cpu.f.set_neg(false);

		// Both of these set carry and half-carry based on the low byte of SP added to the UNSIGNED immediate byte.
		self.set_carry((self.cpu.sp & 0xFF).wrapping_add(val as u16));
		self.set_hcarry(self.cpu.sp as u8, val, res as u8);
		
		set(self, res);
		self.tick();
	}

	fn add(&mut self, get: OpGet<u8>) {
		let val = get(self);
		let res = self.cpu.a as u16 + val as u16;
		
		self.set_z(res as u8);
		self.cpu.f.set_neg(false);
		self.set_hcarry(self.cpu.a, val, res as u8);
		self.set_carry(res);

		self.cpu.a = res as u8;
	}

	// 0xf9
	fn ldhl(&mut self) {
		self.cpu.sp = self.cpu.hl.0;
		self.tick();
	}

	fn adc(&mut self, get: OpGet<u8>) {
		let val = get(self);
		let res = self.cpu.a as u16 
			+ val as u16
			+ self.cpu.f.carry() as u16; 
		
		self.set_z(res as u8);
		self.cpu.f.set_neg(false);
		self.cpu.f.set_hcarry(((self.cpu.a & 0xF).wrapping_add(val & 0xF).wrapping_add(self.cpu.f.carry() as u8)) & 0x10 > 0);
		self.set_carry(res);
		
		self.cpu.a = res as u8;
	}

	fn sub(&mut self, get: OpGet<u8>) {
		let val = get(self);
		let res = (self.cpu.a as u16).wrapping_sub(val as u16);
		
		self.set_z(res as u8);
		self.cpu.f.set_neg(true);
		// pass val as positive and res as subtraction
		self.set_hcarry(self.cpu.a, val, res as u8);
		self.set_carry(res);

		self.cpu.a = res as u8;
	}

	fn sbc(&mut self, get: OpGet<u8>) {
		let val = get(self);
		let res = (self.cpu.a as u16)
			.wrapping_sub(val as u16)
			.wrapping_sub(self.cpu.f.carry() as u16);

		self.set_z(res as u8);
		self.cpu.f.set_neg(true);
		self.cpu.f.set_hcarry(((self.cpu.a & 0xF).wrapping_sub(val & 0xF).wrapping_sub(self.cpu.f.carry() as u8)) & 0x10 > 0);
		self.set_carry(res);
		
		self.cpu.a = res as u8;
	}

	fn cp(&mut self, get: OpGet<u8>) {
		let val = get(self);
		let res = (self.cpu.a as u16).wrapping_sub(val as u16);
		
		self.set_z(res as u8);
		self.cpu.f.set_neg(true);
		self.set_hcarry(self.cpu.a, val, res as u8);
		self.set_carry(res);
	}

	fn inc(&mut self, set: OpSet<u8>, get: OpGet<u8>) {
		let val = get(self);
		let res = val.wrapping_add(1);
		
		self.set_z(res);
		self.cpu.f.set_neg(false);
		self.set_hcarry(val, 1, res);

		set(self, res);
	}

	fn inc16(&mut self, set: OpSet<u16>, get: OpGet<u16>) {
		let val = get(self);
		let res = val.wrapping_add(1);
		set(self, res);
		self.tick();
	}

	fn dec(&mut self, set: OpSet<u8>, get: OpGet<u8>) {
		let val = get(self);
		let res = val.wrapping_sub(1);
		
		self.set_z(res);
		self.cpu.f.set_neg(true);
		self.set_hcarry(val, 1, res);

		set(self, res);
	}

	fn dec16(&mut self, set: OpSet<u16>, get: OpGet<u16>) {
		let val = get(self);
		let res = val.wrapping_sub(1);
		set(self, res);
		self.tick();
	}

	fn logical<F: Fn(u8, u8) -> u8>(&mut self, get: OpGet<u8>, f: F) {
		let val = get(self);
		let res = f(self.cpu.a, val);

		self.set_z(res);
		self.cpu.f.set_neg(false);
		self.cpu.f.set_carry(false);
		self.cpu.a = res;
	}

	fn and(&mut self, get: OpGet<u8>) {
		self.logical(get, u8::bitand);
		self.cpu.f.set_hcarry(true);
	}

	fn or(&mut self, get: OpGet<u8>) {
		self.logical(get, u8::bitor);
		self.cpu.f.set_hcarry(false);
	}

	fn xor(&mut self, get: OpGet<u8>) {
		self.logical(get, u8::bitxor);
		self.cpu.f.set_hcarry(false);
	}

	fn ccf(&mut self) {
		self.cpu.f.set_neg(false);
		self.cpu.f.set_hcarry(false);
		self.cpu.f.set_carry(!self.cpu.f.carry());
	}

	fn scf(&mut self) {
		self.cpu.f.set_neg(false);
		self.cpu.f.set_hcarry(false);
		self.cpu.f.set_carry(true);
	}

	// https://ehaskins.com/2018-01-30%20Z80%20DAA/
	fn daa(&mut self) {
		let mut correction = 0u8;
		let mut carry = false;

		if self.cpu.f.hcarry()
		|| (!self.cpu.f.neg() && self.cpu.a & 0xF > 0x9) {
			correction += 0x6;
		}

		if self.cpu.f.carry()
		|| (!self.cpu.f.neg() && self.cpu.a > 0x99) {
			correction += 0x60;
			carry = true;
		}

		correction = if self.cpu.f.neg() {
			correction.wrapping_neg()
		} else { correction };

		let res = self.cpu.a.wrapping_add(correction);

		self.set_z(res);
		self.cpu.f.set_carry(carry);
		self.cpu.f.set_hcarry(false);

		self.cpu.a = res;
	}

	fn cpl(&mut self) {
		self.cpu.a = self.cpu.a.not();
		self.cpu.f.set_neg(true);
		self.cpu.f.set_hcarry(true);
	}

	fn addhl(&mut self, get: OpGet<u16>) {
		let val = get(self);
		let res = self.cpu.hl.0 as u32 + val as u32;

		self.cpu.f.set_neg(false);
		self.set_carry16(res);
		self.set_hcarry16(self.cpu.hl.0, val, res as u16);

		self.cpu.hl.0 = res as u16;
		self.tick();
	}

	// 0xe8
	// https://stackoverflow.com/questions/5159603/gbz80-how-does-ld-hl-spe-affect-h-and-c-flags
	fn addsp(&mut self, get: OpGet<u8>) {
		let val = get(self);
		let offset = val as i8;
		let res = self.cpu.sp.wrapping_add_signed(offset as i16);
		
		self.cpu.f.set_zero(false);
		self.cpu.f.set_neg(false);

		// Both of these set carry and half-carry based on the low byte of SP added to the UNSIGNED immediate byte.
		self.set_carry((self.cpu.sp & 0xFF).wrapping_add(val as u16));
		self.set_hcarry(self.cpu.sp as u8, val, res as u8);
		
		self.tick();
		self.tick();
		self.cpu.sp = res as u16;
	}

	fn shift_acc<FS: Fn(u8) -> u8>(&mut self, f: FS, carry_mask: u8) {
		self.shift(Self::set_a, Self::a, f, carry_mask);
		self.cpu.f.set_zero(false);
	}

	fn rlca(&mut self) {
		self.shift_acc(|val| val.rotate_left(1), 0x80);
	}

	fn rrca(&mut self) {
		self.shift_acc(|val| val.rotate_right(1), 1);
	}

	fn rla(&mut self) {
		let carry = self.cpu.f.carry() as u8;
		self.shift_acc(|val| val.shl(1) | carry, 0x80);
	}

	fn rra(&mut self) {
		let carry = self.cpu.f.carry() as u8;
		self.shift_acc(|val| (carry << 7) | val.shr(1), 1);
	}


	fn shift<F: Fn(u8) -> u8>(&mut self, set: OpSet<u8>, get: OpGet<u8>, f: F, carry_mask: u8) {
		let val = get(self);
		let res = f(val);

		self.set_z(res);
		self.cpu.f.set_neg(false);
		self.cpu.f.set_carry(val & carry_mask > 0);
		self.cpu.f.set_hcarry(false);

		set(self, res);
	}

	fn rlc(&mut self, set: OpSet<u8>, get: OpGet<u8>) {
		self.shift(set, get, |val| val.rotate_left(1), 0x80);
	}

	fn rrc(&mut self, set: OpSet<u8>, get: OpGet<u8>) {
		self.shift(set, get, |val| val.rotate_right(1), 1);
	}

	fn rl(&mut self, set: OpSet<u8>, get: OpGet<u8>) {
		let carry = self.cpu.f.carry() as u8;
		self.shift(set, get, |val| val.shl(1) | carry, 0x80);
	}

	fn rr(&mut self, set: OpSet<u8>, get: OpGet<u8>) {
		let carry = self.cpu.f.carry() as u8;
		self.shift(set, get, |val| (carry << 7) | val.shr(1), 1);
	}

	fn sla(&mut self, set: OpSet<u8>, get: OpGet<u8>) {
		self.shift(set, get, |val| val.shl(1), 0x80);
	}

	fn sra(&mut self, set: OpSet<u8>, get: OpGet<u8>) {
		self.shift(set, get, |val| (val & 0x80) | val.shr(1), 1);
	}

	fn srl(&mut self, set: OpSet<u8>, get: OpGet<u8>) {
		self.shift(set, get, |val| val.shr(1), 1);
	}

	fn swap(&mut self, set: OpSet<u8>, get: OpGet<u8>) {
		let val = get(self);
		let low = val & 0x0f;
		let high = val & 0xf0;
		let res = (low << 4) | (high >> 4);

		self.set_z(res);
		self.cpu.f.set_neg(false);
		self.cpu.f.set_hcarry(false);
		self.cpu.f.set_carry(false);

		set(self, res);
	}

	fn bit(&mut self, bit_mask: u8, get: OpGet<u8>) {
		let val = get(self);
		let res = val & bit_mask;

		self.set_z(res);
		self.cpu.f.set_neg(false);
		self.cpu.f.set_hcarry(true);
	}

	fn res(&mut self, bit_mask: u8, set: OpSet<u8>, get: OpGet<u8>) {
		let val = get(self);
		let res = val & !bit_mask;
		set(self, res);
	}

	fn set(&mut self, bit_mask: u8, set: OpSet<u8>, get: OpGet<u8>) {
		let val = get(self);
		let res = val | bit_mask;
		set(self, res);
	}

	fn jp(&mut self, get: OpGet<u16>) {
		let addr = get(self);
		self.cpu.pc = addr;
		self.tick();
	}

	// 0xe9
	fn jphl(&mut self) {
		self.cpu.pc = self.cpu.hl.0;
	}

	fn jpc(&mut self, cond: OpGet<bool>, get: OpGet<u16>) {
		let addr = get(self);
		if cond(self) {
			self.cpu.pc = addr;
			self.tick();
		}
	}

	fn jr(&mut self, get: OpGet<u8>) {
		let offset = get(self) as i8;
		self.cpu.pc = self.cpu.pc.wrapping_add_signed(offset as i16);
		self.tick();
	}

	fn jrc(&mut self, cond: OpGet<bool>, get: OpGet<u8>) {
		let offset = get(self) as i8;
		if cond(self) {
			self.cpu.pc = self.cpu.pc.wrapping_add_signed(offset as i16);
			self.tick();
		}
	}

	fn call(&mut self, get: OpGet<u16>) {
		let addr = get(self);
		self.tick();
		self.stack_push(self.cpu.pc);
		self.cpu.pc = addr;
	}

	fn callc(&mut self, cond: OpGet<bool>, get: OpGet<u16>) {
		let addr = get(self);

		if cond(self) {
			self.tick();
			self.stack_push(self.cpu.pc);
			self.cpu.pc = addr;
		}
	}

	fn ret(&mut self) {
		self.cpu.pc = self.stack_pop();
		self.tick();
	}

	fn retc(&mut self, cond: OpGet<bool>,) {
		self.tick();
		if cond(self) {
			self.cpu.pc = self.stack_pop();
			self.tick();
		}
	}

	fn reti(&mut self) {
		self.cpu.ime = true;
		self.cpu.pc = self.stack_pop();
		self.tick();
	}

	fn rst(&mut self, int: u16) {
		self.tick();
		self.stack_push(self.cpu.pc);
		self.cpu.pc = int;
	}

	fn di(&mut self) { self.cpu.ime = false; self.cpu.ei = false; }
	fn ei(&mut self) { self.cpu.ei = true; }

	fn stop(&mut self, _get: OpGet<u8>) {
		// TODO
	}

	fn halt(&mut self) {
		let inte = self.inte.into_bits();
		let intf = self.intf.into_bits();

		// If IME is not set, there are two distinct cases, depending on whether an interrupt is pending as the halt instruction is first executed.
		if !self.cpu.ime && (inte & intf & 0x1f) > 0 {
			// If an interrupt is pending, halt immediately exits, as expected, however the “halt bug” is triggered.
			// https://gbdev.io/pandocs/halt.html#halt-bug

			// read without increasing pc
			let opcode = self.read8(self.cpu.pc);
			self.decode_n_execute(opcode);
		} else {
			self.cpu.halted = true;
		}
	}
}


impl Emu {
  fn decode_n_execute(&mut self, opcode: u8) {
		match opcode {
			0x00 => self.nop(),
			0x01 => self.ld(Self::set_bc,Self::immediate16),
			0x02 => self.ld(Self::set_bc_indirect8,Self::a),
			0x03 => self.inc16(Self::set_bc,Self::bc),
			0x04 => self.inc(Self::set_b,Self::b),
			0x05 => self.dec(Self::set_b,Self::b),
			0x06 => self.ld(Self::set_b,Self::immediate8),
			0x07 => self.rlca(),
			0x08 => self.ld(Self::set_indirect_abs16,Self::sp),
			0x09 => self.addhl(Self::bc),
			0x0A => self.ld(Self::set_a,Self::bc_indirect),
			0x0B => self.dec16(Self::set_bc,Self::bc),
			0x0C => self.inc(Self::set_c,Self::c),
			0x0D => self.dec(Self::set_c,Self::c),
			0x0E => self.ld(Self::set_c,Self::immediate8),
			0x0F => self.rrca(),
			0x10 => self.stop(Self::immediate8),
			0x11 => self.ld(Self::set_de,Self::immediate16),
			0x12 => self.ld(Self::set_de_indirect8,Self::a),
			0x13 => self.inc16(Self::set_de,Self::de),
			0x14 => self.inc(Self::set_d,Self::d),
			0x15 => self.dec(Self::set_d,Self::d),
			0x16 => self.ld(Self::set_d,Self::immediate8),
			0x17 => self.rla(),
			0x18 => self.jr(Self::immediate8),
			0x19 => self.addhl(Self::de),
			0x1A => self.ld(Self::set_a,Self::de_indirect),
			0x1B => self.dec16(Self::set_de,Self::de),
			0x1C => self.inc(Self::set_e,Self::e),
			0x1D => self.dec(Self::set_e,Self::e),
			0x1E => self.ld(Self::set_e,Self::immediate8),
			0x1F => self.rra(),
			0x20 => self.jrc(Self::nz,Self::immediate8),
			0x21 => self.ld(Self::set_hl,Self::immediate16),
			0x22 => self.ld(Self::set_hl_inc_indirect8,Self::a),
			0x23 => self.inc16(Self::set_hl,Self::hl),
			0x24 => self.inc(Self::set_h,Self::h),
			0x25 => self.dec(Self::set_h,Self::h),
			0x26 => self.ld(Self::set_h,Self::immediate8),
			0x27 => self.daa(),
			0x28 => self.jrc(Self::z,Self::immediate8),
			0x29 => self.addhl(Self::hl),
			0x2A => self.ld(Self::set_a,Self::hl_inc_indirect8),
			0x2B => self.dec16(Self::set_hl,Self::hl),
			0x2C => self.inc(Self::set_l,Self::l),
			0x2D => self.dec(Self::set_l,Self::l),
			0x2E => self.ld(Self::set_l,Self::immediate8),
			0x2F => self.cpl(),
			0x30 => self.jrc(Self::ncarry,Self::immediate8),
			0x31 => self.ld(Self::set_sp,Self::immediate16),
			0x32 => self.ld(Self::set_hl_dec_indirect8,Self::a),
			0x33 => self.inc16(Self::set_sp,Self::sp),
			0x34 => self.inc(Self::set_hl_indirect8,Self::hl_indirect8),
			0x35 => self.dec(Self::set_hl_indirect8,Self::hl_indirect8),
			0x36 => self.ld(Self::set_hl_indirect8,Self::immediate8),
			0x37 => self.scf(),
			0x38 => self.jrc(Self::carry,Self::immediate8),
			0x39 => self.addhl(Self::sp),
			0x3A => self.ld(Self::set_a,Self::hl_dec_indirect8),
			0x3B => self.dec16(Self::set_sp,Self::sp),
			0x3C => self.inc(Self::set_a,Self::a),
			0x3D => self.dec(Self::set_a,Self::a),
			0x3E => self.ld(Self::set_a,Self::immediate8),
			0x3F => self.ccf(),
			0x40 => self.ld(Self::set_b,Self::b),
			0x41 => self.ld(Self::set_b,Self::c),
			0x42 => self.ld(Self::set_b,Self::d),
			0x43 => self.ld(Self::set_b,Self::e),
			0x44 => self.ld(Self::set_b,Self::h),
			0x45 => self.ld(Self::set_b,Self::l),
			0x46 => self.ld(Self::set_b,Self::hl_indirect8),
			0x47 => self.ld(Self::set_b,Self::a),
			0x48 => self.ld(Self::set_c,Self::b),
			0x49 => self.ld(Self::set_c,Self::c),
			0x4A => self.ld(Self::set_c,Self::d),
			0x4B => self.ld(Self::set_c,Self::e),
			0x4C => self.ld(Self::set_c,Self::h),
			0x4D => self.ld(Self::set_c,Self::l),
			0x4E => self.ld(Self::set_c,Self::hl_indirect8),
			0x4F => self.ld(Self::set_c,Self::a),
			0x50 => self.ld(Self::set_d,Self::b),
			0x51 => self.ld(Self::set_d,Self::c),
			0x52 => self.ld(Self::set_d,Self::d),
			0x53 => self.ld(Self::set_d,Self::e),
			0x54 => self.ld(Self::set_d,Self::h),
			0x55 => self.ld(Self::set_d,Self::l),
			0x56 => self.ld(Self::set_d,Self::hl_indirect8),
			0x57 => self.ld(Self::set_d,Self::a),
			0x58 => self.ld(Self::set_e,Self::b),
			0x59 => self.ld(Self::set_e,Self::c),
			0x5A => self.ld(Self::set_e,Self::d),
			0x5B => self.ld(Self::set_e,Self::e),
			0x5C => self.ld(Self::set_e,Self::h),
			0x5D => self.ld(Self::set_e,Self::l),
			0x5E => self.ld(Self::set_e,Self::hl_indirect8),
			0x5F => self.ld(Self::set_e,Self::a),
			0x60 => self.ld(Self::set_h,Self::b),
			0x61 => self.ld(Self::set_h,Self::c),
			0x62 => self.ld(Self::set_h,Self::d),
			0x63 => self.ld(Self::set_h,Self::e),
			0x64 => self.ld(Self::set_h,Self::h),
			0x65 => self.ld(Self::set_h,Self::l),
			0x66 => self.ld(Self::set_h,Self::hl_indirect8),
			0x67 => self.ld(Self::set_h,Self::a),
			0x68 => self.ld(Self::set_l,Self::b),
			0x69 => self.ld(Self::set_l,Self::c),
			0x6A => self.ld(Self::set_l,Self::d),
			0x6B => self.ld(Self::set_l,Self::e),
			0x6C => self.ld(Self::set_l,Self::h),
			0x6D => self.ld(Self::set_l,Self::l),
			0x6E => self.ld(Self::set_l,Self::hl_indirect8),
			0x6F => self.ld(Self::set_l,Self::a),
			0x70 => self.ld(Self::set_hl_indirect8,Self::b),
			0x71 => self.ld(Self::set_hl_indirect8,Self::c),
			0x72 => self.ld(Self::set_hl_indirect8,Self::d),
			0x73 => self.ld(Self::set_hl_indirect8,Self::e),
			0x74 => self.ld(Self::set_hl_indirect8,Self::h),
			0x75 => self.ld(Self::set_hl_indirect8,Self::l),
			0x76 => self.halt(),
			0x77 => self.ld(Self::set_hl_indirect8,Self::a),
			0x78 => self.ld(Self::set_a,Self::b),
			0x79 => self.ld(Self::set_a,Self::c),
			0x7A => self.ld(Self::set_a,Self::d),
			0x7B => self.ld(Self::set_a,Self::e),
			0x7C => self.ld(Self::set_a,Self::h),
			0x7D => self.ld(Self::set_a,Self::l),
			0x7E => self.ld(Self::set_a,Self::hl_indirect8),
			0x7F => self.ld(Self::set_a,Self::a),
			0x80 => self.add(Self::b),
			0x81 => self.add(Self::c),
			0x82 => self.add(Self::d),
			0x83 => self.add(Self::e),
			0x84 => self.add(Self::h),
			0x85 => self.add(Self::l),
			0x86 => self.add(Self::hl_indirect8),
			0x87 => self.add(Self::a),
			0x88 => self.adc(Self::b),
			0x89 => self.adc(Self::c),
			0x8A => self.adc(Self::d),
			0x8B => self.adc(Self::e),
			0x8C => self.adc(Self::h),
			0x8D => self.adc(Self::l),
			0x8E => self.adc(Self::hl_indirect8),
			0x8F => self.adc(Self::a),
			0x90 => self.sub(Self::b),
			0x91 => self.sub(Self::c),
			0x92 => self.sub(Self::d),
			0x93 => self.sub(Self::e),
			0x94 => self.sub(Self::h),
			0x95 => self.sub(Self::l),
			0x96 => self.sub(Self::hl_indirect8),
			0x97 => self.sub(Self::a),
			0x98 => self.sbc(Self::b),
			0x99 => self.sbc(Self::c),
			0x9A => self.sbc(Self::d),
			0x9B => self.sbc(Self::e),
			0x9C => self.sbc(Self::h),
			0x9D => self.sbc(Self::l),
			0x9E => self.sbc(Self::hl_indirect8),
			0x9F => self.sbc(Self::a),
			0xA0 => self.and(Self::b),
			0xA1 => self.and(Self::c),
			0xA2 => self.and(Self::d),
			0xA3 => self.and(Self::e),
			0xA4 => self.and(Self::h),
			0xA5 => self.and(Self::l),
			0xA6 => self.and(Self::hl_indirect8),
			0xA7 => self.and(Self::a),
			0xA8 => self.xor(Self::b),
			0xA9 => self.xor(Self::c),
			0xAA => self.xor(Self::d),
			0xAB => self.xor(Self::e),
			0xAC => self.xor(Self::h),
			0xAD => self.xor(Self::l),
			0xAE => self.xor(Self::hl_indirect8),
			0xAF => self.xor(Self::a),
			0xB0 => self.or(Self::b),
			0xB1 => self.or(Self::c),
			0xB2 => self.or(Self::d),
			0xB3 => self.or(Self::e),
			0xB4 => self.or(Self::h),
			0xB5 => self.or(Self::l),
			0xB6 => self.or(Self::hl_indirect8),
			0xB7 => self.or(Self::a),
			0xB8 => self.cp(Self::b),
			0xB9 => self.cp(Self::c),
			0xBA => self.cp(Self::d),
			0xBB => self.cp(Self::e),
			0xBC => self.cp(Self::h),
			0xBD => self.cp(Self::l),
			0xBE => self.cp(Self::hl_indirect8),
			0xBF => self.cp(Self::a),
			0xC0 => self.retc(Self::nz),
			0xC1 => self.pop(Self::set_bc),
			0xC2 => self.jpc(Self::nz,Self::immediate16),
			0xC3 => self.jp(Self::immediate16),
			0xC4 => self.callc(Self::nz,Self::immediate16),
			0xC5 => self.push(Self::bc),
			0xC6 => self.add(Self::immediate8),
			0xC7 => self.rst(0x00),
			0xC8 => self.retc(Self::z),
			0xC9 => self.ret(),
			0xCA => self.jpc(Self::z,Self::immediate16),
			0xCB => self.decode_n_execute_cb(),
			0xCC => self.callc(Self::z,Self::immediate16),
			0xCD => self.call(Self::immediate16),
			0xCE => self.adc(Self::immediate8),
			0xCF => self.rst(0x08),
			0xD0 => self.retc(Self::ncarry),
			0xD1 => self.pop(Self::set_de),
			0xD2 => self.jpc(Self::ncarry,Self::immediate16),
			0xD4 => self.callc(Self::ncarry,Self::immediate16),
			0xD5 => self.push(Self::de),
			0xD6 => self.sub(Self::immediate8),
			0xD7 => self.rst(0x10),
			0xD8 => self.retc(Self::carry),
			0xD9 => self.reti(),
			0xDA => self.jpc(Self::carry,Self::immediate16),
			0xDC => self.callc(Self::carry,Self::immediate16),
			0xDE => self.sbc(Self::immediate8),
			0xDF => self.rst(0x18),
			0xE0 => self.ld(Self::set_indirect_zero8,Self::a),
			0xE1 => self.pop(Self::set_hl),
			0xE2 => self.ld(Self::set_c_indirect,Self::a),
			0xE5 => self.push(Self::hl),
			0xE6 => self.and(Self::immediate8),
			0xE7 => self.rst(0x20),
			0xE8 => self.addsp(Self::immediate8),
			0xE9 => self.jphl(),
			0xEA => self.ld(Self::set_indirect_abs8,Self::a),
			0xEE => self.xor(Self::immediate8),
			0xEF => self.rst(0x28),
			0xF0 => self.ld(Self::set_a,Self::indirect_zero8),
			0xF1 => self.pop(Self::set_af),
			0xF2 => self.ld(Self::set_a,Self::c_indirect),
			0xF3 => self.di(),
			0xF5 => self.push(Self::af),
			0xF6 => self.or(Self::immediate8),
			0xF7 => self.rst(0x30),
			0xF8 => self.ldsp(Self::set_hl, Self::immediate8),
			0xF9 => self.ldhl(),
			0xFA => self.ld(Self::set_a,Self::indirect_abs8),
			0xFB => self.ei(),
			0xFE => self.cp(Self::immediate8),
			0xFF => self.rst(0x38),
			_ => panic!("===[SYSTEM JAM]=== illegal opcode {:02X} reached", opcode)
    }
  }

	fn decode_n_execute_cb(&mut self) {
		let opcode = self.pc_fetch();

		match opcode {
			0x00 => self.rlc(Self::set_b,Self::b),
			0x01 => self.rlc(Self::set_c,Self::c),
			0x02 => self.rlc(Self::set_d,Self::d),
			0x03 => self.rlc(Self::set_e,Self::e),
			0x04 => self.rlc(Self::set_h,Self::h),
			0x05 => self.rlc(Self::set_l,Self::l),
			0x06 => self.rlc(Self::set_hl_indirect8,Self::hl_indirect8),
			0x07 => self.rlc(Self::set_a,Self::a),
			0x08 => self.rrc(Self::set_b,Self::b),
			0x09 => self.rrc(Self::set_c,Self::c),
			0x0A => self.rrc(Self::set_d,Self::d),
			0x0B => self.rrc(Self::set_e,Self::e),
			0x0C => self.rrc(Self::set_h,Self::h),
			0x0D => self.rrc(Self::set_l,Self::l),
			0x0E => self.rrc(Self::set_hl_indirect8,Self::hl_indirect8),
			0x0F => self.rrc(Self::set_a,Self::a),
			0x10 => self.rl(Self::set_b,Self::b),
			0x11 => self.rl(Self::set_c,Self::c),
			0x12 => self.rl(Self::set_d,Self::d),
			0x13 => self.rl(Self::set_e,Self::e),
			0x14 => self.rl(Self::set_h,Self::h),
			0x15 => self.rl(Self::set_l,Self::l),
			0x16 => self.rl(Self::set_hl_indirect8,Self::hl_indirect8),
			0x17 => self.rl(Self::set_a,Self::a),
			0x18 => self.rr(Self::set_b,Self::b),
			0x19 => self.rr(Self::set_c,Self::c),
			0x1A => self.rr(Self::set_d,Self::d),
			0x1B => self.rr(Self::set_e,Self::e),
			0x1C => self.rr(Self::set_h,Self::h),
			0x1D => self.rr(Self::set_l,Self::l),
			0x1E => self.rr(Self::set_hl_indirect8,Self::hl_indirect8),
			0x1F => self.rr(Self::set_a,Self::a),
			0x20 => self.sla(Self::set_b,Self::b),
			0x21 => self.sla(Self::set_c,Self::c),
			0x22 => self.sla(Self::set_d,Self::d),
			0x23 => self.sla(Self::set_e,Self::e),
			0x24 => self.sla(Self::set_h,Self::h),
			0x25 => self.sla(Self::set_l,Self::l),
			0x26 => self.sla(Self::set_hl_indirect8,Self::hl_indirect8),
			0x27 => self.sla(Self::set_a,Self::a),
			0x28 => self.sra(Self::set_b,Self::b),
			0x29 => self.sra(Self::set_c,Self::c),
			0x2A => self.sra(Self::set_d,Self::d),
			0x2B => self.sra(Self::set_e,Self::e),
			0x2C => self.sra(Self::set_h,Self::h),
			0x2D => self.sra(Self::set_l,Self::l),
			0x2E => self.sra(Self::set_hl_indirect8,Self::hl_indirect8),
			0x2F => self.sra(Self::set_a,Self::a),
			0x30 => self.swap(Self::set_b,Self::b),
			0x31 => self.swap(Self::set_c,Self::c),
			0x32 => self.swap(Self::set_d,Self::d),
			0x33 => self.swap(Self::set_e,Self::e),
			0x34 => self.swap(Self::set_h,Self::h),
			0x35 => self.swap(Self::set_l,Self::l),
			0x36 => self.swap(Self::set_hl_indirect8,Self::hl_indirect8),
			0x37 => self.swap(Self::set_a,Self::a),
			0x38 => self.srl(Self::set_b,Self::b),
			0x39 => self.srl(Self::set_c,Self::c),
			0x3A => self.srl(Self::set_d,Self::d),
			0x3B => self.srl(Self::set_e,Self::e),
			0x3C => self.srl(Self::set_h,Self::h),
			0x3D => self.srl(Self::set_l,Self::l),
			0x3E => self.srl(Self::set_hl_indirect8,Self::hl_indirect8),
			0x3F => self.srl(Self::set_a,Self::a),
			0x40 => self.bit(0x01,Self::b),
			0x41 => self.bit(0x01,Self::c),
			0x42 => self.bit(0x01,Self::d),
			0x43 => self.bit(0x01,Self::e),
			0x44 => self.bit(0x01,Self::h),
			0x45 => self.bit(0x01,Self::l),
			0x46 => self.bit(0x01,Self::hl_indirect8),
			0x47 => self.bit(0x01,Self::a),
			0x48 => self.bit(0x02,Self::b),
			0x49 => self.bit(0x02,Self::c),
			0x4A => self.bit(0x02,Self::d),
			0x4B => self.bit(0x02,Self::e),
			0x4C => self.bit(0x02,Self::h),
			0x4D => self.bit(0x02,Self::l),
			0x4E => self.bit(0x02,Self::hl_indirect8),
			0x4F => self.bit(0x02,Self::a),
			0x50 => self.bit(0x04,Self::b),
			0x51 => self.bit(0x04,Self::c),
			0x52 => self.bit(0x04,Self::d),
			0x53 => self.bit(0x04,Self::e),
			0x54 => self.bit(0x04,Self::h),
			0x55 => self.bit(0x04,Self::l),
			0x56 => self.bit(0x04,Self::hl_indirect8),
			0x57 => self.bit(0x04,Self::a),
			0x58 => self.bit(0x08,Self::b),
			0x59 => self.bit(0x08,Self::c),
			0x5A => self.bit(0x08,Self::d),
			0x5B => self.bit(0x08,Self::e),
			0x5C => self.bit(0x08,Self::h),
			0x5D => self.bit(0x08,Self::l),
			0x5E => self.bit(0x08,Self::hl_indirect8),
			0x5F => self.bit(0x08,Self::a),
			0x60 => self.bit(0x10,Self::b),
			0x61 => self.bit(0x10,Self::c),
			0x62 => self.bit(0x10,Self::d),
			0x63 => self.bit(0x10,Self::e),
			0x64 => self.bit(0x10,Self::h),
			0x65 => self.bit(0x10,Self::l),
			0x66 => self.bit(0x10,Self::hl_indirect8),
			0x67 => self.bit(0x10,Self::a),
			0x68 => self.bit(0x20,Self::b),
			0x69 => self.bit(0x20,Self::c),
			0x6A => self.bit(0x20,Self::d),
			0x6B => self.bit(0x20,Self::e),
			0x6C => self.bit(0x20,Self::h),
			0x6D => self.bit(0x20,Self::l),
			0x6E => self.bit(0x20,Self::hl_indirect8),
			0x6F => self.bit(0x20,Self::a),
			0x70 => self.bit(0x40,Self::b),
			0x71 => self.bit(0x40,Self::c),
			0x72 => self.bit(0x40,Self::d),
			0x73 => self.bit(0x40,Self::e),
			0x74 => self.bit(0x40,Self::h),
			0x75 => self.bit(0x40,Self::l),
			0x76 => self.bit(0x40,Self::hl_indirect8),
			0x77 => self.bit(0x40,Self::a),
			0x78 => self.bit(0x80,Self::b),
			0x79 => self.bit(0x80,Self::c),
			0x7A => self.bit(0x80,Self::d),
			0x7B => self.bit(0x80,Self::e),
			0x7C => self.bit(0x80,Self::h),
			0x7D => self.bit(0x80,Self::l),
			0x7E => self.bit(0x80,Self::hl_indirect8),
			0x7F => self.bit(0x80,Self::a),
			0x80 => self.res(0x01,Self::set_b,Self::b),
			0x81 => self.res(0x01,Self::set_c,Self::c),
			0x82 => self.res(0x01,Self::set_d,Self::d),
			0x83 => self.res(0x01,Self::set_e,Self::e),
			0x84 => self.res(0x01,Self::set_h,Self::h),
			0x85 => self.res(0x01,Self::set_l,Self::l),
			0x86 => self.res(0x01,Self::set_hl_indirect8,Self::hl_indirect8),
			0x87 => self.res(0x01,Self::set_a,Self::a),
			0x88 => self.res(0x02,Self::set_b,Self::b),
			0x89 => self.res(0x02,Self::set_c,Self::c),
			0x8A => self.res(0x02,Self::set_d,Self::d),
			0x8B => self.res(0x02,Self::set_e,Self::e),
			0x8C => self.res(0x02,Self::set_h,Self::h),
			0x8D => self.res(0x02,Self::set_l,Self::l),
			0x8E => self.res(0x02,Self::set_hl_indirect8,Self::hl_indirect8),
			0x8F => self.res(0x02,Self::set_a,Self::a),
			0x90 => self.res(0x04,Self::set_b,Self::b),
			0x91 => self.res(0x04,Self::set_c,Self::c),
			0x92 => self.res(0x04,Self::set_d,Self::d),
			0x93 => self.res(0x04,Self::set_e,Self::e),
			0x94 => self.res(0x04,Self::set_h,Self::h),
			0x95 => self.res(0x04,Self::set_l,Self::l),
			0x96 => self.res(0x04,Self::set_hl_indirect8,Self::hl_indirect8),
			0x97 => self.res(0x04,Self::set_a,Self::a),
			0x98 => self.res(0x08,Self::set_b,Self::b),
			0x99 => self.res(0x08,Self::set_c,Self::c),
			0x9A => self.res(0x08,Self::set_d,Self::d),
			0x9B => self.res(0x08,Self::set_e,Self::e),
			0x9C => self.res(0x08,Self::set_h,Self::h),
			0x9D => self.res(0x08,Self::set_l,Self::l),
			0x9E => self.res(0x08,Self::set_hl_indirect8,Self::hl_indirect8),
			0x9F => self.res(0x08,Self::set_a,Self::a),
			0xA0 => self.res(0x10,Self::set_b,Self::b),
			0xA1 => self.res(0x10,Self::set_c,Self::c),
			0xA2 => self.res(0x10,Self::set_d,Self::d),
			0xA3 => self.res(0x10,Self::set_e,Self::e),
			0xA4 => self.res(0x10,Self::set_h,Self::h),
			0xA5 => self.res(0x10,Self::set_l,Self::l),
			0xA6 => self.res(0x10,Self::set_hl_indirect8,Self::hl_indirect8),
			0xA7 => self.res(0x10,Self::set_a,Self::a),
			0xA8 => self.res(0x20,Self::set_b,Self::b),
			0xA9 => self.res(0x20,Self::set_c,Self::c),
			0xAA => self.res(0x20,Self::set_d,Self::d),
			0xAB => self.res(0x20,Self::set_e,Self::e),
			0xAC => self.res(0x20,Self::set_h,Self::h),
			0xAD => self.res(0x20,Self::set_l,Self::l),
			0xAE => self.res(0x20,Self::set_hl_indirect8,Self::hl_indirect8),
			0xAF => self.res(0x20,Self::set_a,Self::a),
			0xB0 => self.res(0x40,Self::set_b,Self::b),
			0xB1 => self.res(0x40,Self::set_c,Self::c),
			0xB2 => self.res(0x40,Self::set_d,Self::d),
			0xB3 => self.res(0x40,Self::set_e,Self::e),
			0xB4 => self.res(0x40,Self::set_h,Self::h),
			0xB5 => self.res(0x40,Self::set_l,Self::l),
			0xB6 => self.res(0x40,Self::set_hl_indirect8,Self::hl_indirect8),
			0xB7 => self.res(0x40,Self::set_a,Self::a),
			0xB8 => self.res(0x80,Self::set_b,Self::b),
			0xB9 => self.res(0x80,Self::set_c,Self::c),
			0xBA => self.res(0x80,Self::set_d,Self::d),
			0xBB => self.res(0x80,Self::set_e,Self::e),
			0xBC => self.res(0x80,Self::set_h,Self::h),
			0xBD => self.res(0x80,Self::set_l,Self::l),
			0xBE => self.res(0x80,Self::set_hl_indirect8,Self::hl_indirect8),
			0xBF => self.res(0x80,Self::set_a,Self::a),
			0xC0 => self.set(0x01,Self::set_b,Self::b),
			0xC1 => self.set(0x01,Self::set_c,Self::c),
			0xC2 => self.set(0x01,Self::set_d,Self::d),
			0xC3 => self.set(0x01,Self::set_e,Self::e),
			0xC4 => self.set(0x01,Self::set_h,Self::h),
			0xC5 => self.set(0x01,Self::set_l,Self::l),
			0xC6 => self.set(0x01,Self::set_hl_indirect8,Self::hl_indirect8),
			0xC7 => self.set(0x01,Self::set_a,Self::a),
			0xC8 => self.set(0x02,Self::set_b,Self::b),
			0xC9 => self.set(0x02,Self::set_c,Self::c),
			0xCA => self.set(0x02,Self::set_d,Self::d),
			0xCB => self.set(0x02,Self::set_e,Self::e),
			0xCC => self.set(0x02,Self::set_h,Self::h),
			0xCD => self.set(0x02,Self::set_l,Self::l),
			0xCE => self.set(0x02,Self::set_hl_indirect8,Self::hl_indirect8),
			0xCF => self.set(0x02,Self::set_a,Self::a),
			0xD0 => self.set(0x04,Self::set_b,Self::b),
			0xD1 => self.set(0x04,Self::set_c,Self::c),
			0xD2 => self.set(0x04,Self::set_d,Self::d),
			0xD3 => self.set(0x04,Self::set_e,Self::e),
			0xD4 => self.set(0x04,Self::set_h,Self::h),
			0xD5 => self.set(0x04,Self::set_l,Self::l),
			0xD6 => self.set(0x04,Self::set_hl_indirect8,Self::hl_indirect8),
			0xD7 => self.set(0x04,Self::set_a,Self::a),
			0xD8 => self.set(0x08,Self::set_b,Self::b),
			0xD9 => self.set(0x08,Self::set_c,Self::c),
			0xDA => self.set(0x08,Self::set_d,Self::d),
			0xDB => self.set(0x08,Self::set_e,Self::e),
			0xDC => self.set(0x08,Self::set_h,Self::h),
			0xDD => self.set(0x08,Self::set_l,Self::l),
			0xDE => self.set(0x08,Self::set_hl_indirect8,Self::hl_indirect8),
			0xDF => self.set(0x08,Self::set_a,Self::a),
			0xE0 => self.set(0x10,Self::set_b,Self::b),
			0xE1 => self.set(0x10,Self::set_c,Self::c),
			0xE2 => self.set(0x10,Self::set_d,Self::d),
			0xE3 => self.set(0x10,Self::set_e,Self::e),
			0xE4 => self.set(0x10,Self::set_h,Self::h),
			0xE5 => self.set(0x10,Self::set_l,Self::l),
			0xE6 => self.set(0x10,Self::set_hl_indirect8,Self::hl_indirect8),
			0xE7 => self.set(0x10,Self::set_a,Self::a),
			0xE8 => self.set(0x20,Self::set_b,Self::b),
			0xE9 => self.set(0x20,Self::set_c,Self::c),
			0xEA => self.set(0x20,Self::set_d,Self::d),
			0xEB => self.set(0x20,Self::set_e,Self::e),
			0xEC => self.set(0x20,Self::set_h,Self::h),
			0xED => self.set(0x20,Self::set_l,Self::l),
			0xEE => self.set(0x20,Self::set_hl_indirect8,Self::hl_indirect8),
			0xEF => self.set(0x20,Self::set_a,Self::a),
			0xF0 => self.set(0x40,Self::set_b,Self::b),
			0xF1 => self.set(0x40,Self::set_c,Self::c),
			0xF2 => self.set(0x40,Self::set_d,Self::d),
			0xF3 => self.set(0x40,Self::set_e,Self::e),
			0xF4 => self.set(0x40,Self::set_h,Self::h),
			0xF5 => self.set(0x40,Self::set_l,Self::l),
			0xF6 => self.set(0x40,Self::set_hl_indirect8,Self::hl_indirect8),
			0xF7 => self.set(0x40,Self::set_a,Self::a),
			0xF8 => self.set(0x80,Self::set_b,Self::b),
			0xF9 => self.set(0x80,Self::set_c,Self::c),
			0xFA => self.set(0x80,Self::set_d,Self::d),
			0xFB => self.set(0x80,Self::set_e,Self::e),
			0xFC => self.set(0x80,Self::set_h,Self::h),
			0xFD => self.set(0x80,Self::set_l,Self::l),
			0xFE => self.set(0x80,Self::set_hl_indirect8,Self::hl_indirect8),
			0xFF => self.set(0x80,Self::set_a,Self::a),
		}
	}
}