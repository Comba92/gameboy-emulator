let input = `0x00 => self.nop(),
			0x01 => self.ld(Self::set_bc,Self::immediate16),
			0x02 => self.ld(Self::set_bc_indirect8,Self::a),
			0x03 => self.inc16(Self::set_bc,Self::bc),
			0x04 => self.inc(Self::set_b,Self::b),
			0x05 => self.dec(Self::set_b,Self::b),
			0x06 => self.ld(Self::set_b,Self::immediate8),
			0x07 => self.rlca(),
			0x08 => self.ld(Self::set_indirect16,Self::sp),
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
			0xE0 => self.ld(Self::set_indirect8,Self::a),
			0xE1 => self.pop(Self::set_hl),
			0xE2 => self.ld(Self::set_c_indirect,Self::a),
			0xE5 => self.push(Self::hl),
			0xE6 => self.and(Self::immediate8),
			0xE7 => self.rst(0x20),
			0xE8 => self.addsp(Self::immediate8),
			0xE9 => self.jphl(),
			0xEA => self.ld(Self::set_indirect16,Self::a16),
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
			0xFF => self.rst(0x38),`

let inputCB = `0x00 => self.rlc(Self::set_b,Self::b),
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
			0x40 => self.bit(0,Self::b),
			0x41 => self.bit(0,Self::c),
			0x42 => self.bit(0,Self::d),
			0x43 => self.bit(0,Self::e),
			0x44 => self.bit(0,Self::h),
			0x45 => self.bit(0,Self::l),
			0x46 => self.bit(0,Self::hl_indirect8),
			0x47 => self.bit(0,Self::a),
			0x48 => self.bit(1,Self::b),
			0x49 => self.bit(1,Self::c),
			0x4A => self.bit(1,Self::d),
			0x4B => self.bit(1,Self::e),
			0x4C => self.bit(1,Self::h),
			0x4D => self.bit(1,Self::l),
			0x4E => self.bit(1,Self::hl_indirect8),
			0x4F => self.bit(1,Self::a),
			0x50 => self.bit(2,Self::b),
			0x51 => self.bit(2,Self::c),
			0x52 => self.bit(2,Self::d),
			0x53 => self.bit(2,Self::e),
			0x54 => self.bit(2,Self::h),
			0x55 => self.bit(2,Self::l),
			0x56 => self.bit(2,Self::hl_indirect8),
			0x57 => self.bit(2,Self::a),
			0x58 => self.bit(3,Self::b),
			0x59 => self.bit(3,Self::c),
			0x5A => self.bit(3,Self::d),
			0x5B => self.bit(3,Self::e),
			0x5C => self.bit(3,Self::h),
			0x5D => self.bit(3,Self::l),
			0x5E => self.bit(3,Self::hl_indirect8),
			0x5F => self.bit(3,Self::a),
			0x60 => self.bit(4,Self::b),
			0x61 => self.bit(4,Self::c),
			0x62 => self.bit(4,Self::d),
			0x63 => self.bit(4,Self::e),
			0x64 => self.bit(4,Self::h),
			0x65 => self.bit(4,Self::l),
			0x66 => self.bit(4,Self::hl_indirect8),
			0x67 => self.bit(4,Self::a),
			0x68 => self.bit(5,Self::b),
			0x69 => self.bit(5,Self::c),
			0x6A => self.bit(5,Self::d),
			0x6B => self.bit(5,Self::e),
			0x6C => self.bit(5,Self::h),
			0x6D => self.bit(5,Self::l),
			0x6E => self.bit(5,Self::hl_indirect8),
			0x6F => self.bit(5,Self::a),
			0x70 => self.bit(6,Self::b),
			0x71 => self.bit(6,Self::c),
			0x72 => self.bit(6,Self::d),
			0x73 => self.bit(6,Self::e),
			0x74 => self.bit(6,Self::h),
			0x75 => self.bit(6,Self::l),
			0x76 => self.bit(6,Self::hl_indirect8),
			0x77 => self.bit(6,Self::a),
			0x78 => self.bit(7,Self::b),
			0x79 => self.bit(7,Self::c),
			0x7A => self.bit(7,Self::d),
			0x7B => self.bit(7,Self::e),
			0x7C => self.bit(7,Self::h),
			0x7D => self.bit(7,Self::l),
			0x7E => self.bit(7,Self::hl_indirect8),
			0x7F => self.bit(7,Self::a),
			0x80 => self.res(0,Self::set_b,Self::b),
			0x81 => self.res(0,Self::set_c,Self::c),
			0x82 => self.res(0,Self::set_d,Self::d),
			0x83 => self.res(0,Self::set_e,Self::e),
			0x84 => self.res(0,Self::set_h,Self::h),
			0x85 => self.res(0,Self::set_l,Self::l),
			0x86 => self.res(0,Self::set_hl_indirect8,Self::hl_indirect8),
			0x87 => self.res(0,Self::set_a,Self::a),
			0x88 => self.res(1,Self::set_b,Self::b),
			0x89 => self.res(1,Self::set_c,Self::c),
			0x8A => self.res(1,Self::set_d,Self::d),
			0x8B => self.res(1,Self::set_e,Self::e),
			0x8C => self.res(1,Self::set_h,Self::h),
			0x8D => self.res(1,Self::set_l,Self::l),
			0x8E => self.res(1,Self::set_hl_indirect8,Self::hl_indirect8),
			0x8F => self.res(1,Self::set_a,Self::a),
			0x90 => self.res(2,Self::set_b,Self::b),
			0x91 => self.res(2,Self::set_c,Self::c),
			0x92 => self.res(2,Self::set_d,Self::d),
			0x93 => self.res(2,Self::set_e,Self::e),
			0x94 => self.res(2,Self::set_h,Self::h),
			0x95 => self.res(2,Self::set_l,Self::l),
			0x96 => self.res(2,Self::set_hl_indirect8,Self::hl_indirect8),
			0x97 => self.res(2,Self::set_a,Self::a),
			0x98 => self.res(3,Self::set_b,Self::b),
			0x99 => self.res(3,Self::set_c,Self::c),
			0x9A => self.res(3,Self::set_d,Self::d),
			0x9B => self.res(3,Self::set_e,Self::e),
			0x9C => self.res(3,Self::set_h,Self::h),
			0x9D => self.res(3,Self::set_l,Self::l),
			0x9E => self.res(3,Self::set_hl_indirect8,Self::hl_indirect8),
			0x9F => self.res(3,Self::set_a,Self::a),
			0xA0 => self.res(4,Self::set_b,Self::b),
			0xA1 => self.res(4,Self::set_c,Self::c),
			0xA2 => self.res(4,Self::set_d,Self::d),
			0xA3 => self.res(4,Self::set_e,Self::e),
			0xA4 => self.res(4,Self::set_h,Self::h),
			0xA5 => self.res(4,Self::set_l,Self::l),
			0xA6 => self.res(4,Self::set_hl_indirect8,Self::hl_indirect8),
			0xA7 => self.res(4,Self::set_a,Self::a),
			0xA8 => self.res(5,Self::set_b,Self::b),
			0xA9 => self.res(5,Self::set_c,Self::c),
			0xAA => self.res(5,Self::set_d,Self::d),
			0xAB => self.res(5,Self::set_e,Self::e),
			0xAC => self.res(5,Self::set_h,Self::h),
			0xAD => self.res(5,Self::set_l,Self::l),
			0xAE => self.res(5,Self::set_hl_indirect8,Self::hl_indirect8),
			0xAF => self.res(5,Self::set_a,Self::a),
			0xB0 => self.res(6,Self::set_b,Self::b),
			0xB1 => self.res(6,Self::set_c,Self::c),
			0xB2 => self.res(6,Self::set_d,Self::d),
			0xB3 => self.res(6,Self::set_e,Self::e),
			0xB4 => self.res(6,Self::set_h,Self::h),
			0xB5 => self.res(6,Self::set_l,Self::l),
			0xB6 => self.res(6,Self::set_hl_indirect8,Self::hl_indirect8),
			0xB7 => self.res(6,Self::set_a,Self::a),
			0xB8 => self.res(7,Self::set_b,Self::b),
			0xB9 => self.res(7,Self::set_c,Self::c),
			0xBA => self.res(7,Self::set_d,Self::d),
			0xBB => self.res(7,Self::set_e,Self::e),
			0xBC => self.res(7,Self::set_h,Self::h),
			0xBD => self.res(7,Self::set_l,Self::l),
			0xBE => self.res(7,Self::set_hl_indirect8,Self::hl_indirect8),
			0xBF => self.res(7,Self::set_a,Self::a),
			0xC0 => self.set(0,Self::set_b,Self::b),
			0xC1 => self.set(0,Self::set_c,Self::c),
			0xC2 => self.set(0,Self::set_d,Self::d),
			0xC3 => self.set(0,Self::set_e,Self::e),
			0xC4 => self.set(0,Self::set_h,Self::h),
			0xC5 => self.set(0,Self::set_l,Self::l),
			0xC6 => self.set(0,Self::set_hl_indirect8,Self::hl_indirect8),
			0xC7 => self.set(0,Self::set_a,Self::a),
			0xC8 => self.set(1,Self::set_b,Self::b),
			0xC9 => self.set(1,Self::set_c,Self::c),
			0xCA => self.set(1,Self::set_d,Self::d),
			0xCB => self.set(1,Self::set_e,Self::e),
			0xCC => self.set(1,Self::set_h,Self::h),
			0xCD => self.set(1,Self::set_l,Self::l),
			0xCE => self.set(1,Self::set_hl_indirect8,Self::hl_indirect8),
			0xCF => self.set(1,Self::set_a,Self::a),
			0xD0 => self.set(2,Self::set_b,Self::b),
			0xD1 => self.set(2,Self::set_c,Self::c),
			0xD2 => self.set(2,Self::set_d,Self::d),
			0xD3 => self.set(2,Self::set_e,Self::e),
			0xD4 => self.set(2,Self::set_h,Self::h),
			0xD5 => self.set(2,Self::set_l,Self::l),
			0xD6 => self.set(2,Self::set_hl_indirect8,Self::hl_indirect8),
			0xD7 => self.set(2,Self::set_a,Self::a),
			0xD8 => self.set(3,Self::set_b,Self::b),
			0xD9 => self.set(3,Self::set_c,Self::c),
			0xDA => self.set(3,Self::set_d,Self::d),
			0xDB => self.set(3,Self::set_e,Self::e),
			0xDC => self.set(3,Self::set_h,Self::h),
			0xDD => self.set(3,Self::set_l,Self::l),
			0xDE => self.set(3,Self::set_hl_indirect8,Self::hl_indirect8),
			0xDF => self.set(3,Self::set_a,Self::a),
			0xE0 => self.set(4,Self::set_b,Self::b),
			0xE1 => self.set(4,Self::set_c,Self::c),
			0xE2 => self.set(4,Self::set_d,Self::d),
			0xE3 => self.set(4,Self::set_e,Self::e),
			0xE4 => self.set(4,Self::set_h,Self::h),
			0xE5 => self.set(4,Self::set_l,Self::l),
			0xE6 => self.set(4,Self::set_hl_indirect8,Self::hl_indirect8),
			0xE7 => self.set(4,Self::set_a,Self::a),
			0xE8 => self.set(5,Self::set_b,Self::b),
			0xE9 => self.set(5,Self::set_c,Self::c),
			0xEA => self.set(5,Self::set_d,Self::d),
			0xEB => self.set(5,Self::set_e,Self::e),
			0xEC => self.set(5,Self::set_h,Self::h),
			0xED => self.set(5,Self::set_l,Self::l),
			0xEE => self.set(5,Self::set_hl_indirect8,Self::hl_indirect8),
			0xEF => self.set(5,Self::set_a,Self::a),
			0xF0 => self.set(6,Self::set_b,Self::b),
			0xF1 => self.set(6,Self::set_c,Self::c),
			0xF2 => self.set(6,Self::set_d,Self::d),
			0xF3 => self.set(6,Self::set_e,Self::e),
			0xF4 => self.set(6,Self::set_h,Self::h),
			0xF5 => self.set(6,Self::set_l,Self::l),
			0xF6 => self.set(6,Self::set_hl_indirect8,Self::hl_indirect8),
			0xF7 => self.set(6,Self::set_a,Self::a),
			0xF8 => self.set(7,Self::set_b,Self::b),
			0xF9 => self.set(7,Self::set_c,Self::c),
			0xFA => self.set(7,Self::set_d,Self::d),
			0xFB => self.set(7,Self::set_e,Self::e),
			0xFC => self.set(7,Self::set_h,Self::h),
			0xFD => self.set(7,Self::set_l,Self::l),
			0xFE => self.set(7,Self::set_hl_indirect8,Self::hl_indirect8),
			0xFF => self.set(7,Self::set_a,Self::a),`

import * as fs from 'fs'

let file = JSON.parse(fs.readFileSync('./instr.json').toString())

let all_operands = new Set()

let get_set_operands = [
	{
		kind: "immediate8",
		descr: "Immediate 8bit value",
	},
	{
		kind: "immediate8_signed",
		descr: "Immediate 8bit signed value (be sure to cast it to signed)",
	},
	{
		kind: "immediate16",
		descr: "Immediate 16bit value",
	},
	{
		kind: "hram_address",
		descr: "Get/set 8bit value from memory by using the immediate 8bit value as offset to HRAM (0xff00 + [value])",
	},
	{
		kind: "absolute_address",
		descr: "Get/set 8bit value from memory by using the immediate 16bit value as absolute address",
	},
	{
		kind: "A",
		descr: "A 8bit register",
	},
	{
		kind: "B",
		descr: "B 8bit register",
	},
	{
		kind: "C",
		descr: "C 8bit register",
	},
	{
		kind: "C_hram_address",
		descr: "Get/set 8bit value by using C 8bit register as offset to HRAM (0xff00 + [C])",
	},
	{
		kind: "D",
		descr: "D 8bit register",
	},
	{
		kind: "E",
		descr: "E 8bit register",
	},
	{
		kind: "F",
		descr: "F 8bit status sregister",
	},
	{
		kind: "H",
		descr: "H 8bit register",
	},
	{
		kind: "L",
		descr: "L 8bit register",
	},

	{
		kind: "AF",
		descr: "AF 16bit register",
	},
	{
		kind: "BC",
		descr: "BC 16bit register",
	},
	{
		kind: "BC_absolute_address",
		descr: "Get/set 8bit value from memory by using BC as absolute address",
	},
	{
		kind: "DE",
		descr: "DE 16bit register",
	},
	{
		kind: "DE_absolute_register",
		descr: "Get/set 8bit value from memory by using DE as absolute address",
	},
	{
		kind: "HL",
		descr: "HL 16bit register",
	},
	{
		kind: "HL_absolute_address",
		descr: "Get/set 8bit value from memory by using HL as absolute address",
	},
	{
		kind: "HL_absolute_address_with_inc",
		descr: "Get/set 8bit value from memory by using HL as absolute address, then increase HL by 1",
	},	{
		kind: "HL_absolute_address_with_dec",
		descr: "Get/set 8bit value from memory by using HL as absolute address, then decrease HL by 1",
	},
	{
		kind: "SP",
		descr: "SP 16bit stack pointer register",
	},
	{
		kind: "PC",
		descr: "PC 16bit program counter register",
	},
]

let special_operands = [
	{
		kind: "interrupt_handler_address",
		possible_values: ["0x00", "0x08", "0x10", "0x18", "0x20", "0x28", "0x30", "0x38"],
		descr: "Special operand for RST instruction. PC will be set to one of these address, which point to the interrupt subroutines",
	},
	{
		kind: "branch_condition",
		possible_values: ["zero_flag", "not_zero_flag", "carry_flag", "half_carry_flag"],
		descr: "Special operand for JPCC, JRCC, CALLCC, RETCC instructions. If the condition is true, the branch will be taken",
	},
	{
		kind: "bit_number",
		possible_values: [0, 1, 2, 3, 4, 5, 6, 7],
		descr: "Special operand for BIT, RES, SET instructions. It indicates which bit index the instruction should operate with"
	},
]

/*
	A
	B
	C
	C_Indirect -> C_HRAM
	D
	E
	F
	Carry
	HalfCarry
	Negative
	Zero
	
	SP
	AF
	BC
	DE
	HL

	// Immediate value given as operand
	Immediate8
	Immediate8Signed
	Immediate16

	// Address to byte given as operand
	Indirect8 -> HRAM
	Indirect16 -> Absolute

	BC_Indirect
	DE_Indirect
	HL_Indirect
	HL_IndirectWithInc
	HL_IndirectWithDec

	Set_HRAM
	Set_Absolute

	// Set byte at address in register
	Set_BC_Indirect
	Set_DE_Indirect
	Set_HL_Indirect
	Set_HL_IndirectWithInc
	Set_HL_IndirectWithDec
*/

let descr = {
	"NOP": "This instruction doesn't do anything, but can be used to add a delay of 1 mcycle and increment PC by 1",
	"STOP": "It's complicated. Check online docs for more detail",
	"HALT": "It's complicated. Check online docs for more detail",
	"JP": "Unconditional jump. Load [get] into PC",
	"JPHL": "Unconditional jump. Load HL into PC",
	"JR": "Unconditional jump. Add [get] to PC",
	"RET": "Unconditional jump. Pop the value from the stack, and assign it to PC",
	"RETI": "Immediately sets IME. Pop the value from the stack, and assign it to PC",
	"RST": "Push the current PC (it should point to the instruction after this CALL) to the stack, and load [special] into PC",
	"CALL": "Push the current PC (it should point to the instruction after this CALL) to the stack, and load [get] into PC",
	"JPCC": "Check the condition in the [special] flag. If true, load [get] into PC. If false, do nothing",
	"JRCC": "Check the condition in the [special] flag. If true, add [get] to PC. If false, do nothing",
	"RETCC": "Check the condition in the [special] flag. If true, Pop value from the stack, and assign it to PC",
	"CALLCC": "Check the condition in the [special] flag. If true, push the current PC (it should point to the instruction after this CALL) to the stack, and load [get] into PC",
	"LD": "Load [get] into [set]",
	"LDHL": "Load HL into SP. This load takes one extra cycle",
	"LDSP": "Load into HL the value pointed by SP + [get]. This load takes one extra cycle",
	"ADD": "Add [get] to A register, and store the result in A register",
	"ADDHL": "Add [get] to HL, and store the result in HL. This add takes one extra cycle",
	"ADDSP": "Add to SP the value pointed by SP + [get], and store the result in SP. This add takes two extra cycles",
	"ADC": "Add [get] to A register, set the carry flag based on result, and store the result in A register",
	"SUB": "Subtract [get] to A register, and store the result in A register",
	"SBC": "Subtract [get] to A register, set the carry flag based on result, and store the result in A register",
	"CP": "Compare [get] and A register, by doing A - [get] and setting the status flags. This instruction does NOT store the result into A register, it only sets status flags",
	"AND": "Logical AND the bits of [get] and A register, and store the result in A register",
	"OR": "Logical OR the bits of [get] and A register, and store the result in A register",
	"XOR": "Logical XOR the bits of [get] and A register, and store the result in A register",
	"PUSH": "Subtract 1 from SP. Push the higher byte of [get] into the stack to the address pointed by SP. Then, subtract 1 from SP, and push the lower byte of [get] to the stack",
	"POP": "Pop the value pointed by SP from the stack, set it to the lower byte of [get], and add 1 to SP. Then, pop the next value from the stack, set it to the higher byte of [get], and add 1 to SP",
	"INC": "Increment [get/set] by 1",
	"DEC": "Decrement [get/set] by 1",
	"INC16": "Increment [get/set] by 1. This inc takes one extra cycle",
	"DEC16": "Decrement [get/set] by 1. This dec takes one extra cycle",
	"EI": "Schedules the IME flag settings AFTER the NEXT mcycle",
	"DI": "Clear the IME flag IMMEDIATELY, and cancels any scheduled EI effects",
	"DAA": "Adjust the A register too a binary-coded decimal (BCD) number. Check online docs for more detail",
	"SCF": "Sets flags",
	"CCF": "Sets flags",
	"CPL": "Flips all A register bits",

	"RLCA": "Set carry flag as bit 7 of A register, then rotate left the A register, and set bit 0 of A register to carry flag",
	"RLA": "Rotate left the A register and set bit 0 to carry flag",
	"RRCA": "Set carry flag as bit 0 of A register, then rotate right the A register, and set bit 7 of A register to carry flag",
	"RRA": "Rotate right the A register and set bit 7 to carry flag",
	"RLC": "Set carry flag as bit 7 of [get/set], then rotate left [get/set], and set bit 0 of [get/set] to carry flag",
	"RL": "Rotate left [get/set] and set bit 0 to carry flag",
	"RRC": "Set carry flag as bit 0 of [get/set], then rotate right [get/set], and set bit 7 of [get/set] to carry flag",
	"RR": "Rotate right [get/set] and set bit 7 to carry flag",
	"SLA": "Set carry flag as bit 7 of [get/set], shift left [get/set], and clear bit 0 of [get/set]",
	"SRA": "Set carry flag as bit 0 of [get/set], shift right [get/set]. Bit 7 of [get/set] remains unchanged",
	"SRL": "Set carry flag as bit 0 of [get/set], shift right [get/set], and clear bit 7 of [get/set]",
	"SWAP": "Swap the lower and higher bytes of [get/set]",
	"BIT": "Set zero flag as bit [special] of [get]",
	"RES": "Clear bit [sepcial] of [get/set]",
	"SET": "Set bit [special] of [get/set]",
	"PREFIX": "This is just a prefix for CB opcodes. Another fetch will occur, and the opcode fetched will be treated as a CB opcode",
	"ILLEGAL": "This stalls the CPU, and a reset is required. Shall not be used"
}

let mappings = {
	"a": "A",
	"b": "B",
	"c": "C",
	"d": "D",
	"e": "E",
	"f": "F",
	"h": "H",
	"l": "L",
	"af": "AF",
	"bc": "BC",
	"de": "DE",
	"hl": "HL",
	"sp": "SP",
	"c_indirect": "C_hram_address",
	"carry": "carry_flag",
	"z": "zero_flag",
	"n": "negative_flag",
	"nz": "not_zero_flag",
	"nc": "not_carry_flag",
	"ncarry": "half_carry_flag",
	"indirect8": "hram_address",
	"indirect16": "absolute_address",
	"bc_indirect8": "BC_absolute_address",
	"de_indirect8": "DE_absolute_address",
	"hl_indirect8": "HL_absolute_address",
	"bc_indirect": "BC_absolute_address",
	"de_indirect": "DE_absolute_address",
	"hl_indirect": "HL_absolute_address",
	"hl_inc_indirect8": "HL_absolute_address_with_inc",
	"hl_dec_indirect8": "HL_absolute_address_with_dec",
	"hl_inc_indirect": "HL_absolute_address_with_inc",
	"hl_dec_indirect": "HL_absolute_address_with_dec",
	"indirect_zero8": "hram_address",
	"indirect_abs8": "absolute_address"
}

let instrs = input.split('\n')
  .map(l => l.trim())
  .map(l => {
    return convert_to_obj(l)
  })

let instrsCB = inputCB.split('\n')
	.map(l => l.trim())
	.map(convert_to_obj)

let unprefixed = Object.entries(file.unprefixed)
let cbprefixed = Object.entries(file.cbprefixed)

let illegals = unprefixed
	.filter(([op, i]) => i.mnemonic.startsWith("ILLEGAL"))
	.map(([op, i]) => ({
		name: "ILLEGAL",
		opcode: parseInt(op, 16),
		opcode_hex: op,
		get: 'none', 
		set: 'none',
		special: 'none',
		branch: false,
	}))

console.log(illegals.length)
instrs = instrs.concat(illegals)
instrs.push({
	name: "PREFIX",
	opcode: "0xCB",
	get: 'none',
	set: 'none',
	special: 'none',
	branch: false,
})
instrs.sort((a,b) => a.opcode - b.opcode)
instrsCB.sort((a,b) => a.opcode - b.opcode)

console.log(unprefixed.length, instrs.length)

unprefixed.forEach(([opcode_hex, op]) => merge_json(opcode_hex, op, instrs))
cbprefixed.forEach(([opcode_hex, op]) => merge_json(opcode_hex, op, instrsCB))

let res = {
	operands: get_set_operands, special_operands: special_operands, opcodes: instrs, cb_opcodes: instrsCB,
}

console.log(get_set_operands.map(x => x.kind))
console.log(instrs.filter(x => !x.descr))
console.log(instrsCB.filter(x => x.descr).length)

fs.writeFileSync('sm8_instructions.json', JSON.stringify(res, null, 2))

function merge_json(opcode_hex, op, instrs) {
	let opcode = parseInt(opcode_hex.slice(2), 16)
	if (instrs[opcode] === undefined) return

	instrs[opcode].size_bytes = op.bytes
	instrs[opcode].tcycles = op.cycles[0]
	instrs[opcode].mcycles = op.cycles[0] / 4
	let flags = Object.entries(op.flags)
		.map(([flag, mode]) => {
			// if (mode == "-") {
			// 	mode = "None"
			// } else if (mode == "0") {
			// 	mode = "Clear"
			// } else if (mode == "1") {
			// 	mode = "Set"
			// } else {
			// 	mode = "Compute"
			// }
			return [flag, mode]
		})

	let name = instrs[opcode].name
	instrs[opcode].flags = Object.fromEntries(flags)
	instrs[opcode].descr = descr[name]
	let online_documentation = []

	if (name === "DAA") {
		online_documentation.push(
			'https://blog.ollien.com/posts/gb-daa/',
			'https://ehaskins.com/2018-01-30%20Z80%20DAA/',
		)
	}

	if (name === 'HALT') {
		online_documentation.push(
			'https://gbdev.io/pandocs/halt.html',
			'https://gbdev.io/pandocs/Reducing_Power_Consumption.html#using-the-halt-instruction',
			'https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#halt',
		)
	}

	if (name === 'STOP') {
		online_documentation.push(
			'https://gbdev.io/pandocs/Reducing_Power_Consumption.html#using-the-stop-instruction',
			'https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#nop-and-stop',
		)
	}

	if (["EI", "DI"].includes(name)) {
		online_documentation.push(
			'https://gbdev.io/pandocs/Interrupts.html#ime-interrupt-master-enable-flag-write-only',
			'https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#ei-and-di',
		)
	}

	if ("JPHL" === name) {
		online_documentation.push('https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#jp-hl')
	}

	if ("JR" === name) {
		online_documentation.push('https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#jr-e8')
	}

	if ("ADDSP" === name) {
		online_documentation.push(
			'https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#add-sp-e8',
			'https://stackoverflow.com/questions/5159603/gbz80-how-does-ld-hl-spe-affect-h-and-c-flags'
		)
	}

	if ("LDSP" === name) {
		online_documentation.push(
			'https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#add-hl-sp--e8',
			'https://stackoverflow.com/questions/5159603/gbz80-how-does-ld-hl-spe-affect-h-and-c-flags'
		)
	}

	if ("ADDHL" === name) {
		online_documentation.push('https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#add-hl-r16')
	}

	if (["PUSH", "POP"].includes(name)) {
		online_documentation.push('https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#push-and-pop-discrepancy')
	}

	if ("PREFIX" === name) {
		online_documentation.push('https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#cb-prefix')
	}

	if (["RET", "RETI"].includes(name)) {
		online_documentation.push('https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#ret-and-reti')
	}

	if ("RST" === name) {
		online_documentation.push('https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#rst-nn-opcode')
	}

	if (["ADD", "ADC", "SUB", "SBC"].includes(name)) {
		online_documentation.push('https://gist.github.com/meganesu/9e228b6b587decc783aa9be34ae27841')
	}

	if (["INC16", "DEC16"].includes(name)) {
		online_documentation.push('https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#incdec-r16')
	}

	instrs[opcode].online_documentation = online_documentation


	// let branch = instrs[opcode].branch
	// delete instrs[opcode].branch
	// instrs[opcode].branch = branch
}

function convert_to_obj(l) {
	let data = l.split(' ')

	let opcode_hex = data[0]
	let opcode = parseInt(data[0], 16)
	let instr = data[2]
	let name = instr
		.slice("self.".length, instr.indexOf('('))
		.toUpperCase()

	let operands = l.split('(')[1]
	operands = operands.slice(0, operands.length - 2)
	let set = 'none'
	let get = 'none'
	let special = 'none'

	operands = operands.split(',')
		.map(o => o.replace("Self::", ""))
		.filter(o => o.length > 0)

	operands.forEach(o => {
		if (name === "RST" || name === "RETC") return

		if (o.startsWith("set_")) {
			set = o.slice("set_".length)
		} else {
			get = o
		}
	})

	if (name === "LDHL") {
		set = "sp",
		get = 'hl'
	}
 
	if ("RST" === name) {
		special = operands[0]
	}

	if (["CALLC", "JRC", "JPC", "RETC"].includes(name)) {
		special = operands[0]
		name += "C"
	}

	if (["RES", "SET", "BIT"].includes(name)) {
		special = operands[0]
	}

	if (mappings[get] !== undefined) {
		get = mappings[get]
	}
	if (mappings[set] !== undefined) {
		set = mappings[set]
	}
	if (mappings[special] !== undefined) {
		special = mappings[special]
	}
	if (name === "ADDSP") {
		set = "sp"
	}

	if (name === "LDSP") {
		get = "immediate8_signed"
	}
	
	if (["JR", "ADDSP",].includes(name) && get === "immediate8") {
		get = "immediate8_signed"
	}

	let branch = false
	return {
		name,
		opcode,
		opcode_hex,
		get,
		set,
		special,
		// branch,
	}
}
