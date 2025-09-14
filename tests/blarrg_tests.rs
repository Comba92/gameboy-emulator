use std::{fs::File, io::{BufRead, BufReader}};

use gameboy_emulator::emu::Emu;


#[derive(Default, Debug, PartialEq)]
struct CpuTestState {
  pc: u16,
  sp: u16,
  a: u8,
  f: u8,
  b: u8,
  c: u8,
  d: u8,
  e: u8,
  h: u8,
  l: u8,
}

// A: 01 F: B0 B: 00 C: 13 D: 00 E: D8 H: 01 L: 4D SP: FFFE PC: 00:0100 (00 C3 13 02)
fn parse_log_line(mut line: &str) -> CpuTestState {
  let mut state = CpuTestState::default();

  state.a = u8::from_str_radix(&line[3..5], 16).unwrap();
  line = &line[6..];
  state.f = u8::from_str_radix(&line[3..5], 16).unwrap();
  line = &line[6..];
  state.b = u8::from_str_radix(&line[3..5], 16).unwrap();
  line = &line[6..];
  state.c = u8::from_str_radix(&line[3..5], 16).unwrap();
  line = &line[6..];
  state.d = u8::from_str_radix(&line[3..5], 16).unwrap();
  line = &line[6..];
  state.e = u8::from_str_radix(&line[3..5], 16).unwrap();
  line = &line[6..];
  state.h = u8::from_str_radix(&line[3..5], 16).unwrap();
  line = &line[6..];
  state.l = u8::from_str_radix(&line[3..5], 16).unwrap();
  line = &line[6..];
  state.sp = u16::from_str_radix(&line[4..8], 16).unwrap();
  line = &line[9..];
  state.pc = u16::from_str_radix(&line[7..11], 16).unwrap();

  state
}

fn cpu_to_mock(emu: &Emu) -> CpuTestState {
  let cpu = &emu.cpu;
  
  CpuTestState {
    pc: cpu.pc,
    sp: cpu.sp,
    a: cpu.a,
    f: cpu.f.clone().into_bits(),
    b: cpu.bc.hi(),
    c: cpu.bc.lo(),
    d: cpu.de.hi(),
    e: cpu.de.lo(),
    h: cpu.hl.hi(),
    l: cpu.hl.lo(),
  }
}

#[test]
fn parse_test() {
  let file = File::open("./tests/blarrg/Test1.txt").unwrap();
  let mut reader = BufReader::new(file);
  let mut log  = String::new();
  reader.read_line(&mut log).unwrap();

  let state = parse_log_line(log.lines().next().unwrap());
  println!("{state:x?}");
}

use pretty_assertions::assert_eq;

#[test]
fn test_whole_file() {
  let file = File::open("./tests/blarrg/Test1.txt").unwrap();
  let mut reader = BufReader::new(file);
  let mut log  = String::new();

  let rom = std::fs::read("./roms_tests/gb-test-roms/cpu_instrs/individual/01-special.gb").unwrap();
  let mut emu = Emu::new(rom).unwrap();

  let mut line = 0;
  while let Ok(len) = reader.read_line(&mut log) {
    if len == 0 { break; }

    let log_state = parse_log_line(&log);
    let my_state = cpu_to_mock(&emu);
    
    assert_eq!(log_state, my_state, "Test failed at line {line}: {} \n{:x?}\n{:x?}", log, log_state, my_state);
    
    emu.cpu_step();
    line += 1;
    log.clear();
  }
}