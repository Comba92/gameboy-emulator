#[derive (Debug, serde::Serialize, serde::Deserialize)]
struct CpuTest<'a> {
  name: &'a str,
  #[serde(rename = "initial")]
  start: CpuTestState,
  #[serde(rename = "final")]
  end: CpuTestState,
  cycles: Vec<(usize, Option<usize>, &'a str)>
}

#[derive (Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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
  ime: u8,
  ei: Option<u8>,
  ram: Vec<(u16, u8)>,
}

use std::{fs, io::{self, Read}};

use gameboy_emulator::{emu, cpu};

fn cpu_to_mock(emu: &mut emu::Emu, mock: &CpuTestState) -> CpuTestState {
  let cpu = &emu.cpu;
  
  let mut test = CpuTestState {
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
    ime: cpu.ime as u8,
    ei: cpu.ei.then_some(1),
    ram: Vec::new(),
  };

  for (addr, _) in &mock.ram {
    test.ram.push((*addr, emu.ram[*addr as usize]));
  }

  test
}

fn cpu_from_mock(emu: &mut emu::Emu, mock: &CpuTestState) {
  let cpu = &mut emu.cpu;
  
  cpu.pc = mock.pc;
  cpu.sp = mock.sp;
  cpu.ime = mock.ime > 0;
  cpu.ei = mock.ei.unwrap_or_default() > 0;
  cpu.a = mock.a;
  cpu.f = cpu::Flags::from_bits(mock.f);
  cpu.bc.set_hi(mock.b);
  cpu.bc.set_lo(mock.c);
  cpu.de.set_hi(mock.d);
  cpu.de.set_lo(mock.e);
  cpu.hl.set_hi(mock.h);
  cpu.hl.set_lo(mock.l);

  for (addr, val) in &mock.ram {
    emu.ram[*addr as usize] = *val;
  }
}

#[test]
fn parse_test() {
  let res: Vec<CpuTest> = serde_json
    ::from_str(include_str!("./sm83/v1/00.json"))
    .unwrap();

  println!("{:?}", res[0]);
}

fn cpu_test(test: &CpuTest) -> bool {
  let mut emu = emu::Emu::default();
  cpu_from_mock(&mut emu, &test.start);
  
  while emu.cpu.mcycles < test.cycles.len() {
    emu.emu_step();
  }
  
  use pretty_assertions::assert_eq;
  let res = cpu_to_mock(&mut emu, &test.end);
  assert_eq!(res, test.end, "{}", test.name);
  res == test.end
}

#[test]
fn exec_test() {
  let test: Vec<CpuTest> = serde_json
    ::from_str(include_str!("./sm83/v1/a9.json"))
    .unwrap();

  let mut emu = emu::Emu::default();
  println!("{:?}", emu.cpu);

  cpu_from_mock(&mut emu, &test[0].start);

  println!("{:?}", test[0]);

  while emu.cpu.mcycles < test[0].cycles.len() {
    emu.emu_step();
  }

  let res = cpu_to_mock(&mut emu, &test[0].end);

  assert_eq!(res, test[0].end);
}

#[test]
fn exec_all_tests() {
  let files = fs::read_dir("./tests/sm83/v1/").expect("tests folder missing");
  let mut file_str = String::new();

  for file in files {
    let entry = file.unwrap();

    let name = entry.file_name().into_string().unwrap();
    if name.starts_with("cb") || name.starts_with("10") || name.starts_with("76") { continue; }

    println!("{:?}", entry.file_name());
    let file = fs::File::open(entry.path()).unwrap();
    let mut file_buf = io::BufReader::new(file);
    file_buf.read_to_string(&mut file_str).unwrap();
    
    let tests: Vec<CpuTest> = serde_json::from_str(&file_str).unwrap(); 
    for test in tests {
      cpu_test(&test);
    }

    file_str.clear();
  }
}
