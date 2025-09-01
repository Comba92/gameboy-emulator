use crate::cpu::CpuSM83;

pub struct Emu {
  pub cpu: CpuSM83,
  pub ram: [u8; 64 * 1024],
}

impl Default for Emu {
  fn default() -> Self {
    Self {
      cpu: CpuSM83::default(),
      ram: [0; 64 * 1024],
    }  
  }
}

impl Emu {
  pub fn emu_step(&mut self) {
    self.cpu_step();
  }
}