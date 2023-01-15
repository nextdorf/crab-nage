
use bitflags::bitflags;


#[repr(C)]
#[allow(non_snake_case)]
pub struct Register {
  /// Flagregister
  // pub F: u8,
  pub F: FlagRegister,
  /// Accumalator
  pub A: u8,
  pub C: u8,
  pub B: u8,
  pub E: u8,
  pub D: u8,
  pub L: u8,
  pub H: u8,
  /// Stack pointer, points to the current stack position
  pub SP: u16,
  /// Programm Counter, points to the next instruction to be executed in the Gameboy memory
  pub PC: u16,
}

bitflags! {
  #[repr(C)]
  pub struct FlagRegister: u8 {
    /// Zero Flag, This bit is set when the result of a math operation is zero or two values match when using the CP instruction
    const Z = 0b1000_0000;
    /// Subtract Flag, This bit is set if a subtraction was performed in the last math instruction
    const N = 0b0100_0000;
    /// Half Carry Flag, This bit is set if a carry occured from the lower nibble in the last math operation
    const H = 0b0010_0000;
    /// Carry Flag, This bit is set if a carry occured from the last math operation or if register A is the smaller value when executing the CP instruction
    const C = 0b0001_0000;
  }
}

#[allow(non_snake_case)]
impl Register {
  pub fn AF(&self) -> u16 {
    unsafe {*(&self.F.bits as *const u8 as *const u16)}
  }
  pub fn AF_mut(&mut self) -> &mut u16 {
    unsafe {&mut *(&mut self.F.bits as *mut u8 as *mut u16)}
  }
  
  pub fn BC(&self) -> u16 {
    unsafe {*(&self.C as *const u8 as *const u16)}
  }
  pub fn BC_mut(&mut self) -> &mut u16 {
    unsafe {&mut *(&mut self.C as *mut u8 as *mut u16)}
  }
  
  pub fn DE(&self) -> u16 {
    unsafe {*(&self.E as *const u8 as *const u16)}
  }
  pub fn DE_mut(&mut self) -> &mut u16 {
    unsafe {&mut *(&mut self.E as *mut u8 as *mut u16)}
  }
  
  pub fn HL(&self) -> u16 {
    unsafe {*(&self.L as *const u8 as *const u16)}
  }
  pub fn HL_mut(&mut self) -> &mut u16 {
    unsafe {&mut *(&mut self.L as *mut u8 as *mut u16)}
  }

  pub fn init_SP(&mut self) { self.SP = 0xFF_FE }
  pub fn init_PC(&mut self) { self.PC = 0x100 }
}

impl FlagRegister {
  pub fn add8(&mut self, a: u8, b: u8) -> u8 {
    let res = a.wrapping_add(b);
    self.set(FlagRegister::Z, res == 0);
    self.set(FlagRegister::N, false);
    self.set(FlagRegister::C, Self::add_and_carry(a as _, b as _, 8).1);
    self.set(FlagRegister::H, Self::add_and_carry(a as _, b as _, 4).1);
    res
  }

  pub fn sub8(&mut self, a: u8, b: u8) -> u8 {
    let res = a.wrapping_sub(b);
    self.set(FlagRegister::Z, res == 0);
    self.set(FlagRegister::N, true);
    self.set(FlagRegister::C, Self::sub_and_borrow(a as _, b as _, 0).1);
    self.set(FlagRegister::H, Self::sub_and_borrow(a as _, b as _, 4).1);
    res
  }

  pub fn inc8(&mut self, a: u8) -> u8 {
    let res = a.wrapping_add(1);
    self.set(FlagRegister::Z, res == 0);
    self.set(FlagRegister::N, false);
    self.set(FlagRegister::H, Self::add_and_carry(a as _, 1, 4).1);
    res
  }

  pub fn dec8(&mut self, a: u8) -> u8 {
    let res = a.wrapping_sub(1);
    self.set(FlagRegister::Z, res == 0);
    self.set(FlagRegister::N, true);
    self.set(FlagRegister::H, Self::sub_and_borrow(a as _, 1, 4).1);
    res
  }

  pub fn add16(&mut self, a: u16, b: u16) -> u16 {
    let res = a.wrapping_add(b);
    self.set(FlagRegister::N, false);
    self.set(FlagRegister::C, Self::add_and_carry(a as _, b as _, 16).1);
    self.set(FlagRegister::H, Self::add_and_carry(a as _, b as _, 12).1);
    res
  }

  /// Only needed for ADD SP, r8
  pub fn add_u16_i8(&mut self, sp: u16, r: i8) -> u16 {
    let res = (sp as i32) + (r as i32);
    let res = (res % (1 << 16)) as u16;
    self.set(FlagRegister::Z, false);
    self.set(FlagRegister::N, false);
    if r >= 0 {
      // Assume sp is always high since it's the stack pointer
      self.set(FlagRegister::C, Self::add_and_carry(sp as _, r as _, 16).1);
      self.set(FlagRegister::H, Self::add_and_carry(sp as _, r as _, 12).1);
    } else {
      self.set(FlagRegister::C, Self::sub_and_borrow(sp as _, -r as _, 8).1);
      self.set(FlagRegister::H, Self::add_and_carry(sp as _, -r as _, 12).1);
    }
    res
  }

  pub fn set_flags(&mut self, z: Option<bool>, n: Option<bool>, h: Option<bool>, c: Option<bool>) -> Self {
    if let Some(z) = z {
      self.set(Self::Z, z);
    }
    if let Some(n) = n {
      self.set(Self::N, n);
    }
    if let Some(h) = h {
      self.set(Self::H, h);
    }
    if let Some(c) = c {
      self.set(Self::C, c);
    }
    *self
  }

  fn add_and_carry(a: u32, b: u32, bit: u8) -> (u32, bool) {
    let x = 1 << bit;
    let mask = x - 1;
    let a = a & mask;
    let b = b & mask;
    let c = a+b;
    (c, c >= x)
  }

  fn sub_and_borrow(a: u32, b: u32, bit: u8) -> (i32, bool) {
    let x = 1 << bit;
    let mask = (u32::MAX-x)+1;
    let a = (a & mask) as i32;
    let b = (b & mask) as i32;
    let c = a-b;
    (c, c < 0)
  }
}
