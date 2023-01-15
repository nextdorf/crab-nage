pub mod instruction;
pub mod opcodes;

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



