
pub enum OPCode {
  /// 0x00, no operation
  Nop,
  /// 0x10
  Stop,
  /// 0x76
  Halt,
  /// 0xCB, indicates that the next bytes define the instruction
  CB(CBPrefixedOPCode),
  /// Copy 8bit from `src` to `dst`
  LD8 { dst: Var8Ext, src: Val8Ext },
  /// Copy 16bit from `src` to `dst`
  LD16 { dst: Var16, src: Val16Ext },
  /// Increment
  Inc(Var),
  /// Decrement
  Dec(Var),
  /// `A` += (val [+ CarryFlag if cond])
  AddToA(Val8, bool),
  /// `HL` += val
  AddToHL(Reg16),
  /// `SP` += val
  AddToSP(i8),
  /// Bitwise `A` -= (val [+ CarryFlag if cond])
  Sub(Val8, bool),
  /// Bitwise `A` &= val
  And(Val8),
  /// Bitwise `A` ^= val
  Xor(Val8),
  /// Bitwise `A` |= val
  Or(Val8),
  /// Check whether `A` == val
  CP(Val8),
  /// Decimal Adjust Accumulator for BCD addition and subtraction
  DAA,
  /// Bitwise flip accumulator register, `A` = ~`A`
  CPL,
  /// Mutate the carry flag
  MutCarryFlag(BitMut),
  /// Checks for Condition. If None or `true` add `offset` to the adress, add `add_cycles` to cycles and jump to the new adress
  Jump { cond: Option<Cond>, add_cycles: u8, target: PtrTarget },
  /// Checks for Condition. If None or `true` add `add_cycles` to cycles, pops `SP` into `PC` and the execution is
  /// continued at the new `PC`
  Return { cond: Option<Cond>, add_cycles: u8 },
  /// Add `add_cycles` to cycles, pop `SP` into `PC`. Then the execution is continued at the new `PC` and interrupts are
  /// enabled
  ReturnInterrupt { add_cycles: u8 },
  /// Checks for Condition. If None or `true` add `add_cycles` to cycles, push `PC` onto `SP`, jump to `target` and continue
  /// execution.
  Call { cond: Option<Cond>, add_cycles: u8, target: PtrTarget },
  /// Bitwise rotation. `dir` determines direction. `with_carry` determines whether the carry flag is treated like the highest bit
  /// `with_carry: true` corresponds to the R(LR)-instruction and `with_carry: false` corresponds to R(LR)C-instruction
  Rotate { dir: ShiftDir, with_carry: bool },
  /// Pop 16bit Value from stack into register
  Pop(Reg16),
  /// Push 16bit Value onto stack from register
  Push(Reg16),
  /// Disable interruptions after the next instruction is executed
  DisableInterrupts,
  /// Enable interruptions after the next instruction is executed
  EnableInterrupts,
  /// Unsupported OPCode
  Unsupported,
}


#[derive(Debug, Clone, Copy)]
pub enum Reg8 {
  /// A register
  A,
  /// B register
  B,
  /// C register
  C,
  /// D register
  D,
  /// E register
  E,
  /// H register
  H,
  /// L register
  L,
}

#[derive(Debug, Clone, Copy)]
pub enum Reg16 {
  /// AF register
  AF,
  /// BC register
  BC,
  /// DE register
  DE,
  /// HL register
  HL,
  /// SP register
  SP,
}

#[derive(Debug, Clone, Copy)]
pub enum Var8 {
  ValReg(Reg8),
  DerefReg(Reg16),
  Deref(u16),
}

#[derive(Debug, Clone, Copy)]
pub enum Var8Ext {
  Var(Var8),
  Deref8(u8), //+ 0xFF00
  DerefReg8(Reg8), //+ 0xFF00
  HLpp,
  HLmm,
}

#[derive(Debug, Clone, Copy)]
pub enum Var16 {
  ValReg(Reg16),
  DerefReg(Reg16),
  Deref(u16),
}  

#[derive(Debug, Clone, Copy)]
pub enum Var {
  Var8(Var8),
  Var16(Var16),
}

#[derive(Debug, Clone, Copy)]
pub enum Val8 {
  Var(Var8),
  Val(u8)
}  

#[derive(Debug, Clone, Copy)]
pub enum Val8Ext {
  Var(Var8),
  // SPPlus(i8), //SP + value
  HLpp,
  HLmm,
  Val(u8)
}  

#[derive(Debug, Clone, Copy)]
pub enum Val16 {
  Var(Var16),
  Val(u16)
}  

#[derive(Debug, Clone, Copy)]
pub enum Val16Ext {
  Var(Var16),
  SPPlus(i8), //SP + value
  Val(u16)
}  


#[derive(Debug, Clone, Copy)]
pub enum Cond {
  NZ,
  Z,
  NC,
  C,
}

impl Cond {
  pub fn test(&self, reg: &super::FlagRegister) -> bool {
    match self {
      Cond::NZ => !reg.contains(super::FlagRegister::Z),
      Cond::Z => reg.contains(super::FlagRegister::Z),
      Cond::NC => !reg.contains(super::FlagRegister::C),
      Cond::C => reg.contains(super::FlagRegister::C),
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub enum ShiftDir {
  Left,
  Right,
}


#[derive(Debug, Clone, Copy)]
pub enum BitMut {
  Set,
  Unset,
  Flip,
  Test,
}


#[derive(Debug, Clone, Copy)]
pub enum U8Bit {
  Bit0 = 1 << 0,
  Bit1 = 1 << 1,
  Bit2 = 1 << 2,
  Bit3 = 1 << 3,
  Bit4 = 1 << 4,
  Bit5 = 1 << 5,
  Bit6 = 1 << 6,
  Bit7 = 1 << 7,
}

#[derive(Debug, Clone, Copy)]
pub enum CBPrefixedOPCode {
  /// Bitwise rotation. `dir` determines direction. `with_carry` determines whether the carry flag is treated like the highest bit
  /// `with_carry: true` corresponds to the R(LR)-instruction and `with_carry: false` corresponds to R(LR)C-instruction
  Rotate { dir: ShiftDir, with_carry: bool, var: Var8 },
  /// Bitwise shift. `dir` determines direction. Fill bit with 0
  Shift(ShiftDir, Var8),
  /// Bitwise shift to the right and keep the MSB
  ShiftRightKeepMSB(Var8),
  /// Swap higher 4 bits with lower 4 bits
  SwapNibbles(Var8),
  /// Mutate a single bit
  BitMutation(BitMut, U8Bit, Var8),
}

#[derive(Debug, Clone, Copy)]
pub enum PtrTarget {
  Offset(i8),
  Ptr(u16),
  DerefHL,
}



impl From<Reg8> for Var8 { fn from(value: Reg8) -> Self { Self::ValReg(value) } }
impl From<Reg16> for Var8 { fn from(value: Reg16) -> Self { Self::DerefReg(value) } }
impl From<u16> for Var8 { fn from(value: u16) -> Self { Self::Deref(value) } }

impl From<u8> for Val8 { fn from(value: u8) -> Self { Self::Val(value) } }
impl From<Var8> for Val8 { fn from(value: Var8) -> Self { Self::Var(value) } }

impl<V8: Into<Var8>> From<V8> for Var8Ext { fn from(value: V8) -> Self { Self::Var(value.into()) } }
impl<V8: Into<Val8>> From<V8> for Val8Ext {
  fn from(value: V8) -> Self {
    match value.into() {
      Val8::Var(x) => Self::Var(x),
      Val8::Val(x) => Self::Val(x),
    }
  }
}


impl TryFrom<Var8Ext> for Val8Ext {
  type Error = ();
  fn try_from(value: Var8Ext) -> Result<Self, Self::Error> {
    match value {
      Var8Ext::Var(x) => Ok(Self::Var(x)),
      Var8Ext::HLpp => Ok(Self::HLpp),
      Var8Ext::HLmm => Ok(Self::HLmm),
      _ => Err(())
    }
  }
}


impl From<u16> for Val16 { fn from(value: u16) -> Self { Self::Val(value) } }
impl From<Var16> for Val16 { fn from(value: Var16) -> Self { Self::Var(value) } }


impl<V16: Into<Var16>> From<V16> for Val16Ext { fn from(value: V16) -> Self { Self::Var(value.into()) } }


impl From<Var8> for Var { fn from(value: Var8) -> Self { Self::Var8(value) } }
impl From<Var16> for Var { fn from(value: Var16) -> Self { Self::Var16(value) } }


impl From<i8> for PtrTarget { fn from(value: i8) -> Self { Self::Offset(value) } }
impl From<u16> for PtrTarget { fn from(value: u16) -> Self { Self::Ptr(value) } }


impl TryFrom<u8> for U8Bit {
  type Error = u8;
  fn try_from(value: u8) -> Result<Self, Self::Error> {
    match value {
      0 => Ok(Self::Bit0),
      1 => Ok(Self::Bit1),
      2 => Ok(Self::Bit2),
      3 => Ok(Self::Bit3),
      4 => Ok(Self::Bit4),
      5 => Ok(Self::Bit5),
      6 => Ok(Self::Bit6),
      7 => Ok(Self::Bit7),
      8.. => Err(value)
    }
  }
}


pub trait MemoryLookup<T> {
  fn get(&self, register: &super::Register, memory: &super::Heap) -> T;
  fn get_mut<'a: 'c, 'b: 'c, 'c>(&self, register: &'a mut super::Register, memory: &'b mut super::Heap) -> &'c mut T;
}

impl MemoryLookup<u8> for Reg8 {
  fn get(&self, register: &super::Register, _memory: &super::Heap) -> u8 {
    match self {
      Reg8::A => register.A,
      Reg8::B => register.B,
      Reg8::C => register.C,
      Reg8::D => register.D,
      Reg8::E => register.E,
      Reg8::H => register.H,
      Reg8::L => register.L,
    }
  }

  fn get_mut<'a:'c,'b:'c,'c>(&self, register: &'a mut super::Register, _memory: &'b mut super::Heap) -> &'c mut u8 {
    match self {
      Reg8::A => &mut register.A,
      Reg8::B => &mut register.B,
      Reg8::C => &mut register.C,
      Reg8::D => &mut register.D,
      Reg8::E => &mut register.E,
      Reg8::H => &mut register.H,
      Reg8::L => &mut register.L,
    }
  }

}

impl MemoryLookup<u16> for Reg16 {
  fn get(&self, register: &super::Register, _memory: &super::Heap) -> u16 {
    match self {
      Reg16::AF => register.AF(),
      Reg16::BC => register.BC(),
      Reg16::DE => register.DE(),
      Reg16::HL => register.HL(),
      Reg16::SP => register.SP,
    }
  }

  fn get_mut<'a:'c,'b:'c,'c>(&self, register: &'a mut super::Register, _memory: &'b mut super::Heap) -> &'c mut u16 {
    match self {
      Reg16::AF => register.AF_mut(),
      Reg16::BC => register.BC_mut(),
      Reg16::DE => register.DE_mut(),
      Reg16::HL => register.HL_mut(),
      Reg16::SP => &mut register.SP,
    }
  }
}

impl MemoryLookup<u8> for Var8 {
  fn get(&self, register: &super::Register, memory: &super::Heap) -> u8 {
    match self {
      Var8::ValReg(reg) => reg.get(register, memory),
      Var8::DerefReg(reg) => {
        let addr_16bit = reg.get(register, memory);
        super::read_from_offset(memory.as_ref(), addr_16bit as _)
      },
      Var8::Deref(addr) => super::read_from_offset(memory.as_ref(), *addr as _),
    }
  }

  fn get_mut<'a:'c, 'b:'c, 'c>(&self, register: &'a mut super::Register, memory: &'b mut super::Heap) -> &'c mut u8 {
    match self {
      Var8::ValReg(reg) => reg.get_mut(register, memory),
      Var8::DerefReg(reg) => {
        let addr_16bit = reg.get(register, memory);
        super::mut_at_offset(memory.as_mut(), addr_16bit as _)
      },
      Var8::Deref(addr) => super::mut_at_offset(memory.as_mut(), *addr as _),
    }
  }
}

impl MemoryLookup<u16> for Var16 {
  fn get(&self, register: &super::Register, memory: &super::Heap) -> u16 {
    match self {
      Var16::ValReg(reg) => reg.get(register, memory),
      Var16::DerefReg(reg) => {
        let addr_16bit = reg.get(register, memory);
        super::read_from_offset(memory.as_ref(), addr_16bit as _)
      },
      Var16::Deref(addr) => super::read_from_offset(memory.as_ref(), *addr as _),
    }
  }

  fn get_mut<'a:'c, 'b:'c, 'c>(&self, register: &'a mut super::Register, memory: &'b mut super::Heap) -> &'c mut u16 {
    match self {
      Var16::ValReg(reg) => reg.get_mut(register, memory),
      Var16::DerefReg(reg) => {
        let addr_16bit = reg.get(register, memory);
        super::mut_at_offset(memory.as_mut(), addr_16bit as _)
      },
      Var16::Deref(addr) => super::mut_at_offset(memory.as_mut(), *addr as _),
    }
  }
}


