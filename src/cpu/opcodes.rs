
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
  /// Mutate a single bit
  BitMutation(BitMut, RegisterBit),
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
  Rotate { dir: ShiftDir, with_carry: bool, var_or_arith: Option<Var> },
  /// Bitwise shift. `dir` determines direction.
  Shift(ShiftDir, Var),
  /// Pop 16bit Value from register
  Pop(Reg16),
  /// Push 16bit Value onto register
  Push(Reg16),
  /// Disable interruptions after the next instruction is executed
  DisableInterrupts,
  /// Enable interruptions after the next instruction is executed
  EnableInterrupts,
  /// Unsupported OPCode
  Unsupported,
}


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

pub enum Var8 {
  ValReg(Reg8),
  DerefReg(Reg16),
  Deref(u16),
}

pub enum Var8Ext {
  Var(Var8),
  Deref8(u8), //+ 0xFF00
  DerefReg8(Reg8), //+ 0xFF00
  HLpp,
  HLmm,
}

pub enum Var16 {
  ValReg(Reg16),
  DerefReg(Reg16),
  Deref(u16),
}  

pub enum Var {
  Var8(Var8),
  Var16(Var16),
}

pub enum Val8 {
  Var(Var8),
  Val(u8)
}  

pub enum Val8Ext {
  Var(Var8),
  // SPPlus(i8), //SP + value
  HLpp,
  HLmm,
  Val(u8)
}  

pub enum Val16 {
  Var(Var16),
  Val(u16)
}  

pub enum Val16Ext {
  Var(Var16),
  SPPlus(i8), //SP + value
  Val(u16)
}  


pub enum Cond {
  NZ,
  Z,
  NC,
  C,
}

pub enum ShiftDir {
  Left,
  Right,
}


pub enum BitMut {
  Set,
  Unset,
  Flip
}

pub enum RegisterBit {
  Carry,
  Reg8(Reg8, U8Bit)
}

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

pub enum CBPrefixedOPCode {
}

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



