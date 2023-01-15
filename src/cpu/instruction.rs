use super::opcodes::*;
use super::read_next;

pub struct Instruction {
  pub op: OPCode,
  pub length: u8,
  pub cycles: u8,
}

impl Instruction {
  pub fn read(stack: &[u8]) -> Self {
    let opcode = stack[0];
    let memory = &stack[1..];
    match opcode {
      0x00..=0x3F => Self::read_0x00_0x40(opcode, memory),
      0x40..=0x7F => Self::read_0x40_0x80(opcode),
      0x80..=0xBF => Self::read_0x80_0xc0(opcode),
      0xC0..=0xFF => Self::read_0xc0_0x100(opcode, memory),
    }
  }

  pub fn unsupported() -> Self {
    Self { op: OPCode::Unsupported, length: 0, cycles: 255 }
  }

  fn read_0x00_0x40(opcode: u8, memory: &[u8]) -> Self {
    assert!((..0x40).contains(&opcode));
    match opcode % 8 {
      0 => Self::read_0x00_0x40_x0_x8(opcode, memory),
      1 => if opcode % 16 == 1 {
        Self::read_0x00_0x40_x1(opcode, memory)
      } else {
        Self::read_0x00_0x40_x9(opcode)
      },
      2 | 6 => Self::read_0x00_0x40_x2_x6_xa_xe(opcode, memory),
      3 => Self::read_0x00_0x40_x3_xb(opcode),
      4 | 5 => Self::read_0x00_0x40_x4_x5_xc_xd(opcode),
      7 => Self::read_0x00_0x40_x7_xf(opcode),
      8.. => Err(opcode).expect("Unreachable")
    }
  }
  fn read_0x40_0x80(opcode: u8) -> Self {
    assert!((0x40..0x80).contains(&opcode));
    if opcode == 0x76 {
      return Self { op: OPCode::Halt, length: 1, cycles: 4 };
    }
    let op_high = (opcode - 0x40) / 8;
    let op_low = (opcode - 0x40) % 8;
    let (dst, dst_cycle) = Self::parse_b_c_d_e_h_l_hl_a(op_high);
    let (src, src_cycle) = Self::parse_b_c_d_e_h_l_hl_a(op_low);
    let op = OPCode::LD8 { dst: dst.into(), src: src.into() };
    let cycles = dst_cycle + src_cycle - 4;
    Self { op, length: 1, cycles }
  }
  fn read_0x80_0xc0(opcode: u8) -> Self {
    assert!((0x80..0xC0).contains(&opcode));
    let op_high = (opcode - 0x80) / 8;
    let op_low = (opcode - 0x80) % 8;
    let (val, cycles) = Self::parse_b_c_d_e_h_l_hl_a(op_low);
    let val = val.into();
    let op = match op_high {
      0 => OPCode::AddToA(val, false),
      1 => OPCode::AddToA(val, true),
      2 => OPCode::Sub(val, false),
      3 => OPCode::Sub(val, true),
      4 => OPCode::And(val),
      5 => OPCode::Xor(val),
      6 => OPCode::Or(val),
      7 => OPCode::CP(val),
      8.. => Err(opcode).expect("Unreachable")
    };
    Self { op, length: 1, cycles }
  }
  fn read_0xc0_0x100(opcode: u8, memory: &[u8]) -> Self {
    let op_high = (opcode - 0xC0) / 16;
    let op_low = (opcode - 0xC0) % 16;
    match op_low {
      0 | 2..=4 | 8 | 10..=12 => {
        if op_high < 2 {
          Self::read_0xc0_0xe0_x0_x2_x3_x4_and_high(opcode, memory)
        } else if (op_low % 8) <= 2 {
          Self::read_0xe0_0x100_x0_x2_x8_xa(opcode, memory)
        } else { // (op_low % 8) > 2
          Self::read_0xe0_0x100_x3_x4_xb_xc(opcode)
        }
      },
      1 | 5 => Self::read_0xc0_0x100_x1_x5(opcode),
      9 | 13 => Self::read_0xc0_0x100_x9_xd(opcode, memory),
      6 | 14 => Self::read_0xc0_0x100_x6_xe(opcode, memory),
      7 | 15 => Self::read_0xc0_0x100_x7_xf(opcode),
      16.. => Err(opcode).expect("Unreachable"),
    }
    // todo!()
  }


  fn read_0x00_0x40_x0_x8(opcode: u8, memory: &[u8]) -> Self {
    assert!((..0x40).contains(&opcode) && (opcode % 8 == 0));
    let packed_op = opcode / 8;
    match packed_op {
      0 => Self { op: OPCode::Nop, length: 1, cycles: 4 },
      1 => Self { length: 3, cycles: 20, op: OPCode::LD16 {
        dst: Var16::Deref(read_next(memory)),
        src: Var16::ValReg(Reg16::SP).into()
      }},
      2 => Self { op: OPCode::Stop, length: 2, cycles: 4 },
      3..=7 => {
        let cond = Self::parse_opt_cond(packed_op - 3);
        Self { op: OPCode::Jump { cond, add_cycles: 4, target: read_next::<i8>(memory).into() }, length: 2, cycles: 8 }
      },
      8.. => Err(opcode).expect("Unreachable")
    }
  }

  fn read_0x00_0x40_x1(opcode: u8, memory: &[u8]) -> Self {
    assert!((..0x40).contains(&opcode) && (opcode % 16 == 1));
    Self { length: 3, cycles: 12, op: OPCode::LD16 {
      dst: Self::parse_bc_de_hl_sp_reg(opcode / 16).0,
      src: Val16Ext::Val(read_next(memory)),
    }}
  }

  fn read_0x00_0x40_x9(opcode: u8) -> Self {
    assert!((..0x40).contains(&opcode) && (opcode % 16 == 9));
    let (reg, cycles) = Self::parse_bc_de_hl_sp_reg_raw(opcode / 16);
    Self { op: OPCode::AddToHL(reg), length: 1, cycles }
  }

  fn read_0x00_0x40_x2_x6_xa_xe(opcode: u8, memory: &[u8]) -> Self {
    assert!((..0x40).contains(&opcode) && (opcode % 4 == 2));
    let packed_op = (opcode - 2) / 4;
    let (dst, src, length, cycles);
    if packed_op % 2 == 0 {// X2, XA
      let (parsed, cycles0) = Self::parse_bc_de_hl_pm_deref(packed_op/4);
      cycles = cycles0;
      length = 1;
      (dst, src) = if packed_op % 4 == 1 { // X2
        (parsed, Into::<Var8>::into(Reg8::A).into())
      } else { // packed_op % 4 == 3, XA
        (Reg8::A.into(), parsed.try_into().unwrap())
      }
    } else { // X6, XE
      let packed_op = packed_op / 2;
      let (dst0, dst_cycle) = Self::parse_b_c_d_e_h_l_hl_a(packed_op);
      length = 2;
      cycles = dst_cycle + 4;
      (dst, src) = (dst0.into(), read_next::<u8>(memory).into());
    }
    Self { op: OPCode::LD8 { dst, src }, length, cycles }
  }

  fn read_0x00_0x40_x3_xb(opcode: u8) -> Self {
    assert!((..0x40).contains(&opcode) && (opcode & 0x7 == 3));
    let packed_op = (opcode - 3) / 8;
    let (value, cycles) = Self::parse_bc_de_hl_sp_reg(packed_op/2);
    let op = if packed_op % 2 == 0 { OPCode::Inc } else { OPCode::Dec }(value.into());
    Self { op, length: 1, cycles }
  }

  fn read_0x00_0x40_x4_x5_xc_xd(opcode: u8) -> Self {
    assert!((..0x40).contains(&opcode) && (opcode & 0x7 == 4 || opcode & 0x7 == 5));
    let packed_op = (opcode - 4) / 8;
    let (value, cycles) = Self::parse_b_c_d_e_h_l_hl_a(packed_op);
    let op = if opcode % 8 == 4 { OPCode::Inc } else { OPCode::Dec }(value.into());
    Self { op, length: 1, cycles: cycles*2 - 4 }
  }

  fn read_0x00_0x40_x7_xf(opcode: u8) -> Self {
    assert!((..0x40).contains(&opcode) && (opcode % 8 == 7));
    let packed_op = opcode / 8;
    match packed_op {
      0..=3 => {
        let dir = if packed_op & 1 == 0 { ShiftDir::Left } else { ShiftDir::Right };
        let with_carry = packed_op & 2 != 0;
        Self { op: OPCode::Rotate { dir, with_carry }, length: 1, cycles: 4 }
      },
      4 => Self { op: OPCode::DAA, length: 1, cycles: 4 },
      5 => Self { op: OPCode::CPL, length: 1, cycles: 4 },
      6 => Self { op: OPCode::MutCarryFlag(BitMut::Set), length: 1, cycles: 4 },
      7 => Self { op: OPCode::MutCarryFlag(BitMut::Flip), length: 1, cycles: 4 },
      8.. => Err(opcode).expect("Unreachable")
    }
  }


  fn read_0xc0_0xe0_x0_x2_x3_x4_and_high(opcode: u8, memory: &[u8]) -> Self {
    assert!((0xC0..0xE0).contains(&opcode) && [0,2,3,4].contains(&(opcode % 8)));
    let (op_high, op_low) = ((opcode - 0xC0) / 8, opcode % 8);
    let cond = Some(Self::parse_cond(op_high));
    match op_low {
      0 => Self { op: OPCode::Return { cond, add_cycles: 12 }, length: 1, cycles: 8 },
      2 => Self { length: 3, cycles: 4,
        op: OPCode::Jump { cond, add_cycles: 12, target: read_next::<u16>(memory).into() }
      },
      3 => match op_high {
        0 => Self { length: 3, cycles: 4,
          op: OPCode::Jump { cond: None, add_cycles: 12, target: read_next::<u16>(memory).into() }
        },
        1 => { // Prefix CB
          let (op, length, cycles) = Self::read_cb(memory);
          Self { op: OPCode::CB(op), length, cycles }
        },
        2 | 3 => Self::unsupported(),
        4.. => Err(opcode).expect("Unreachable")
      },
      4 => Self { length: 3, cycles: 12,
        op: OPCode::Call { cond, add_cycles: 12, target: read_next::<u16>(memory).into() }
      },
      1 | 5.. => Err(opcode).expect("Unreachable")
    }
  }

  fn read_0xe0_0x100_x0_x2_x8_xa(opcode: u8, memory: &[u8]) -> Self {
    assert!((0xE0..).contains(&opcode) && [0,2].contains(&(opcode % 8)));
    match opcode {
      0xE0 => Self { length: 2, cycles: 12, op: OPCode::LD8 {
        dst: Var8Ext::Deref8(read_next::<u8>(memory)),
        src: Var8::ValReg(Reg8::A).into(),
      }},
      0xF0 => Self { length: 2, cycles: 12, op: OPCode::LD8 {
        dst: Var8::ValReg(Reg8::A).into(),
        src: Var8Ext::Deref8(read_next::<u8>(memory)).try_into().unwrap(),
      }},
      0xE2 => Self { length: 2, cycles: 8, op: OPCode::LD8 {
        dst: Var8::ValReg(Reg8::C).into(),
        src: Var8::ValReg(Reg8::A).into(),
      }},
      0xF2 => Self { length: 2, cycles: 8, op: OPCode::LD8 {
        dst: Var8::ValReg(Reg8::A).into(),
        src: Var8::ValReg(Reg8::C).into(),
      }},

      0xE8 => Self { op: OPCode::AddToSP(read_next(memory)), length: 2, cycles: 16 },
      0xF8 => Self { length: 2, cycles: 12, op: OPCode::LD16 {
        dst: Var16::ValReg(Reg16::HL),
        src: Val16Ext::SPPlus(read_next(memory)),
      }},
      0xEA => Self { length: 3, cycles: 16, op: OPCode::LD8 {
        dst: Var8::Deref(read_next(memory)).into(),
        src: Var8::ValReg(Reg8::A).into(),
      }},
      0xFA => Self { length: 3, cycles: 16, op: OPCode::LD8 {
        dst: Var8::ValReg(Reg8::A).into(),
        src: Var8::Deref(read_next(memory)).into(),
      }},

      _ => Err(opcode).expect("Unreachable"),
    }
  }

  fn read_0xc0_0x100_x1_x5(opcode: u8) -> Self {
    assert!((0xC0..).contains(&opcode) && [1,5].contains(&(opcode % 16)));
    let op_high = (opcode - 0xC0) / 16;
    let reg16 = Self::parse_bc_de_hl_af_reg_raw(op_high).0;
    let (op, cycles) = if opcode % 16 == 1 {
      (OPCode::Pop(reg16), 12)
    } else {
      (OPCode::Push(reg16), 16)
    };
    Self { op, length: 1, cycles }
  }
  
  fn read_0xc0_0x100_x6_xe(opcode: u8, memory: &[u8]) -> Self {
    assert!((0xC0..).contains(&opcode));
    let packed_op = (opcode - 0xC0) / 8;
    let val = Val8::Val(read_next(memory));
    let op = match packed_op {
      0 => OPCode::AddToA(val, false),
      1 => OPCode::AddToA(val, true),
      2 => OPCode::Sub(val, false),
      3 => OPCode::Sub(val, true),
      4 => OPCode::And(val),
      5 => OPCode::Xor(val),
      6 => OPCode::Or(val),
      7 => OPCode::CP(val),
      8.. => Err(opcode).expect("Unreachable")
    };
    Self { op, length: 2, cycles: 8 }
  }

  fn read_0xe0_0x100_x3_x4_xb_xc(opcode: u8) -> Self {
    assert!((0xE0..).contains(&opcode) && [3, 4].contains(&(opcode % 8)));
    match opcode {
      0xF3 => Self { op: OPCode::DisableInterrupts, length: 1, cycles: 4 },
      0xFB => Self { op: OPCode::EnableInterrupts, length: 1, cycles: 4 },
      0xE3 | 0xE4 | 0xF4 | 0xEB | 0xEC | 0xFC => Self::unsupported(),
      _ => Err(opcode).expect("Not part of the pattern matching")
    }
  }

  fn read_0xc0_0x100_x7_xf(opcode: u8) -> Self {
    assert!((0xC0..).contains(&opcode) && (opcode % 8 == 7));
    let offset = (opcode as u16 - 0xC0)/8;
    let target = PtrTarget::Ptr(offset*8);
    Self { op: OPCode::Call { cond: None, add_cycles: 4, target }, length: 1, cycles: 12 }
  }

  fn read_0xc0_0x100_x9_xd(opcode: u8, memory: &[u8]) -> Self {
    assert!((0xC0..).contains(&opcode) && [9,12].contains(&(opcode % 16)));
    match opcode {
      0xC9 => Self { length: 1, cycles: 4, op: OPCode::Return { cond: None, add_cycles: 12 }},
      0xD9 => Self { length: 1, cycles: 4, op: OPCode::ReturnInterrupt { add_cycles: 12 }},
      0xE9 => Self { length: 1, cycles: 12,
        op: OPCode::Jump { cond: None, add_cycles: 0, target: PtrTarget::DerefHL }
      },
      0xF9 => Self { length: 1, cycles: 8, op: OPCode::LD16 {
        dst: Var16::ValReg(Reg16::SP), src: Var16::ValReg(Reg16::HL).into(),
      }},

      0xCD => Self { length: 3, cycles: 12,
        op: OPCode::Call { cond: None, add_cycles: 12, target: read_next::<u16>(memory).into() }
      },
      0xDD | 0xED | 0xFD => Self::unsupported(),

      _ => Err(opcode).expect("Unreachable"),
    }
  }
  
  fn parse_b_c_d_e_h_l_hl_a(x: u8) -> (Var8, u8) {
    match x {
      0 => (Reg8::B.into(), 4),
      1 => (Reg8::C.into(), 4),
      2 => (Reg8::D.into(), 4),
      3 => (Reg8::E.into(), 4),
      4 => (Reg8::H.into(), 4),
      5 => (Reg8::L.into(), 4),
      6 => (Reg16::HL.into(), 8),
      7 => (Reg8::A.into(), 4),
      8.. => panic!("Expected to parse only values x in 0..8")
    }
  }

  fn parse_bc_de_hl_sp_reg_raw(x: u8) -> (Reg16, u8) {
    match x {
      0 => (Reg16::BC, 8),
      1 => (Reg16::DE, 8),
      2 => (Reg16::HL, 8),
      3 => (Reg16::SP, 8),
      4.. => panic!("Expected to parse only values x in 0..4")
    }
  }
  fn parse_bc_de_hl_sp_reg(x: u8) -> (Var16, u8) {
    let (reg, cycles) = Self::parse_bc_de_hl_sp_reg_raw(x);
    (Var16::ValReg(reg), cycles)
  }

  fn parse_bc_de_hl_af_reg_raw(x: u8) -> (Reg16, u8) {
    match x {
      0 => (Reg16::BC, 8),
      1 => (Reg16::DE, 8),
      2 => (Reg16::HL, 8),
      3 => (Reg16::AF, 8),
      4.. => panic!("Expected to parse only values x in 0..4")
    }
  }
  fn _parse_bc_de_hl_af_reg(x: u8) -> (Var16, u8) {
    let (reg, cycles) = Self::parse_bc_de_hl_af_reg_raw(x);
    (Var16::ValReg(reg), cycles)
  }

  fn parse_bc_de_hl_pm_deref(x: u8) -> (Var8Ext, u8) {
    match x {
      0 => (Reg16::BC.into(), 8),
      1 => (Reg16::DE.into(), 8),
      2 => (Var8Ext::HLpp, 8),
      3 => (Var8Ext::HLmm, 8),
      4.. => panic!("Expected to parse only values x in 0..4")
    }
  }

  fn parse_cond(x: u8) -> Cond {
    match x {
      0 => Cond::NZ,
      1 => Cond::Z,
      2 => Cond::NC,
      3 => Cond::C,
      4.. => Err(x).expect("")
    }
  }
  fn parse_opt_cond(x: u8) -> Option<Cond> {
    if x == 0 { None } else { Some(Self::parse_cond(x-1)) }
  }

  fn read_cb(memory: &[u8]) -> (CBPrefixedOPCode, u8, u8) {
    let opcode = memory[0];
    let (op_high, op_low) = (opcode / 8, opcode % 8);
    let (reg, cycles) = Self::parse_b_c_d_e_h_l_hl_a(op_low);
    let op = match op_high {
      0 => CBPrefixedOPCode::Rotate { dir: ShiftDir::Left, with_carry: false, var: reg },
      1 => CBPrefixedOPCode::Rotate { dir: ShiftDir::Right, with_carry: false, var: reg },
      2 => CBPrefixedOPCode::Rotate { dir: ShiftDir::Left, with_carry: true, var: reg },
      3 => CBPrefixedOPCode::Rotate { dir: ShiftDir::Right, with_carry: true, var: reg },
      4 => CBPrefixedOPCode::Shift(ShiftDir::Left, reg),
      5 => CBPrefixedOPCode::ShiftRightKeepMSB(reg),
      6 => CBPrefixedOPCode::SwapNibbles(reg),
      7 => CBPrefixedOPCode::Shift(ShiftDir::Right, reg),
      8..=15 => CBPrefixedOPCode::BitMutation(BitMut::Test, (op_high-8).try_into().unwrap(), reg),
      16..=23 => CBPrefixedOPCode::BitMutation(BitMut::Unset, (op_high-16).try_into().unwrap(), reg),
      24..=31 => CBPrefixedOPCode::BitMutation(BitMut::Set, (op_high-24).try_into().unwrap(), reg),
      32.. => Err(opcode).expect("Unreachable")
    };
    (op, 2, cycles*2)
  }
}
