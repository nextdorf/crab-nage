pub mod instruction;
pub mod opcodes;
pub mod register;
mod util;


pub use util::*;

use register::Register;

use crate::cpu::register::FlagRegister;

use self::{opcodes::{OPCode, MemoryLookup, CBPrefixedOPCode}, instruction::Instruction};


pub type Heap = Box<[u8; crate::constants::MAIN_RAM_IN_BYTE as _]>;

pub struct CPU {
  pub register: Register,
  pub memory: Heap,
}

impl CPU {
  pub fn get_code(&mut self) -> &[u8] {
    &self.memory[(self.register.PC as _)..]
  }

  pub fn get_mut_code(&mut self) -> &mut [u8] {
    &mut self.memory[(self.register.PC as _)..]
  }

  // pub fn get_stack(&mut self) -> &[u8] {
  //   &self.memory[(self.register.SP as _)..]
  // }

  // pub fn get_mut_stack(&mut self) -> &mut [u8] {
  //   &mut self.memory[(self.register.SP as _)..]
  // }

  pub fn do_instruction(&mut self) -> CPUResult {
    use opcodes::*;
    let Instruction { op, length, mut cycles } = Instruction::read(self.get_code());
    let auto_pc_inc = match op {
      OPCode::Nop => true,
      OPCode::Stop => true, //TODO
      OPCode::Halt => true, //TODO
      OPCode::CB(cb_opcode) => {
        self.do_cb_instruction(cb_opcode);
        true
      },
      OPCode::LD8 { dst, src } => {
        let src_val = match src {
          Val8Ext::Var(v) => v.get(&self.register, &self.memory),
          Val8Ext::HLpp => {
            let val = Var8::DerefReg(Reg16::HL).get(&self.register, &self.memory);
            *self.register.HL_mut() += 1;
            val
          },
          Val8Ext::HLmm => {
            let val = Var8::DerefReg(Reg16::HL).get(&self.register, &self.memory);
            *self.register.HL_mut() -= 1;
            val
          },
          Val8Ext::Val(v) => v,
        };
        let dst_var = match dst {
          Var8Ext::Var(v) => v.get_mut(&mut self.register, &mut self.memory),
          Var8Ext::Deref8(_) => todo!(),
          Var8Ext::DerefReg8(_) => todo!(),
          Var8Ext::HLpp => {
            let var = Var8::DerefReg(Reg16::HL).get_mut(&mut self.register, &mut self.memory);
            *var += 1;
            var
          },
          Var8Ext::HLmm => {
            let var = Var8::DerefReg(Reg16::HL).get_mut(&mut self.register, &mut self.memory);
            *var -= 1;
            var
          },
        };
        *dst_var = src_val;
        true
      },
      OPCode::LD16 { dst, src } => {
        let src_val = match src {
          Val16Ext::Var(v) => v.get(&self.register, &self.memory),
          // Val16Ext::SPPlus(off) => self.register.F.add16(self.register.SP, off as _, true),
          Val16Ext::SPPlus(off) => self.register.F.add16(self.register.SP, off as _),
          Val16Ext::Val(v) => v,
        };
        let dst_var = dst.get_mut(&mut self.register, &mut self.memory);
        *dst_var = src_val;
        true
      },
      OPCode::Inc(v) => match v {
        Var::Var8(v) => {
          let val = v.get(&self.register, &self.memory);
          *v.get_mut(&mut self.register, &mut self.memory) = self.register.F.inc8(val);
          true
        },
        Var::Var16(v) => {
          *v.get_mut(&mut self.register, &mut self.memory) += 1;
          true
        },
      },
      OPCode::Dec(v) => match v {
        Var::Var8(v) => {
          let val = v.get(&self.register, &self.memory);
          *v.get_mut(&mut self.register, &mut self.memory) = self.register.F.dec8(val);
          true
        },
        Var::Var16(v) => {
          *v.get_mut(&mut self.register, &mut self.memory) -= 1;
          true
        },
      },
      OPCode::AddToA(val, with_carry) => {
        //FIXME: How exactly is carrying handled for ADC if Z=1 and val=255?
        let mut dst_val = match val {
            Val8::Var(v) => v.get(&self.register, &self.memory),
            Val8::Val(v) => v,
        };
        if with_carry {
          dst_val = dst_val.wrapping_add(
            if self.register.F.contains(FlagRegister::Z) {
              1
            } else {
              0
            }
          );
        }
        let src_val = &mut self.register.A;
        *src_val = self.register.F.add8(*src_val, dst_val);
        true
      },
      OPCode::AddToHL(var) => {
        let dst_val = var.get(&self.register, &self.memory);
        let src_val = &mut self.register.HL();
        *src_val = self.register.F.add16(*src_val, dst_val);
        true
      },
      OPCode::AddToSP(offset) => {
        let src_val = &mut self.register.SP;
        *src_val = self.register.F.add_u16_i8(*src_val, offset);
        true
      },
      OPCode::Sub(val, with_carry) => {
        //FIXME: How exactly is carrying handled for ADC if Z=1 and val=255?
        let mut dst_val = match val {
            Val8::Var(v) => v.get(&self.register, &self.memory),
            Val8::Val(v) => v,
        };
        if with_carry {
          dst_val = dst_val.wrapping_add(
            if self.register.F.contains(FlagRegister::Z) {
              1
            } else {
              0
            }
          );
        }
        let src_val = &mut self.register.A;
        *src_val = self.register.F.sub8(*src_val, dst_val);
        true
      },
      OPCode::And(val) => {
        let mut dst_val = match val {
          Val8::Var(v) => v.get(&self.register, &self.memory),
          Val8::Val(v) => v,
        };
        self.register.A &= dst_val;
        self.register.F.set_flags(Some(self.register.A == 0), Some(false), Some(true), Some(false));
        true
      },
      OPCode::Xor(val) => {
        let mut dst_val = match val {
          Val8::Var(v) => v.get(&self.register, &self.memory),
          Val8::Val(v) => v,
        };
        self.register.A ^= dst_val;
        self.register.F.set_flags(Some(self.register.A == 0), Some(false), Some(false), Some(false));
        true
      },
      OPCode::Or(val) => {
        let mut dst_val = match val {
          Val8::Var(v) => v.get(&self.register, &self.memory),
          Val8::Val(v) => v,
        };
        self.register.A |= dst_val;
        self.register.F.set_flags(Some(self.register.A == 0), Some(false), Some(true), Some(false));
        true
      },
      OPCode::CP(val) => {
        let dst_val = match val {
          Val8::Var(v) => v.get(&self.register, &self.memory),
          Val8::Val(v) => v,
        };
        self.register.F.sub8(self.register.A, dst_val);
        true
      },
      OPCode::DAA => true, //TODO
      OPCode::CPL => {
        self.register.A ^= 0xFF; 
        self.register.F.set_flags(None, Some(true), Some(true), None);
        true
      },
      OPCode::MutCarryFlag(action) => {
        let old_carry = self.register.F.contains(FlagRegister::C);
        let new_carry = match action {
          BitMut::Set => true,
          BitMut::Unset => false,
          BitMut::Flip => !old_carry,
          BitMut::Test => old_carry,
        };
        self.register.F.set_flags(None, Some(false), Some(false), Some(new_carry));
        true
      },
      OPCode::Jump { cond, add_cycles, target } => {
        let cond_is_met = if let Some(c) = cond {
          c.test(&self.register.F)
        } else {
          true
        };
        if cond_is_met {
          cycles += add_cycles;
          let new_addr = match target {
            PtrTarget::Offset(offset) => self.register.PC + offset as u16 + length as u16,
            PtrTarget::Ptr(addr) => addr,
            PtrTarget::DerefHL => self.register.HL(),
          };
          self.register.PC = new_addr;
          false
        } else {
          true
        }
      },
      OPCode::Return { cond, add_cycles } => {
        let cond_is_met = if let Some(c) = cond {
          c.test(&self.register.F)
        } else {
          true
        };
        if cond_is_met {
          cycles += add_cycles;
          self.register.PC = pop_stack(&mut self.register, &self.memory);
          false
        } else {
          true
        }
      },
      OPCode::ReturnInterrupt { add_cycles } => true, //TODO
      OPCode::Call { cond, add_cycles, target } => {
        let cond_is_met = if let Some(c) = cond {
          c.test(&self.register.F)
        } else {
          true
        };
        if cond_is_met {
          cycles += add_cycles;
          let new_addr = match target {
            PtrTarget::Offset(offset) => self.register.PC + offset as u16 + length as u16,
            PtrTarget::Ptr(addr) => addr,
            PtrTarget::DerefHL => self.register.HL(),
          };
          let pc = self.register.PC;
          push_stack(&mut self.register, &mut self.memory, pc);
          self.register.PC = new_addr;
          false
        } else {
          true
        }
      },
      OPCode::Rotate { dir, with_carry } => {
        let carry;
        let var = &mut self.register.A;
        match (dir, with_carry) {
          (ShiftDir::Left, true) => {
            let lsb = if self.register.F.contains(FlagRegister::C) {1} else {0};
            carry = *var & 0x80 != 0;
            *var = *var << 1 | lsb;
          },
          (ShiftDir::Left, false) => {
            carry = *var & 0x80 != 0;
            *var = var.rotate_left(1);
          },
          (ShiftDir::Right, true) => {
            let msb = if self.register.F.contains(FlagRegister::C) {0x80} else {0};
            carry = *var & 1 != 0;
            *var = *var >> 1 | msb;
          },
          (ShiftDir::Right, false) => {
            carry = *var & 1 != 0;
            *var = var.rotate_right(1);
          },
        }
        self.register.F.set_flags(Some(false), Some(false), Some(false), Some(carry));
        true
      },
      OPCode::Pop(reg) => {
        let val = pop_stack(&mut self.register, &self.memory);
        let dst = reg.get_mut(&mut self.register, &mut self.memory);
        *dst = val;
        true
      },
      OPCode::Push(reg) => {
        let src = reg.get(&self.register, &self.memory);
        push_stack(&mut self.register, &mut self.memory, src);
        true
      },
      OPCode::DisableInterrupts => true, //TODO
      OPCode::EnableInterrupts => true, //TODO
      OPCode::Unsupported => return CPUResult::UnsupportedOP,
    };
    if auto_pc_inc {
      self.register.PC += length as u16;
    }
    CPUResult::Wait(cycles)
  }

  pub fn do_cb_instruction(&mut self, opcode: CBPrefixedOPCode) {
    use opcodes::*;
    match opcode {
      CBPrefixedOPCode::Rotate { dir, with_carry, var } => {
        let carry;
        let carry_is_set = self.register.F.contains(FlagRegister::C);
        let var = var.get_mut(&mut self.register, &mut self.memory);
        match (dir, with_carry) {
          (ShiftDir::Left, true) => {
            let lsb = if carry_is_set {1} else {0};
            carry = *var & 0x80 != 0;
            *var = *var << 1 | lsb;
          },
          (ShiftDir::Left, false) => {
            carry = *var & 0x80 != 0;
            *var = var.rotate_left(1);
          },
          (ShiftDir::Right, true) => {
            let msb = if carry_is_set {0x80} else {0};
            carry = *var & 1 != 0;
            *var = *var >> 1 | msb;
          },
          (ShiftDir::Right, false) => {
            carry = *var & 1 != 0;
            *var = var.rotate_right(1);
          },
        }
        let is_zero = *var == 0;
        self.register.F.set_flags(Some(is_zero), Some(false), Some(false), Some(carry));
      },
      CBPrefixedOPCode::Shift(dir, var) => {
        let carry;
        let var = var.get_mut(&mut self.register, &mut self.memory);
        match dir {
          ShiftDir::Left => {
            carry = *var & 0x80 != 0;
            *var <<= 1;
          },
          ShiftDir::Right => {
            carry = *var & 1 != 0;
            *var >>= 1;
          },
        }
        let is_zero = *var == 0;
        self.register.F.set_flags(Some(is_zero), Some(false), Some(false), Some(carry));
      },
      CBPrefixedOPCode::ShiftRightKeepMSB(var) => {
        let var = var.get_mut(&mut self.register, &mut self.memory);
        let carry = *var & 1 != 0;
        *var = (*var >> 1) | (*var & 0x80);
        let is_zero = *var == 0;
        self.register.F.set_flags(Some(is_zero), Some(false), Some(false), Some(carry));
      },
      CBPrefixedOPCode::SwapNibbles(var) => {
        let var = var.get_mut(&mut self.register, &mut self.memory);
        let high_nibble = *var & 0b1111_0000;
        let low_nibble = *var & 0b0000_1111;
        *var = (high_nibble >> 4) | (low_nibble << 4);
        let is_zero = *var == 0;
        self.register.F.set_flags(Some(is_zero), Some(false), Some(false), Some(false));
      },
      CBPrefixedOPCode::BitMutation(bitmut, bit, var) => {
        let var = var.get_mut(&mut self.register, &mut self.memory);
        let bit_mask = bit as u8;
        match bitmut {
          BitMut::Set => *var |= bit_mask,
          BitMut::Unset => *var &= bit_mask ^ 0xFF,
          BitMut::Flip => Err(opcode).expect("Not actually a CB OpCode"),
          BitMut::Test => {
            let bit_is_zero = *var & bit_mask != 0;
            self.register.F.set_flags(Some(bit_is_zero), Some(false), Some(true), None);
          },
        }
      },
    }
  }
}


pub enum CPUResult {
  Wait(u8),
  UnsupportedOP,
  MemoryOutOfBounds,
}
