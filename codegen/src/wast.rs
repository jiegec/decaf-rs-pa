use common::{BinOp, IgnoreResult, UnOp};
use std::fmt;
use tac::{Operand, CallKind, Intrinsic};

pub type Reg = u32;
type Imm = i32;

pub enum AsmTemplate {
  Bin(BinOp, Reg, Operand, Operand),
  Un(UnOp, Reg, Operand),
  Mv(Reg, Operand),
  Param(Operand),
  CallStatic(Option<u32>, String),
  CallVirtual(Option<u32>, usize /* args len */, Operand),
  CallIntrinsic(Option<u32>, Intrinsic),
  Lw(Reg /* dst */, Operand /* base */, Imm),
  Sw(Operand /* src */, Operand /* base */, Imm),
  La(Reg, String),
  Label(String),
  Jmp(String, u32),
  Jif(String, u32, Reg, bool /* z */),
  Ret(Option<Operand>),
}

impl fmt::Debug for AsmTemplate {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    use AsmTemplate::*;
    match self {
      Bin(op, w1, r1, r2) => write!(f, "(set_local {} ({} {} {}))", w1, bin_str(op), operand_str(r1), operand_str(r2)),
      Un(op, w1, r1) => write!(f, "(set_local {} ({} {}))", w1, un_str(op), operand_str(r1)),
      Mv(w1, r1) => write!(f, "(set_local {} {})", w1, operand_str(r1)),
      Param(r1) => write!(f, "{}", operand_str(r1)),
      CallStatic(dst, fun) => {
        if let Some(dst) = dst {
          write!(f, "(set_local {} (call ${}))", dst, fun)
        } else {
          write!(f, "(drop (call ${}))", fun)
        }
      },
      CallVirtual(dst, args_len, fun) => {
        let mut signature = String::from("(param");
        for _ in 0..*args_len {
          signature.push_str(" i32");
        }
        signature.push_str(") (result i32)");
        if let Some(dst) = dst {
          write!(f, "(set_local {} (call_indirect {} {}))", dst, signature, operand_str(fun))
        } else {
          write!(f, "(drop (call_indirect {} {}))", signature, operand_str(fun))
        }
      },
      Lw(dst, base, imm) => write!(f, "(set_local {} (i32.load (i32.add {} (i32.const {}))))", dst, operand_str(base), imm),
      Sw(src, base, imm) => write!(f, "(i32.store (i32.add {} (i32.const {})) {})", operand_str(base), imm, operand_str(src)),
      La(dst, addr) => write!(f, "(set_local {} (call ${}))", dst, addr),
      Ret(ret) => match ret {
        Some(op) => {
          write!(f, "(return {})", operand_str(op))
        }
        None => write!(f, "(return (i32.const 0))")
      },
      Label(label) => {
        write!(f, ") ;; label {}", label)
      }
      Jmp(t, l) => write!(f, "(set_global $trampoline (i32.const {})) (br ${}) ;; Jump to L{}", l, t, l),
      Jif(t, l, cond, z) => if *z {
        write!(f, "(set_global $trampoline (i32.const {})) (br_if ${} (i32.eq (get_local {}) (i32.const 0))) ;; Jump if T{} == 0 to L{}", l, t, cond, cond, l)
      } else {
        write!(f, "(set_global $trampoline (i32.const {})) (br_if ${} (get_local {})) ;; Jump to T{} != 0 L{}", l, t, cond, cond, l)
      }
      _ => Ok(())
    }
  }
}

pub fn operand_str(operand: &Operand) -> String {
  match operand {
    Operand::Const(i) => format!("(i32.const {})", i),
    Operand::Reg(i) => format!("(get_local {})", i),
  }
}

pub fn bin_str(op: &BinOp) -> &'static str {
  match op {
    BinOp::Add => "i32.add",
    BinOp::Sub => "i32.sub",
    BinOp::Mul => "i32.mul",
    BinOp::Div => "i32.div_s",
    BinOp::Mod => "i32.mod",
    BinOp::And => "i32.and",
    BinOp::Or => "i32.or",
    BinOp::Eq => "i32.eq",
    BinOp::Ne => "i32.ne",
    BinOp::Lt => "i32.lt_s",
    BinOp::Le => "i32.le_s",
    BinOp::Gt => "i32.gt_s",
    BinOp::Ge => "i32.ge_s",
  }
}

pub fn un_str(op: &UnOp) -> &'static str {
  match op {
    UnOp::Neg => "i32.sub (i32.const 0)",
    UnOp::Not => "i32.not",
  }
}