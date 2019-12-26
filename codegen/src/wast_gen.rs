use crate::{graph_alloc::*, wast::{*, Reg}, AllocMethod};
use tacopt::{bb::{FuncBB, NextKind}, flow::{Flow, Or, FlowElem}};
use tac::{Tac, TacProgram, Operand, CallKind, Intrinsic};
use common::{HashSet, HashMap, BinOp};
use bitset::traits::*;

pub struct FuncGen<'a, 'b> {
  pub(crate) param_num: u32,
  pub(crate) max_reg: u32,
  // for functions that this function calls
  pub(crate) max_param: u32,
  pub(crate) name: &'b str,
  pub(crate) program: &'b TacProgram<'a>,
  // we do need to insert in the SomeContainer<AsmTemplate>, but rust's LinkedList's api is so limited
  // and we do not need arbitrary insertion/deletion, so a Vec will be enough
  pub(crate) bb: Vec<(Vec<AsmTemplate>, [Option<u32>; 2])>,
}

impl<'a: 'b, 'b> FuncGen<'a, 'b> {
  pub fn work(f: &FuncBB<'a>, p: &'b TacProgram<'a>, m: AllocMethod) -> (usize, Vec<AsmTemplate>) {
    let mut fu = FuncGen { param_num: f.param_num, max_reg: f.reg_num, max_param: 0, name: &f.name, program: p, bb: Vec::new() };
    fu.populate(f);

    (fu.bb.len(), fu.bb.into_iter()
      .flat_map(|(b, _)| b.into_iter())
      .collect())
  }

  fn populate(&mut self, f: &FuncBB<'a>) {
    for (idx, b1) in f.bb.iter().enumerate() {
      let mut b2 = Vec::new();
      if idx > 0 {
        b2.push(AsmTemplate::Label(format!("{:?}_L{}:", self.name, idx)));
      }
      let mut arg_cnt = 0;
      for t in b1.iter() {
        self.select_inst(t.tac.get(), &mut b2, &mut arg_cnt);
      }
      // generate ret/jmp/..., and return the `next` by the way
      self.build_next(idx as u32, f.bb.len() as u32 + 1, b1.next, &mut b2);
      self.bb.push((b2, [None, None]));
    }
  }
}

impl FuncGen<'_, '_> {
  fn select_inst(&mut self, t: Tac, b: &mut Vec<AsmTemplate>, arg_cnt: &mut u32) {
    use AsmTemplate::*;
    match t {
      Tac::Bin { op, dst, lr } => {
        match lr {
          [l, r] => b.push(Bin(op, dst, l, r))
        }
      }
      Tac::Un { op, dst, r } => b.push(Un(op, dst, r[0])),
      Tac::Assign { dst, src } => b.push(Mv(dst, src[0])),
      Tac::Param { src } => {
        b.push(Param(src[0]));
      }
      Tac::Call { dst, kind } => {
        match kind {
          CallKind::Virtual(r, args_len, _) => {
            b.push(CallVirtual(dst, args_len, r[0]));
          }
          CallKind::Static(f, _) => {
            b.push(CallStatic(dst, format!("{:?}", self.program.func[f as usize].name)));
          }
          CallKind::Intrinsic(i)  => {
            b.push(CallStatic(dst, format!("{:?}", i)));
          }
        }
      }
      Tac::Load { dst, base, off, .. } => {
        b.push(Lw(dst, base[0], off));
      }
      Tac::Store { src_base, off, .. } => {
        b.push(Sw(src_base[0], src_base[1], off));
      }
      Tac::LoadStr { dst, s } => b.push(La(dst, format!("_STRING{}", s))),
      Tac::LoadVTbl { dst, v } => b.push(La(dst, format!("_{}", self.program.vtbl[v as usize].class))),
      Tac::LoadFunc { dst, f } => b.push(La(dst, self.program.func[f as usize].name.clone())),
      Tac::Label { .. } | Tac::Ret { .. } | Tac::Jmp { .. } | Tac::Jif { .. } => unreachable!("Shouldn't meet Ret/Jmp/Jif/Label in a tac bb."),
    }
  }

  // epilogue is the index of epilogue bb
  // note that all jump target should inc by 1, because prologue takes index 0
  fn build_next(&mut self, idx: u32, epilogue: u32, next: NextKind, b: &mut Vec<AsmTemplate>) -> [Option<u32>; 2] {
    match next {
      // turn ret into jmp to the last bb(epilogue)
      NextKind::Ret(src) => {
        b.push(AsmTemplate::Ret(src));
        [Some(epilogue), None]
      }
      NextKind::Jmp(jump) => {
        b.push(AsmTemplate::Jmp(format!("{:?}_T", self.name), jump));
        [Some(jump), None]
      }
      NextKind::Jif { cond, z, fail, jump } => {
        b.push(AsmTemplate::Jif(format!("{:?}_T", self.name), jump, cond, z));
        [Some(fail), Some(jump)]
      }
      NextKind::Halt => {
        [None, None]
      }
    }
  }
}