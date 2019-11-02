mod scope_stack;
mod symbol_pass;
mod type_pass;

use common::{Errors, ErrorKind::*, Ref};
use syntax::{ClassDef, SynTy, SynTyKind, ScopeOwner, Ty, TyKind, Program, VarDef};
use typed_arena::Arena;
use std::ops::{Deref, DerefMut};
use crate::{symbol_pass::SymbolPass, type_pass::TypePass, scope_stack::ScopeStack};

// if you want to alloc other types, you can add them to TypeCkAlloc
#[derive(Default)]
pub struct TypeCkAlloc<'a> {
  pub ty: Arena<Ty<'a>>,
}

#[derive(Clone)]
struct FuncInfo<'a> {
  pub name: &'a str,
  pub static_: bool,
  pub ret_ty: Ty<'a>
}

pub fn work<'a>(p: &'a Program<'a>, alloc: &'a TypeCkAlloc<'a>) -> Result<(), Errors<'a, Ty<'a>>> {
  let mut s = SymbolPass(TypeCk { errors: Errors(vec![]), scopes: ScopeStack::new(p), loop_cnt: 0, cur_used: false, cur_func_info: None, cur_class: None, cur_var_def: None, alloc });
  s.program(p);
  if !s.errors.0.is_empty() { return Err(s.0.errors.sorted()); }
  let mut t = TypePass(s.0);
  t.program(p);
  if !t.errors.0.is_empty() { return Err(t.0.errors.sorted()); }
  Ok(())
}

struct TypeCk<'a> {
  errors: Errors<'a, Ty<'a>>,
  scopes: ScopeStack<'a>,
  loop_cnt: u32,
  // `cur_used` is only used to determine 2 kinds of errors:
  // Class.var (cur_used == true) => BadFieldAssess; Class (cur_used == false) => UndeclaredVar
  cur_used: bool,
  cur_func_info: Option<FuncInfo<'a>>,
  cur_class: Option<&'a ClassDef<'a>>,
  // actually only use cur_var_def's loc
  // if cur_var_def is Some, wil use it's loc to search for symbol in TypePass::var_sel
  // this can reject code like `int a = a;`
  cur_var_def: Option<&'a VarDef<'a>>,
  alloc: &'a TypeCkAlloc<'a>,
}


impl<'a> TypeCk<'a> {
  // is_arr can be helpful if you want the type of array while only having its element type (to avoid cloning other fields)
  fn ty(&mut self, s: &SynTy<'a>, is_arr: bool) -> Ty<'a> {
    let mut void_arg_pos = None;
    let kind = match &s.kind {
      SynTyKind::Int => TyKind::Int,
      SynTyKind::Bool => TyKind::Bool,
      SynTyKind::String => TyKind::String,
      SynTyKind::Void => TyKind::Void,
      SynTyKind::Var => TyKind::Var,
      SynTyKind::Function => {
        if let Some(func_ty) = &s.function_type {
          let (ret_ty, param_ty) : &(_, _)= &func_ty;
          let mut ret_param_ty = Vec::new();
          ret_param_ty.push(self.ty(ret_ty, false));
          for ty in param_ty.iter() {
            if ty.kind == SynTyKind::Void {
              void_arg_pos = Some(ty.loc);
              break;
            }
            ret_param_ty.push(self.ty(ty, false));
          }

          let ret_param_ty = self.alloc.ty.alloc_extend(ret_param_ty.into_iter());
          TyKind::Func(ret_param_ty)
        } else {
          TyKind::Error
        }
      },
      SynTyKind::Named(name) => if let Some(c) = self.scopes.lookup_class(name) {
        TyKind::Object(Ref(c))
      } else { self.issue(s.loc, NoSuchClass(name)) },
    };
    match kind {
      TyKind::Error => Ty::error(),
      TyKind::Func(_) if void_arg_pos.is_some() => self.issue(void_arg_pos.unwrap(), VoidInFuncTypeArg),
      TyKind::Void if s.arr != 0 => self.issue(s.loc, VoidArrayElement),
      _ => Ty { arr: s.arr + (is_arr as u32), kind }
    }
  }
}

impl<'a> Deref for TypeCk<'a> {
  type Target = Errors<'a, Ty<'a>>;
  fn deref(&self) -> &Self::Target { &self.errors }
}

impl<'a> DerefMut for TypeCk<'a> {
  fn deref_mut(&mut self) -> &mut Self::Target { &mut self.errors }
}

trait TypeCkTrait<'a> {
  fn scoped<F: FnMut(&mut Self) -> R, R>(&mut self, s: ScopeOwner<'a>, f: F) -> R;
}

impl<'a, T: std::ops::DerefMut<Target=TypeCk<'a>>> TypeCkTrait<'a> for T {
  fn scoped<F: FnMut(&mut Self) -> R, R>(&mut self, s: ScopeOwner<'a>, mut f: F) -> R {
    self.deref_mut().scopes.open(s);
    let ret = f(self);
    self.deref_mut().scopes.close();
    ret
  }
}
