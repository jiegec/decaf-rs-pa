use crate::{TypeCk, FuncInfo, TypeCkTrait};
use common::{ErrorKind::*, Loc, LENGTH, BinOp, UnOp, ErrorKind, Ref};
use syntax::ast::*;
use syntax::{ScopeOwner, Symbol, ty::*};
use std::ops::{Deref, DerefMut};
use either::Either;
use std::iter;

pub(crate) struct TypePass<'a>(pub TypeCk<'a>);

impl<'a> Deref for TypePass<'a> {
  type Target = TypeCk<'a>;
  fn deref(&self) -> &Self::Target { &self.0 }
}

impl<'a> DerefMut for TypePass<'a> {
  fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
}

impl<'a> TypePass<'a> {
  pub fn program(&mut self, p: &'a Program<'a>) {
    for c in &p.class { self.class_def(c); }
  }

  fn class_def(&mut self, c: &'a ClassDef<'a>) {
    self.cur_class = Some(c);
    self.scoped(ScopeOwner::Class(c), |s| for f in &c.field {
      if let FieldDef::FuncDef(f) = f {
        s.cur_func_info = Some(FuncInfo {
          static_: f.static_,
          ret_ty: f.ret_ty(),
          name: f.name,
        });
        if let Some(body) = f.body.as_ref() {
          let t = s.scoped(ScopeOwner::Param(f), |s| s.block(body, false));
          if t.is_none() && f.ret_ty() != Ty::void() {
            s.issue(body.loc, ErrorKind::NoReturn)
          }
        }
      };
    });
  }

  fn block(&mut self, b: &'a Block<'a>, lambda: bool) -> Option<Ty<'a>> {
    let mut old = vec![];
    if lambda {
      old.append(&mut self.cur_return_ty);
    }
    let mut ret = false;
    self.scoped(ScopeOwner::Local(b), |s| for st in &b.stmt {
      ret = s.stmt(st);
    });
    if lambda {
      let res = if let Some(ty) = self.cur_return_ty.iter().next() {
        let mut cur_ty = *ty;
        for next_ty in self.cur_return_ty.iter().skip(1) {
          cur_ty = cur_ty.find_common(*next_ty, &self.alloc.ty, true);
        }
        if cur_ty.is_error() {
          self.issue(b.loc, ErrorKind::IncompatibleReturnTypeOfBlock)
        }
        Some(cur_ty)
      } else {
        // no return
        Some(Ty::void())
      };
      self.cur_return_ty = old;
      res
    } else {
      if ret {
        Some(Ty::void())
      } else {
        None
      }
    }
  }

  // return whether this stmt has returned
  fn stmt(&mut self, s: &'a Stmt<'a>) -> bool {
    match &s.kind {
      StmtKind::Assign(a) => {
        let old = self.cur_assign_loc;
        self.cur_assign_loc = Some(a.loc);
        let (l, r) = (self.expr(&a.dst, true), self.expr(&a.src, false));
        if !r.assignable_to(l) {
          self.issue(s.loc, IncompatibleBinary { l, op: "=", r })
        }
        self.cur_assign_loc = old;
        false
      }
      StmtKind::LocalVarDef(v) => {
        self.scopes.enter_var_def(v.loc);
        if let Some((loc, e)) = &v.init {
          let (l, r) = (&v.ty, self.expr(e, false));
          if !r.assignable_to(l.get()) {
            self.issue(*loc, IncompatibleBinary { l: l.get(), op: "=", r })
          }
          // type deduction
          if l.get().is_var() {
            // cannot deduct void type
            if r == Ty::void() { self.issue(v.loc, VoidVar(v.name)) }
            l.set(r);
          }
        }
        self.scopes.end_var_def();
        false
      }
      StmtKind::ExprEval(e) => {
        self.expr(e, false);
        false
      }
      StmtKind::Skip(_) => false,
      StmtKind::If(i) => {
        self.check_bool(&i.cond);
        let s1 = self.block(&i.on_true, false);
        let s2 = if let Some(of) = &i.on_false { self.block(of, false) } else { None };
        s1.is_some() && s2.is_some()
      }
      StmtKind::While(w) => {
        self.check_bool(&w.cond);
        self.loop_cnt += 1;
        self.block(&w.body, false);
        self.loop_cnt -= 1;
        false
      }
      StmtKind::For(f) => self.scoped(ScopeOwner::Local(&f.body), |s| {
        s.stmt(&f.init);
        s.check_bool(&f.cond);
        s.stmt(&f.update);
        for st in &f.body.stmt { s.stmt(st); } // not calling block(), because the scope is already opened
        false
      }),
      StmtKind::Return(r) => {
        let expect = self.cur_func_info.as_ref().unwrap().ret_ty;
        let actual = if let Some(e) = r {
          self.expr(e, false)
        } else {
          Ty::void()
        };
        if !actual.assignable_to(expect) {
          self.issue(s.loc, ReturnMismatch { actual, expect })
        }
        self.cur_return_ty.push(Ty::void());
        true
      }
      StmtKind::Print(p) => {
        for (i, e) in p.iter().enumerate() {
          let ty = self.expr(e, false);
          if ty != Ty::error() && ty != Ty::bool() && ty != Ty::int() && ty != Ty::string() {
            self.issue(e.loc, BadPrintArg { loc: i as u32 + 1, ty })
          }
        }
        false
      }
      StmtKind::Break(_) => {
        if self.loop_cnt == 0 { self.issue(s.loc, BreakOutOfLoop) }
        false
      }
      StmtKind::Block(b) => self.block(b, false).is_some(),
    }
  }

  // e.ty is set to the return value; e.result is set if e can be statically evaluated
  fn expr(&mut self, e: &'a Expr<'a>, lvalue: bool) -> Ty<'a> {
    use ExprKind::*;
    let ty = match &e.kind {
      VarSel(v) => self.var_sel(v, e.loc, lvalue),
      IndexSel(i) => {
        let (arr, idx) = (self.expr(&i.arr, false), self.expr(&i.idx, false));
        if idx != Ty::int() && idx != Ty::error() {
          self.issue(e.loc, IndexNotInt)
        }
        match arr {
          Ty { arr, kind } if arr > 0 => Ty { arr: arr - 1, kind },
          e if e == Ty::error() => Ty::error(),
          _ => self.issue(i.arr.loc, IndexNotArray),
        }
      }
      IntLit(_) | ReadInt(_) => Ty::int(),
      BoolLit(_) => Ty::bool(),
      StringLit(_) | ReadLine(_) => Ty::string(),
      NullLit(_) => Ty::null(),
      Call(c) => self.call(c, e.loc),
      Unary(u) => self.unary(u, e.loc),
      Binary(b) => self.binary(b, e.loc),
      This(_) => if !self.cur_func_info.as_ref().unwrap().static_ {
        Ty::mk_obj(self.cur_class.unwrap())
      } else { self.issue(e.loc, ThisInStatic) }
      NewClass(n) => match self.scopes.lookup_class(n.name) {
        Some(c) => {
          if c.abstract_ {
            self.issue(e.loc, CannotInstantiateAbstractClass { class: c.name })
          }
          n.class.set(Some(c));
          Ty::mk_obj(c)
        }
        None => self.issue(e.loc, NoSuchClass(n.name)),
      },
      NewArray(n) => {
        let len = self.expr(&n.len, false);
        if len != Ty::int() && len != Ty::error() {
          self.issue(n.len.loc, NewArrayNotInt)
        }
        self.ty(&n.elem, true)
      }
      ClassTest(c) => {
        let src = self.expr(&c.expr, false);
        if src != Ty::error() && !src.is_object() {
          self.issue(e.loc, NotObject { owner: src })
        }
        match self.scopes.lookup_class(c.name) {
          Some(class) => {
            c.class.set(Some(class));
            Ty::bool()
          }
          None => self.issue(e.loc, NoSuchClass(c.name)),
        }
      }
      ClassCast(c) => {
        let src = self.expr(&c.expr, false);
        if src != Ty::error() && !src.is_object() {
          self.issue(e.loc, NotObject { owner: src })
        }
        match self.scopes.lookup_class(c.name) {
          Some(class) => {
            c.class.set(Some(class));
            Ty::mk_obj(class)
          }
          None => self.issue(e.loc, NoSuchClass(c.name)),
        }
      }
      Lambda(l) => self.scoped(ScopeOwner::Lambda(&l), |s| {
        // save upper function func info
        let old_cur_func_info = s.cur_func_info.replace(FuncInfo {
          name: "lambda",
          static_: false,
          ret_ty: Ty::var()
        });
        let ret_ty = match &l.body {
          Either::Left(expr) => {
            s.expr(&expr, false)
          },
          Either::Right(block) => {
            s.block(&block, true).unwrap_or(Ty::void())
          },
        };
        let ret_param_ty = iter::once(ret_ty)
          .chain(l.param.iter().map(|v| {
            v.ty.get()
          }));
        let ret_param_ty = s.alloc.ty.alloc_extend(ret_param_ty);
        // restore
        s.cur_func_info = old_cur_func_info;
        l.ret_param_ty.set(Some(ret_param_ty));
        Ty::mk_lambda(l)
      }),
    };
    e.ty.set(ty);
    ty
  }

  fn binary(&mut self, b: &'a Binary<'a>, loc: Loc) -> Ty<'a> {
    use BinOp::*;
    let (l, r) = (self.expr(&b.l, false), self.expr(&b.r, false));
    if l == Ty::error() || r == Ty::error() {
      match b.op {
        Add | Sub | Mul | Div | Mod => Ty::int(),
        And | Or | Eq | Ne | Lt | Le | Gt | Ge => Ty::bool(),
      }
    } else {
      let (ret, ok) = match b.op {
        Add | Sub | Mul | Div | Mod => (Ty::int(), l == Ty::int() && r == Ty::int()),
        Lt | Le | Gt | Ge => (Ty::bool(), l == Ty::int() && r == Ty::int()),
        Eq | Ne => (Ty::bool(), l.assignable_to(r) || r.assignable_to(l)),
        And | Or => (Ty::bool(), l == Ty::bool() && r == Ty::bool())
      };
      if !ok { self.errors.issue(loc, IncompatibleBinary { l, op: b.op.to_op_str(), r }) }
      ret
    }
  }

  fn unary(&mut self, u: &'a Unary<'a>, loc: Loc) -> Ty<'a> {
    let r = self.expr(&u.r, false);
    match u.op {
      UnOp::Neg => {
        if r != Ty::int() && r != Ty::error() { self.errors.issue(loc, IncompatibleUnary { op: "-", r }) }
        Ty::int()
      }
      UnOp::Not => {
        if r != Ty::bool() && r != Ty::error() { self.errors.issue(loc, IncompatibleUnary { op: "!", r }) }
        Ty::bool()
      }
    }
  }

  fn var_sel(&mut self, v: &'a VarSel<'a>, loc: Loc, lvalue: bool) -> Ty<'a> {
    // (no owner)not_found_var / ClassName<ns> / (no owner)method => UndeclaredVar
    // object.not_found_var => NoSuchField
    // (no owner)field_var && cur function is static => RefInStatic
    // <not object>.a (e.g.: Class.a, 1.a) / object.method => BadFieldAccess
    // object.field_var, where object's class is not self or any of ancestors => PrivateFieldAccess

    match &v.owner {
      Some(o) => {
        self.cur_used = true;
        let o_t = self.expr(o, false);
        self.cur_used = false;
        match o_t {
          Ty { arr: 0, kind: TyKind::Object(Ref(c)) } => match c.lookup(v.name) {
            Some(sym) => {
              match sym {
                Symbol::Var(var) => {
                  v.var.set(Some(var));
                  // only allow self & descendents to access field
                  if !self.cur_class.unwrap().extends(c) {
                    self.issue(loc, PrivateFieldAccess { name: v.name, owner: o_t })
                  }
                  var.ty.get()
                }
                _ => sym.ty(),
              }
            }
            None => self.issue(loc, NoSuchField { name: v.name, owner: o_t })
          }
          Ty { arr: 0, kind: TyKind::Class(Ref(c)) } => match c.lookup(v.name) {
            Some(sym) => {
              match sym {
                Symbol::Var(var) => {
                  v.var.set(Some(var));
                  // only allow self & descendents to access field
                  if !self.cur_class.unwrap().extends(c) {
                    self.issue(loc, PrivateFieldAccess { name: v.name, owner: o_t })
                  }
                  var.ty.get()
                }
                Symbol::Func(f) => {
                  let cur_func_info = self.cur_func_info.as_ref().unwrap();
                  if !f.static_ && cur_func_info.static_ {
                    self.issue(loc, BadFieldAccess { name: v.name, owner: o_t })
                  }
                  sym.ty()
                }
                _ => sym.ty(),
              }
            }
            None => self.issue(loc, NoSuchField { name: v.name, owner: o_t })
          }
          _ if o_t.is_arr() => {
            if v.name == LENGTH {
              let ty = iter::once(Ty::int());
              let ret_param_ty = self.alloc.ty.alloc_extend(ty);
              Ty {
                arr: 0,
                kind: TyKind::Func(ret_param_ty),
              }
            } else {
              self.issue(loc, NotCallableType { ty: o_t })
            }
          }
          e if e == Ty::error() => Ty::error(),
          _ => self.issue(loc, BadFieldAccess { name: v.name, owner: o_t }),
        }
      }
      None => {
        // if this expr is in an VarDef, it cannot access the variable that is being declared
        let (lookup, across_lambda) = self.scopes.lookup_var_sel(v.name, loc);
        let ret = match lookup {
          Some(sym) => match sym {
            Symbol::Var(var) => {
              v.var.set(Some(var));
              let var_owner = var.owner.get().unwrap();
              if var_owner.is_class() {
                let cur = self.cur_func_info.as_ref().unwrap();
                if cur.static_ {
                  let name = cur.name;
                  self.issue(loc, RefInStatic { field: v.name, func: name })
                }
              }
              if across_lambda && lvalue {
                // cannot assign value to captured variables
                let loc = self.cur_assign_loc.unwrap();
                self.issue(loc, AssignCapturedVarInLambda)
              }
              var.ty.get()
            }
            Symbol::Func(f) => {
              if let Some(_class) = f.class.get() {
                let cur = self.cur_func_info.as_ref().unwrap();
                if cur.static_ && !f.static_ {
                  let name = cur.name;
                  self.issue(loc, RefInStatic { field: v.name, func: name })
                }
              }
              Ty::mk_func(f)
            },
            Symbol::This(f) => Ty::mk_obj(f.class.get().unwrap()),
            Symbol::Class(c) => {
              if !self.cur_used {
                self.issue(loc, UndeclaredVar(v.name))
              } else { Ty::mk_class(c) }
            }
          }
          None => self.errors.issue(loc, UndeclaredVar(v.name)),
        };
        self.cur_used = false;
        ret
      }
    }
  }

  fn call(&mut self, c: &'a Call<'a>, loc: Loc) -> Ty<'a> {
    let ty = self.expr(&c.func, false);
    match ty.kind {
      TyKind::Func(f) => {
        match &c.func.kind {
          ExprKind::VarSel(v) => {
            self.check_arg_param(&c.arg, f, v.name, loc)
          }
          _ => self.check_arg_param(&c.arg, f, "lambda", loc)
        }
      }
      TyKind::Error => Ty::error(),
      _ => self.issue(loc, NotCallableType { ty })
    }
  }
}

impl<'a> TypePass<'a> {
  fn check_bool(&mut self, e: &'a Expr<'a>) {
    let ty = self.expr(e, false);
    if ty != Ty::bool() && ty != Ty::error() {
      self.issue(e.loc, TestNotBool)
    }
  }

  fn check_arg_param(&mut self, arg: &'a [Expr<'a>], ret_param: &[Ty<'a>], name: &'a str, loc: Loc) -> Ty<'a> {
    let (ret, param) = (ret_param[0], &ret_param[1..]);
    if param.len() != arg.len() {
      self.issue(loc, ArgcMismatch { name, expect: param.len() as u32, actual: arg.len() as u32 })
    }
    for (idx, arg0) in arg.iter().enumerate() {
      let arg = self.expr(arg0, false);
      if let Some(&param) = param.get(idx) {
        if !arg.assignable_to(param) {
          self.issue(arg0.loc, ArgMismatch { loc: idx as u32 + 1, arg, param })
        }
      }
    }
    ret
  }
}
