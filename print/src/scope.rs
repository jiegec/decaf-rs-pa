use common::{IndentPrinter, IgnoreResult};
use syntax::{ast::*, Scope};
use std::fmt::Write;
use either::Either;

fn show_scope(s: &Scope, p: &mut IndentPrinter) {
  let mut s = s.iter().map(|(_, sym)| *sym).collect::<Vec<_>>();
  s.sort_unstable_by_key(|x| x.loc());
  if s.is_empty() { write!(p, "<empty>").ignore(); } else { for s in s { write!(p, "{:?}", s).ignore(); } }
}

pub fn program(pr: &Program, p: &mut IndentPrinter) {
  write!(p, "GLOBAL SCOPE:").ignore();
  p.indent(|p| {
    show_scope(&pr.scope.borrow(), p);
    for c in &pr.class { class_def(c, p); }
  });
}

pub fn class_def(c: &ClassDef, p: &mut IndentPrinter) {
  write!(p, "CLASS SCOPE OF '{}':", c.name).ignore();
  p.indent(|p| {
    show_scope(&c.scope.borrow(), p);
    for f in &c.field {
      if let FieldDef::FuncDef(f) = f { func_def(f, p); }
    }
  });
}

pub fn func_def(f: &FuncDef, p: &mut IndentPrinter) {
  write!(p, "FORMAL SCOPE OF '{}':", f.name).ignore();
  p.indent(|p| {
    show_scope(&f.scope.borrow(), p);
    if let Some(body) = f.body.as_ref() {
      block(body, p);
    }
  });
}

pub fn block(b: &Block, p: &mut IndentPrinter) {
  write!(p, "LOCAL SCOPE:").ignore();
  p.indent(|p| {
    show_scope(&b.scope.borrow(), p);
    for s in &b.stmt {
      stmt(s, p);
    }
  });
}

pub fn stmt(s: &Stmt, p: &mut IndentPrinter) {
  match &s.kind {
    StmtKind::LocalVarDef(v) => {
      if let Some((_loc, e)) = &v.init {
        expr(e, p);
      }
    },
    StmtKind::If(i) => {
      block(&i.on_true, p);
      if let Some(on_false) = &i.on_false { block(on_false, p); }
    }
    StmtKind::While(w) => block(&w.body, p),
    StmtKind::For(f) => block(&f.body, p),
    StmtKind::Block(b) => block(b, p),
    _ => {}
  }
}

pub fn expr(e: &Expr, p: &mut IndentPrinter) {
  match &e.kind {
    ExprKind::Lambda(l) => {
      write!(p, "LOCAL SCOPE:").ignore();
      p.indent(|p| {
        show_scope(&l.scope.borrow(), p);
        match &l.body {
          Either::Left(e) => expr(e, p),
          Either::Right(b) => {
            show_scope(&b.scope.borrow(), p);
            for s in &b.stmt {
              stmt(s, p);
            }
          },
        }
      });
    }
    _ => {}
  }
}