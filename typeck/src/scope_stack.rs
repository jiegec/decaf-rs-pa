use std::iter;
use common::Loc;
use syntax::{ScopeOwner, Symbol, ClassDef, Program};
use std::collections::HashMap;

pub(crate) struct ScopeStack<'a> {
  // `global` must be ScopeOwner::Global, but we will not depend on this, so just define it as ScopeOwner
  global: ScopeOwner<'a>,
  stack: Vec<ScopeOwner<'a>>,
}

impl<'a> ScopeStack<'a> {
  pub fn new(p: &'a Program<'a>) -> Self {
    Self { global: ScopeOwner::Global(p), stack: vec![] }
  }

  pub fn lookup(&self, name: &'a str) -> Option<(Symbol<'a>, ScopeOwner<'a>)> {
    self.stack.iter().rev().chain(iter::once(&self.global))
        .filter_map(|&owner| owner.scope().get(name).map(|&sym| (sym, owner)))
        .next()
  }

  // Return a map of (function_name, abstract_)
  pub fn collect_member_fun(&self) -> HashMap<&'a str, bool> {
    let mut res = HashMap::new();
    for owner in self.stack.iter() {
      for (_, symbol) in owner.scope().iter() {
        if let Symbol::Func(func) = symbol {
          if !func.static_ {
            res.insert(func.name, func.abstract_);
          }
        }
      }
    }
    res
  }

  // do lookup, but will ignore those local symbols whose loc >= the given loc
  pub fn lookup_before(&self, name: &'a str, loc: Loc) -> Option<Symbol<'a>> {
    self.stack.iter().rev().chain(iter::once(&self.global))
      .filter_map(|&owner| owner.scope().get(name).cloned().filter(|sym| !(owner.is_local() && sym.loc() >= loc)))
      .next()
  }

  pub fn declare(&mut self, sym: Symbol<'a>) {
    self.cur_owner().scope_mut().insert(sym.name(), sym);
  }

  // if `owner` is ScopeOwner::Class, then will recursively open all its ancestors
  pub fn open(&mut self, owner: ScopeOwner<'a>) {
    if let ScopeOwner::Class(c) = owner {
      if let Some(p) = c.parent_ref.get() {
        self.open(ScopeOwner::Class(p));
      }
    }
    self.stack.push(owner);
  }

  // the global scope is not affected
  pub fn close(&mut self) {
    let owner = self.stack.pop().unwrap();
    if let ScopeOwner::Class(_) = owner {
      self.stack.clear();
    }
  }

  pub fn cur_owner(&self) -> ScopeOwner<'a> {
    *self.stack.last().unwrap_or(&self.global)
  }

  pub fn lookup_class(&self, name: &'a str) -> Option<&'a ClassDef<'a>> {
    self.global.scope().get(name).map(|class| match class {
      Symbol::Class(c) => *c,
      _ => unreachable!("Global scope should only contain classes."),
    })
  }
}