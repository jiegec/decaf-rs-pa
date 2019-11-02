use std::iter;
use common::Loc;
use syntax::{ScopeOwner, Symbol, ClassDef, Program};
use std::collections::HashMap;

pub(crate) struct ScopeStack<'a> {
  // `global` must be ScopeOwner::Global, but we will not depend on this, so just define it as ScopeOwner
  global: ScopeOwner<'a>,
  // ignore symbols whose loc >= item.1
  // for local var def
  stack: Vec<(ScopeOwner<'a>, Option<Loc>)>,
}

impl<'a> ScopeStack<'a> {
  pub fn new(p: &'a Program<'a>) -> Self {
    Self { global: ScopeOwner::Global(p), stack: vec![] }
  }

  pub fn lookup(&self, name: &'a str) -> Option<(Symbol<'a>, ScopeOwner<'a>)> {
    self.stack.iter().rev().chain(iter::once(&(self.global, None)))
        .filter_map(|&(owner, _loc)| owner.scope().get(name).map(|&sym| (sym, owner)))
        .next()
  }

  // do lookup, but will ignore those local symbols whose loc >= item.1
  // returns (symbol, across_lambda)
  pub fn lookup_var_sel(&self, name: &'a str, loc: Loc) -> (Option<Symbol<'a>>, bool) {
    let mut across_lambda = false;
    for (owner, def_loc) in self.stack.iter().rev().chain(iter::once(&(self.global, None))) {
      if let Some(sym) = owner.scope().get(name).cloned() {
        if owner.is_local() && sym.loc() >= loc {
          continue;
        }
        if let Some(def) = def_loc {
          if def <= &sym.loc() {
            continue;
          }
        }
        return (Some(sym), across_lambda);
      }
      if owner.is_lambda() {
        across_lambda = true;
      }
    }
    (None, across_lambda)
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
    self.stack.push((owner, None));
  }

  // the global scope is not affected
  pub fn close(&mut self) {
    let (owner, _loc) = self.stack.pop().unwrap();
    if let ScopeOwner::Class(_) = owner {
      self.stack.clear();
    }
  }

  pub fn cur_owner(&self) -> ScopeOwner<'a> {
    *self.stack.last().map(|t| &t.0).unwrap_or(&self.global)
  }

  pub fn lookup_class(&self, name: &'a str) -> Option<&'a ClassDef<'a>> {
    self.global.scope().get(name).map(|class| match class {
      Symbol::Class(c) => *c,
      _ => unreachable!("Global scope should only contain classes."),
    })
  }

  // Return a map of (function_name, abstract_)
  pub fn collect_member_fun(&self) -> HashMap<&'a str, bool> {
    let mut res = HashMap::new();
    for (owner, _loc) in self.stack.iter() {
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

  pub fn enter_var_def(&mut self, loc: Loc) {
    if let Some((_owner, def_loc)) = self.stack.last_mut() {
      *def_loc = Some(loc);
    }
  }

  pub fn end_var_def(&mut self) {
    if let Some((_owner, def_loc)) = self.stack.last_mut() {
      *def_loc = None;
    }
  }
}