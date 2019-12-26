use common::{IndentPrinter, IgnoreResult};
use tac::{TacProgram, TacFunc};
use codegen::wast::AsmTemplate;
use std::fmt::Write;

fn to_wasm_int(num: usize) -> String {
  format!("\"\\{:02X}\\{:02X}\\{:02X}\\{:02X}\"", (num) & 0xFF, (num >> 8) & 0xFF, (num >> 16) & 0xFF, (num >> 24) & 0xFF)
}

fn to_wasm_string(s: &str) -> String {
  let mut result = String::new();
  result.push_str("\"");
  for byte in s.as_bytes() {
    if *byte >= 0x20 && *byte <= 0x7e {
      result.push(*byte as char);
    } else {
      result.push_str(&format!("\\{:02X}", byte));
    }
  }
  result.push_str("\\00\"");
  result
}

fn wasm_string_len(s: &str) -> usize {
  ((s.len() + 1) + 3) & !3
}

pub fn data(pr: &TacProgram, p: &mut IndentPrinter) {
  write!(p, "(module").ignore();
  p.inc();
  write!(p, "(import \"wasi_unstable\" \"fd_write\" (func $fd_write (param i32 i32 i32 i32) (result i32)))").ignore();
  write!(p, "(memory 128)").ignore();
  write!(p, "(global $trampoline (mut i32) (i32.const 0)) ;; Trampoline").ignore();
  write!(p, "(export \"memory\" (memory 0))").ignore();

  let mut offsets = Vec::new();
  let mut offset = 4;
  // calculate vtable offsets
  for v in &pr.vtbl {
    let (_, name) = pr.str_pool.get_full(v.class).expect("tacgen should have put class name into `str_pool`");
    let size = 4 + 4 + v.func.len() * 4 + wasm_string_len(name);
    offsets.push(offset);
    offset += size;
  }
  // calculate string offsets
  for (_idx, s) in pr.str_pool.iter().enumerate() {
    offset += wasm_string_len(s);
  }
  // Total static data extent
  let extent = offset;
  let mut table_offset = 0;
  offset = 0;
  write!(p, ";; Memory extent").ignore();
  write!(p, "(data (i32.const {}) {})", offset, to_wasm_int(extent)).ignore();
  writeln!(p).ignore();
  offset += 4;

  // VTable
  for v in &pr.vtbl {
    write!(p, ";; VTBL({})", v.class).ignore();
    // VTbl symbol as a func
    write!(p, "(func $_{} (result i32)", v.class).ignore();
    p.indent(|p| write!(p, "(i32.const {}))", offset).ignore());

    // parent
    if let Some(pa) = v.parent {
      write!(p, "(data (i32.const {}) {}) ;; Parent", offset, to_wasm_int(offsets[pa as usize])).ignore();
    } else {
      write!(p, "(data (i32.const {}) {}) ;; Parent", offset, to_wasm_int(0)).ignore();
    }
    offset += 4;

    // name ptr
    write!(p, "(data (i32.const {}) {}) ;; Name Addr", offset, to_wasm_int(offset + 4 + v.func.len() * 4)).ignore();
    offset += 4;

    // funcs
    for &f in &v.func {
      write!(p, "(data (i32.const {}) {}) ;; Table Offset for {:?}", offset, to_wasm_int(table_offset), f).ignore();
      table_offset += 1;
      offset += 4;
    }

    // name
    let (_, name) = pr.str_pool.get_full(v.class).expect("tacgen should have put class name into `str_pool`");
    write!(p, "(data (i32.const {}) {}) ;; Name", offset, to_wasm_string(name)).ignore();
    offset += wasm_string_len(name);
    writeln!(p).ignore();
  }
  writeln!(p).ignore();

  for (idx, s) in pr.str_pool.iter().enumerate() {
    write!(p, "(func $_STRING{} (result i32)", idx).ignore();
    p.indent(|p| write!(p, "(i32.const {}))", offset).ignore());
    write!(p, "(data (i32.const {}) {})", offset, to_wasm_string(s)).ignore();
    offset += wasm_string_len(s);
  }
  writeln!(p).ignore();

  // VTable Functions
  write!(p, "(table funcref (elem").ignore();
  p.indent(|p| {
    for v in &pr.vtbl {
      for &f in &v.func {
        write!(p, "${}", pr.func[f as usize].name).ignore();
      }
    }
    for f in &pr.func {
      write!(p, "${}", f.name).ignore();
    }
  });
  write!(p, "))").ignore();

  // All functions
  for f in &pr.func {
    write!(p, "(func $_FUNC{} (result i32)", f.name).ignore();
    p.indent(|p| write!(p, "(i32.const {}))", table_offset).ignore());
    table_offset += 1;
  }
  
  writeln!(p).ignore();
}

pub fn func(f: &(usize, Vec<AsmTemplate>), name: &str, p: &mut IndentPrinter, fun: &TacFunc) {
  let (bb_count, f) = f;

  let mut func_declaration = format!("(func ${} (", name);
  if fun.param_num > 0 {
    func_declaration.push_str("param");
    for _ in 0..fun.param_num {
      func_declaration.push_str(" i32");
    }
    func_declaration.push_str(") (");
  }
  func_declaration.push_str("result i32)");
  write!(p, "{}", func_declaration).ignore();
  p.indent(|p| {
    if fun.reg_num > 0 {
      let mut locals = String::new();
      locals.push_str("(local");
      // TODO
      for _ in 0..256 {
        locals.push_str(" i32");
      }
      locals.push_str(")");
      write!(p, "{}", locals).ignore();
    }

    write!(p, "(set_global $trampoline (i32.const 0))").ignore();
    if *bb_count > 0 {
      write!(p, "(loop ${}_T", name).ignore();
      p.inc();
      for i in (0..*bb_count).rev() {
        write!(p, "(block ${}_L{}", name, i).ignore();
      }
      p.indent(|p| {
        let mut table = String::new();
        table.push_str("(br_table");
        for i in 0..*bb_count {
          table.push_str(&format!(" ${}_L{}", name, i));
        }
        write!(p, "{} (get_global $trampoline))", table).ignore();
        write!(p, ") ;; label ${:?}_L0", name).ignore();
      });
      p.inc();
    }

    for asm in f {
      write!(p, "{:?}", asm).ignore();
    }

    if *bb_count > 0 {
      p.dec();
      write!(p, ")").ignore();
      p.dec();
    }
    write!(p, "(unreachable))").ignore();
  });
  writeln!(p).ignore();
}
