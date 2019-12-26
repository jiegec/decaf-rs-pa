use driver::{Pa, Alloc};
use clap::{Arg, App};
use std::{io, fs, process, io::Write};

fn main() -> io::Result<()> {
  let matches = App::new("decaf")
    .arg(Arg::with_name("input").required(true))
    .arg(Arg::with_name("output").long("output").short("o").takes_value(true))
    .arg(Arg::with_name("target").long("target").short("t").takes_value(true).default_value("pa5"))
    .get_matches();
  let pa = match matches.value_of("target").unwrap() {
    t if t.eq_ignore_ascii_case("pa1a") => Pa::Pa1a,
    t if t.eq_ignore_ascii_case("pa1b") => Pa::Pa1b,
    t if t.eq_ignore_ascii_case("pa2") => Pa::Pa2,
    t if t.eq_ignore_ascii_case("pa3") => Pa::Pa3,
    t if t.eq_ignore_ascii_case("pa4") => Pa::Pa4,
    t if t.eq_ignore_ascii_case("pa5") => Pa::Pa5,
    t if t.eq_ignore_ascii_case("pa5wast") => Pa::Pa5Wast,
    t if t.eq_ignore_ascii_case("pa5wasm") => Pa::Pa5Wast,
    t => {
      eprintln!("invalid target pa: `{}`", t);
      process::exit(1);
    }
  };
  let wasm = match matches.value_of("target").unwrap() {
    t if t.eq_ignore_ascii_case("pa5wasm") => true,
    _ => false,
  };
  let input = matches.value_of("input").unwrap();
  let result = match driver::compile(&fs::read_to_string(input)?, &Alloc::default(), pa.to_cfg()) {
    Ok(p) => p,
    Err(e) => format!("{:?}", e),
  };
  let result = if wasm {
    wat::parse_str(result).unwrap()
  } else {
    result.into_bytes()
  };
  if let Some(output) = matches.value_of("output") {
    fs::write(output, result)
  } else {
    io::stdout().write_all(&result)?;
    Ok(())
  }
}