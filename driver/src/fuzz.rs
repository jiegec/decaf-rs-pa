use std::io;
use std::fs;
use ebnf_tools::Generate;

fn main() -> io::Result<()> {
    loop {
        let code = include_str!("../decaf-2019.ebnf");
        let alloc = ebnf_tools::ASTAlloc::default();
        let ebnf = ebnf_tools::work(code, &alloc);
        if let Ok(ebnf) = ebnf {
            let gen = ebnf.generate(&ebnf, 20);
            let pa = driver::Pa::Pa1b;
            let alloc = driver::Alloc::default();
            let result = driver::compile(&gen, &alloc, pa.to_cfg());
            if result.is_err() {
                println!("Generated: {}", gen);
                fs::write("fuzz.decaf", gen.clone())?;
                println!("Compiled: {:?}", result);
                return Ok(());
            }
        }
    }
}
