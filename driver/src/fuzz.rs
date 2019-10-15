use std::io;
use ebnf_gen::Generate;

fn main() -> io::Result<()> {
    loop {
        let code = include_str!("../decaf-2019.ebnf");
        let alloc = ebnf_gen::ASTAlloc::default();
        let ebnf = ebnf_gen::work(code, &alloc);
        if let Ok(ebnf) = ebnf {
            let gen = ebnf.generate(&ebnf);
            let pa = driver::Pa::Pa1a;
            let alloc = driver::Alloc::default();
            let result = driver::compile(&gen, &alloc, pa.to_cfg());
            if result.is_err() {
                println!("Generated: {}", gen);
                println!("Compiled: {:?}", result);
                return Ok(());
            }
        }
    }
}