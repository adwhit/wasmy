// Useful links:
// https://pengowray.github.io/wasm-ops/
// https://coinexsmartchain.medium.com/wasm-introduction-part-1-binary-format-57895d851580
// https://webassembly.github.io/spec/core/

use std::{fmt::Display, path::PathBuf};

use clap::Parser;

mod interpreter;
mod parser;

use parser::parse_wasm;

#[derive(Parser)]
struct Cli {
    file: PathBuf,
}

#[derive(Debug, Default)]
pub struct Binary {
    pub version: u32,
    pub types: Vec<FuncSig>,
    pub functions: Vec<u32>,
    pub memory: Vec<Limits>,
    pub global: Vec<Global>,
    pub export: Vec<Export>,
    pub code: Vec<Code>,
    pub data: Vec<Data>,
    pub names: Names,
}

impl Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Binary:")?;
        writeln!(f, "  Types:")?;
        for typ in &self.types {
            writeln!(f, "    {typ:?}")?;
        }
        writeln!(f, "  Functions:")?;
        for func in &self.functions {
            writeln!(f, "    {func:?}")?;
        }
        writeln!(f, "  Memory:")?;
        for m in &self.memory {
            writeln!(f, "    {m:?}")?;
        }
        writeln!(f, "  Globals:")?;
        for g in &self.global {
            writeln!(f, "    {g:?}")?;
        }
        writeln!(f, "  Exports:")?;
        for e in &self.export {
            writeln!(f, "    {e:?}")?;
        }
        writeln!(f, "  Code:")?;
        for c in &self.code {
            writeln!(f, "    locals: {:?}", c.locals)?;
            writeln!(f, "    code:")?;
            for i in &c.code {
                writeln!(f, "      {i:?}")?;
            }
        }
        writeln!(f, "  Data:")?;
        for d in &self.data {
            writeln!(f, "    {d:?}")?;
        }
        writeln!(f, "  Names:")?;
        for (ix, n) in &self.names.func_names {
            writeln!(f, "    {ix}: {n}")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct FuncSig {
    pub params: Vec<Value>,
    pub results: Vec<Value>,
}

#[derive(Debug)]
pub struct Limits {
    pub min: u64,
    pub max: Option<u64>,
}

#[derive(Debug)]
pub struct Global {
    pub typ: Value,
    pub mutable: Mutability,
    pub expr: Vec<Instruction>,
}

#[derive(Debug, num_derive::FromPrimitive, num_derive::ToPrimitive)]
pub enum ExportType {
    Func = 0,
    Table = 1,
    Mem = 2,
    Global = 3,
}

#[derive(Debug)]
pub struct Export {
    pub name: String,
    pub tag: ExportType,
    pub index: u32,
}

#[derive(Debug)]
pub struct Code {
    pub locals: Vec<(u32, Value)>,
    pub code: Vec<Instruction>,
}

#[derive(Debug, num_derive::FromPrimitive, num_derive::ToPrimitive)]
pub enum Value {
    None = 0x40,
    I32 = 0x7f,
    I64 = 0x7e,
    F32 = 0x7d,
    F64 = 0x7c,
}

#[derive(Debug, num_derive::FromPrimitive, num_derive::ToPrimitive)]
pub enum Mutability {
    Const = 0,
    Var = 1,
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Instruction {
    Block {
        typ: Value,
        expr: Vec<Instruction>,
    },
    End,
    Br(u32),
    BrIf(u32),
    BrTable {
        branch_ixs: Vec<u32>,
        default_ix: u32,
    },
    Return,
    Call(u32),
    Select,
    LocalGet(u32),
    LocalSet(u32),
    LocalTee(u32),
    GlobalGet(u32),
    GlobalSet(u32),
    I32Load {
        offset: u32,
        alignment: u32,
    },
    I32Store {
        offset: u32,
        alignment: u32,
    },
    I32Const(i32),
    Add,
    Sub,
    Gt,
    Ge,
}

#[derive(Debug)]
pub struct Data {
    pub mem_idx: u64,
    pub offset: Vec<Instruction>,
    pub init: Vec<u8>,
}

#[derive(Debug, Default)]
pub struct Names {
    pub module_name: Option<String>,
    pub func_names: Vec<(u32, String)>,
}

#[derive(Debug, num_derive::FromPrimitive, num_derive::ToPrimitive)]
enum NameType {
    Module = 0x0,
    Function = 0x1,
    Local = 0x2,
}

fn print_ast(code: &[Instruction]) {
    use Instruction::*;

    fn inner(code: &[Instruction], depth: &mut usize) {
        for c in code {
            match c {
                Block { typ, expr } => {
                    print_indent("(block", *depth);
                    *depth += 1;
                    inner(expr, depth);
                }
                End => {
                    if *depth > 0 {
                        *depth -= 1;
                    }
                    print_indent(")", *depth)
                }
                Br(val) => print_indent("(br)", *depth),
                BrIf(val) => print_indent("(br_if)", *depth),
                BrTable {
                    branch_ixs,
                    default_ix,
                } => print_indent("(br_table)", *depth),
                Return => print_indent("(return)", *depth),
                Call(ix) => print_indent("(call)", *depth),
                Select => print_indent("(select)", *depth),
                LocalGet(ix) => print_indent(&format!("(local_get ${ix})"), *depth),
                LocalSet(ix) => print_indent(&format!("(local_set ${ix})"), *depth),
                GlobalGet(ix) => print_indent(&format!("(global_get ${ix})"), *depth),
                GlobalSet(ix) => print_indent(&format!("(global_set ${ix})"), *depth),
                I32Const(c) => print_indent(&format!("(i32_const {c})"), *depth),
                I32Load { offset, alignment } => {
                    print_indent(&format!("(i32_load {offset})"), *depth)
                }
                I32Store { offset, alignment } => {
                    print_indent(&format!("(i32_store {offset})"), *depth)
                }
                Add => print_indent("(add)", *depth),
                Gt => print_indent("(gt)", *depth),
                Ge => print_indent("(ge)", *depth),
                _other => print_indent("(inst)", *depth),
            }
        }
    }
    let mut depth = 0;
    inner(code, &mut depth)
}

fn print_indent(s: &str, indent: usize) {
    println!("{:>width$}{}", "", s, width = indent * 4);
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let code = std::fs::read(cli.file)?;
    let binary = parse_wasm(&code).map_err(|e| anyhow::format_err!("{:?}", e.code))?;
    println!("{binary}");

    for code in &binary.code {
        println!("Function");
        print_ast(&code.code);
    }

    // interpreter::interpret(&binary, "tagliatelle")?;
    interpreter::interpret(&binary, "do_add")?;

    Ok(())
}
