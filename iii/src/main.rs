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
    #[command(subcommand)]
    action: Action,
}

#[derive(clap::Subcommand)]
enum Action {
    Exec { main: String },
    Show,
    Validate,
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
    pub custom: Vec<(String, Vec<u8>)>,
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
        writeln!(f, "  Func Names:")?;
        for (ix, n) in &self.names.func_names {
            writeln!(f, "    {ix}: {n}")?;
        }
        writeln!(f, "  Global Names:")?;
        for (ix, n) in &self.names.global_names {
            writeln!(f, "    {ix}: {n}")?;
        }
        writeln!(f, "  Data Names:")?;
        for (ix, n) in &self.names.data_names {
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
    Unreachable,
    NoOp,
    Block {
        typ: Option<Value>,
        expr: Vec<Instruction>,
    },
    Loop {
        typ: Option<Value>,
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
    CallIndirect(u32),
    Drop,
    Select,
    LocalGet(u32),
    LocalSet(u32),
    LocalTee(u32),
    GlobalGet(u32),
    GlobalSet(u32),
    MemoryGrow,
    MemOp {
        offset: u32,
        alignment: u32,
        op: MemOp,
    },
    I32Const(i32),
    I64Const(i64),
    UnaryOp {
        op: UnaryOp,
        size: Size,
    },
    BinOp {
        op: BinOp,
        size: Size,
    },
    BinOpSigned {
        op: BinOpSigned,
        size: Size,
        sign: Sign,
    },
    Wrap,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Size {
    X32,
    X64,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Sign {
    Signed,
    Unsigned,
}

#[derive(Debug, Copy, Clone)]
pub enum BinOpSigned {
    Lt,
    Gt,
    Le,
    Ge,
    Div,
    Rem,
    Shr,
}

#[derive(Debug)]
pub enum UnaryOp {
    Eqz,
    Ctz,
    Clz,
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    And,
    Sub,
    Le,
    LeU,
    Eq,
    Neq,
    Mul,
    Or,
    Xor,
    Rotl,
    Rotr,
    Shl,
}

#[derive(Debug)]
pub enum MemOp {
    I32Load,
    I64Load,
    I32Store,
    I64Store,
    I32Store8,
    I32Store16,
    I32Load8,
    U32Load8,
    U32Load16,
    I64Load32,
    U64Load32,
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
    pub global_names: Vec<(u32, String)>,
    pub data_names: Vec<(u32, String)>,
}

fn print_ast(code: &[Instruction]) {
    use Instruction::*;

    fn inner(code: &[Instruction], depth: &mut usize) {
        for c in code {
            match c {
                Block { typ: _, expr } => {
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
                Br(_val) => print_indent("(br)", *depth),
                BrIf(_val) => print_indent("(br_if)", *depth),
                BrTable {
                    branch_ixs: _,
                    default_ix: _,
                } => print_indent("(br_table)", *depth),
                Return => print_indent("(return)", *depth),
                Call(_ix) => print_indent("(call)", *depth),
                Select => print_indent("(select)", *depth),
                LocalGet(ix) => print_indent(&format!("(local_get ${ix})"), *depth),
                LocalSet(ix) => print_indent(&format!("(local_set ${ix})"), *depth),
                GlobalGet(ix) => print_indent(&format!("(global_get ${ix})"), *depth),
                GlobalSet(ix) => print_indent(&format!("(global_set ${ix})"), *depth),
                I32Const(c) => print_indent(&format!("(i32_const {c})"), *depth),
                MemOp {
                    offset,
                    alignment: _,
                    op,
                } => print_indent(&format!("({op:?} {offset})"), *depth),
                UnaryOp { op, .. } => print_indent(&format!("({op:?})"), *depth),
                BinOp { op, .. } => print_indent(&format!("({op:?})"), *depth),
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
    match cli.action {
        Action::Validate => {
            todo!("no validation yet")
        }
        Action::Show => {
            println!("{binary}");
        }
        Action::Exec { main } => {
            interpreter::interpret(&binary, &main)?;
        }
    }
    Ok(())
}
