// Useful links:
// https://pengowray.github.io/wasm-ops/
// https://coinexsmartchain.medium.com/wasm-introduction-part-1-binary-format-57895d851580
// https://webassembly.github.io/spec/core/

use std::path::PathBuf;

use nom::{
    bytes::complete::{tag, take_until, take_while},
    combinator::{eof, map, map_opt, map_res, recognize},
    multi::{count, length_count, length_data, many1},
    number::complete::le_u32 as the_u32,
    number::complete::u8,
    sequence::{terminated, tuple},
    Finish, IResult,
};

use clap::Parser;
use num_traits::FromPrimitive;

#[derive(Parser)]
struct Cli {
    file: PathBuf,
}

type Result<'a, T> = IResult<&'a [u8], T>;

#[derive(Debug, num_derive::FromPrimitive, num_derive::ToPrimitive)]
pub enum OpCode {
    NoOp = 0x01,
    Block = 0x02,
    Loop = 0x03,
    If = 0x04,
    Else = 0x05,
    End = 0x0B,
    Br = 0x0C,
    BrIf = 0x0D,
    BrTable = 0x0E,
    Return = 0x0F,
    Call = 0x10,
    Drop = 0x1A,
    Select = 0x1B,
    LocalGet = 0x20,
    LocalSet = 0x21,
    LocalTee = 0x22,
    GlobalGet = 0x23,
    GlobalSet = 0x24,
    I32Store = 0x36,
    I32Const = 0x41,
    Add = 0x6a,
}

fn opcode(input: &[u8]) -> Result<OpCode> {
    map_opt(u8, OpCode::from_u8)(input)
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
    BrTable {
        branch_ixs: Vec<u32>,
        default_ix: u32,
    },
    Return,
    Call(u32),
    Select,
    LocalGet(u32),
    LocalSet(u32),
    I32Store {
        offset: u32,
        alignment: u32,
    },
    I32Const(i32),
    Add,
}

fn ast(mut input: &[u8]) -> Result<Vec<Instruction>> {
    let mut output = Vec::new();
    let mut instr;
    loop {
        (input, instr) = instruction(input)?;
        if let Instruction::End = instr {
            output.push(instr);
            return Ok((input, output));
        }
        output.push(instr);
    }
}

fn instruction(input: &[u8]) -> Result<Instruction> {
    // if !input.is_empty() { println!("{:x?}", input[0]) }
    use Instruction as I;
    use OpCode as O;
    let (input, op) = opcode(input)?;
    // println!("raw: {:?}", op);
    let op = match op {
        O::Block => {
            let (input, typ) = value(input)?;
            let (input, expr) = ast(&input)?;
            Ok((input, I::Block { typ, expr }))
        }
        O::End => Ok((input, I::End)),
        O::Br => map(leb128_u32, I::Br)(input),
        O::BrTable => map(
            tuple((length_count(leb128_u32, leb128_u32), leb128_u32)),
            |(branch_ixs, default_ix)| I::BrTable {
                branch_ixs,
                default_ix,
            },
        )(input),
        O::Return => Ok((input, I::Return)),
        O::Call => map(leb128_u32, I::Call)(input),
        O::Select => Ok((input, I::Select)),
        O::LocalGet => map(leb128_u32, I::LocalGet)(input),
        O::LocalSet => map(leb128_u32, I::LocalSet)(input),
        O::I32Store => map(tuple((leb128_u32, leb128_u32)), |(offset, alignment)| {
            I::I32Store { offset, alignment }
        })(input),
        O::I32Const => map(leb128_i32, I::I32Const)(input),
        O::Add => Ok((input, I::Add)),
        s => todo!("{s:?}"),
    };
    // println!("parsed: {:?}", op.as_ref().map(|v| &v.1).unwrap());
    op
}

#[derive(Debug)]
pub struct Binary {
    pub version: u32,
    pub sections: Vec<Section>,
}

#[derive(Debug)]
pub struct AnonSection {
    pub id: SectionId,
    pub data: Vec<u8>,
}

#[derive(Debug)]
pub struct Limits {
    pub min: u64,
    pub max: Option<u64>,
}

fn limits(input: &[u8]) -> Result<Limits> {
    let (input, has_max) = map_opt(u8, |v| match v {
        0 => Some(false),
        1 => Some(true),
        _ => None,
    })(input)?;
    let (input, min) = leb128(input)?;
    if has_max {
        let (input, max) = leb128(input)?;
        Ok((
            input,
            Limits {
                min,
                max: Some(max),
            },
        ))
    } else {
        Ok((input, Limits { min, max: None }))
    }
}

#[derive(Debug, num_derive::FromPrimitive, num_derive::ToPrimitive)]
pub enum Mutability {
    Const = 0,
    Var = 1,
}

fn mutability(input: &[u8]) -> Result<Mutability> {
    map_opt(u8, Mutability::from_u8)(input)
}

#[derive(Debug)]
pub struct Global {
    pub typ: Value,
    pub mutable: Mutability,
    pub expr: Vec<Instruction>,
}

fn global(input: &[u8]) -> Result<Global> {
    let (input, (typ, mutable, expr)) = tuple((value, mutability, ast))(input)?;
    Ok((input, Global { typ, mutable, expr }))
}

#[derive(Debug, num_derive::FromPrimitive, num_derive::ToPrimitive)]
pub enum Value {
    None = 0x40,
    I32 = 0x7f,
    I64 = 0x7e,
    F32 = 0x7d,
    F64 = 0x7c,
}

fn value(input: &[u8]) -> Result<Value> {
    map_opt(u8, Value::from_u8)(input)
}

#[derive(Debug, num_derive::FromPrimitive, num_derive::ToPrimitive)]
pub enum ExportType {
    Func = 0,
    Table = 1,
    Mem = 2,
    Global = 3,
}

fn export_type(input: &[u8]) -> Result<ExportType> {
    map_opt(u8, ExportType::from_u8)(input)
}

#[derive(Debug)]
pub struct Export {
    pub name: String,
    pub tag: ExportType,
    pub index: u64,
}

fn export(input: &[u8]) -> Result<Export> {
    let (input, slice) = map_res(length_data(leb128), std::str::from_utf8)(input)?;
    let (input, (tag, index)) = tuple((export_type, leb128))(input)?;
    Ok((
        input,
        Export {
            name: slice.to_string(),
            tag,
            index,
        },
    ))
}

#[derive(Debug)]
pub struct Code {
    pub locals: Vec<(u32, Value)>,
    pub code: Vec<Instruction>,
}

fn code(input: &[u8]) -> Result<Code> {
    let (input, _size) = leb128(input)?;
    let (input, locals) = length_count(leb128, tuple((map(leb128, |v| v as u32), value)))(input)?;
    let (input, code) = ast(input)?;
    Ok((input, Code { locals, code }))
}

#[derive(Debug)]
pub enum Section {
    Types(Vec<FuncSig>),
    Functions(Vec<u64>),
    Memory(Vec<Limits>),
    Global(Vec<Global>),
    Export(Vec<Export>),
    Code(Vec<Code>),
    Data(Vec<Data>),
    Anon(AnonSection),
}

#[derive(Debug, num_derive::FromPrimitive, num_derive::ToPrimitive)]
pub enum SectionId {
    Custom = 0,
    Type = 1,
    Import = 2,
    Func = 3,
    Table = 4,
    Mem = 5,
    Global = 6,
    Export = 7,
    Start = 8,
    Elem = 9,
    Code = 10,
    Data = 11,
}

#[derive(Debug)]
pub struct FuncSig {
    pub params: Vec<Value>,
    pub results: Vec<Value>,
}

fn func_sig(input: &[u8]) -> Result<FuncSig> {
    let (input, _) = tag(&[0x60])(input)?;
    let (input, nparams) = leb128(input)?;
    let (input, params) = count(value, nparams as usize)(input)?;
    let (input, nresults) = leb128(input)?;
    let (input, results) = count(value, nresults as usize)(input)?;
    Ok((input, FuncSig { params, results }))
}

#[derive(Debug)]
pub struct Data {
    pub mem_idx: u64,
    pub offset: Vec<Instruction>,
    pub init: Vec<u8>,
}

fn data(input: &[u8]) -> Result<Data> {
    let (input, (mem_idx, offset)) = tuple((map(u8, u64::from), ast))(input)?;
    let (input, init) = length_data(leb128)(input)?;
    Ok((
        input,
        Data {
            mem_idx,
            offset,
            init: init.to_vec(),
        },
    ))
}

const MASK: u8 = 0b01111111;

// todo handle overflows
fn leb128(input: &[u8]) -> Result<u64> {
    let (input, parts) = take_while(|v| v >> 7 == 1u8)(input)?;
    let (input, v2) = u8(input)?;
    let mut val = (v2 as u64) << (7 * parts.len());
    for (ix, v) in parts.iter().enumerate() {
        val += ((MASK & v) as u64) << (ix * 7)
    }
    Ok((input, val))
}

fn leb128_u32(input: &[u8]) -> Result<u32> {
    let (input, parts) = take_while(|v| v >> 7 == 1u8)(input)?;
    let (input, v2) = u8(input)?;
    let mut val = (v2 as u32) << (7 * parts.len());
    for (ix, v) in parts.iter().enumerate() {
        val += ((MASK & v) as u32) << (ix * 7)
    }
    Ok((input, val))
}

fn leb128_i32(input: &[u8]) -> Result<i32> {
    let (input, parts) = take_while(|v| v >> 7 == 1u8)(input)?;
    let (input, v2) = u8(input)?;
    let mut val = (v2 as u32) << (7 * parts.len());
    for (ix, v) in parts.iter().enumerate() {
        val += ((MASK & v) as u32) << (ix * 7)
    }
    Ok((input, unsafe { std::mem::transmute(val) }))
}

fn function_sigs(input: &[u8]) -> Result<Vec<FuncSig>> {
    length_count(leb128, func_sig)(input)
}

fn function_indices(input: &[u8]) -> Result<Vec<u64>> {
    length_count(leb128, leb128)(input)
}

fn memory(input: &[u8]) -> Result<Vec<Limits>> {
    length_count(leb128, limits)(input)
}

fn globals(input: &[u8]) -> Result<Vec<Global>> {
    length_count(leb128, global)(input)
}

fn section(input: &[u8]) -> Result<Section> {
    let (input, id) = map_opt(u8, SectionId::from_u8)(input)?;
    let (input, section_data) = length_data(leb128)(input)?;
    println!("Segment: {id:?}");

    // TODO if segment parsing fails, should be a HARD error

    let section = match id {
        SectionId::Type => {
            let (_, funcs) = function_sigs(section_data)?;
            Section::Types(funcs)
        }
        SectionId::Func => {
            let (_, func_ixs) = function_indices(section_data)?;
            Section::Functions(func_ixs)
        }
        SectionId::Mem => {
            let (_, mem) = memory(section_data)?;
            Section::Memory(mem)
        }
        SectionId::Global => {
            let (_, globals) = globals(section_data)?;
            Section::Global(globals)
        }
        SectionId::Export => {
            let (_, exports) = length_count(leb128, export)(section_data)?;
            Section::Export(exports)
        }
        SectionId::Code => {
            let (_, code) = length_count(leb128, code)(section_data)?;
            Section::Code(code)
        }
        SectionId::Data => {
            let (_, data) = length_count(leb128, data)(section_data)?;
            Section::Data(data)
        }
        _ => Section::Anon(AnonSection {
            id,
            data: section_data.to_vec(),
        }),
    };
    Ok((input, section))
}

fn binary(input: &[u8]) -> Result<Binary> {
    let (input, _) = tag(b"\0asm")(input)?;
    let (input, version) = the_u32(input)?;
    let (input, sections) = many1(section)(input)?;
    Ok((input, Binary { version, sections }))
}

fn parse_wasm(input: &[u8]) -> anyhow::Result<Binary, nom::error::Error<&[u8]>> {
    let (_, out) = terminated(binary, eof)(input).finish()?;
    Ok(out)
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
                BrTable {
                    branch_ixs,
                    default_ix,
                } => print_indent("(br_table)", *depth),
                Return => print_indent("(return)", *depth),
                Call(ix) => print_indent("(call)", *depth),
                Select => print_indent("(select)", *depth),
                LocalGet(ix) => print_indent(&format!("(local_get ${ix})"), *depth),
                LocalSet(ix) => print_indent(&format!("(local_set ${ix})"), *depth),
                I32Const(c) => print_indent(&format!("(i32_const {c})"), *depth),
                I32Store { offset, alignment } => {
                    print_indent(&format!("(i32_store {offset})"), *depth)
                }
                Add => print_indent("(add)", *depth),
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
    println!("{binary:?}");

    for s in binary.sections {
        match s {
            Section::Code(codes) => {
                for code in codes {
                    println!("Function");
                    print_ast(&code.code);
                }
            }
            _ => {}
        }
    }

    Ok(())
}
