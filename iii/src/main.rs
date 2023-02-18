// Useful links:
// https://pengowray.github.io/wasm-ops/
// https://coinexsmartchain.medium.com/wasm-introduction-part-1-binary-format-57895d851580
// https://webassembly.github.io/spec/core/

use std::path::PathBuf;

use nom::{
    bytes::complete::{tag, take_until, take_while},
    combinator::{eof, map, map_opt, map_res},
    multi::{count, length_count, length_data, many1},
    number::complete::le_u32,
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
    Drop = 0x1A,
    Select = 0x1B,
    LocalGet = 0x20,
    LocalSet = 0x21,
    LocalTee = 0x22,
    GlobalGet = 0x23,
    GlobalSet = 0x24,
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
    let (input, min) = leb28(input)?;
    if has_max {
        let (input, max) = leb28(input)?;
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

type Instruction = u8;

#[derive(Debug)]
pub struct Global {
    pub typ: Value,
    pub mutable: Mutability,
    pub expr: Vec<Instruction>,
}

fn instructions(input: &[u8]) -> Result<Vec<Instruction>> {
    let (input, (expr, _)) = tuple((take_until(&[0x0B][..]), u8))(input)?;
    let mut expr = expr.to_vec();
    expr.push(0x0B);
    Ok((input, expr))
}

fn global(input: &[u8]) -> Result<Global> {
    let (input, (typ, mutable, expr)) = tuple((value, mutability, instructions))(input)?;
    Ok((input, Global { typ, mutable, expr }))
}

#[derive(Debug, num_derive::FromPrimitive, num_derive::ToPrimitive)]
pub enum Value {
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
    let (input, slice) = map_res(length_data(leb28), std::str::from_utf8)(input)?;
    let (input, (tag, index)) = tuple((export_type, leb28))(input)?;
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
    let (input, _size) = leb28(input)?;
    let (input, locals) = length_count(leb28, tuple((map(leb28, |v| v as u32), value)))(input)?;
    let (input, code) = instructions(input)?;
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
    let (input, nparams) = leb28(input)?;
    let (input, params) = count(value, nparams as usize)(input)?;
    let (input, nresults) = leb28(input)?;
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
    let (input, (mem_idx, offset)) = tuple((map(u8, u64::from), instructions))(input)?;
    let (input, init) = length_data(leb28)(input)?;
    Ok((
        input,
        Data {
            mem_idx,
            offset,
            init: init.to_vec(),
        },
    ))
}

fn leb28(input: &[u8]) -> Result<u64> {
    let (input, parts) = take_while(|v| v >> 7 == 1u8)(input)?;
    let (input, v2) = u8(input)?;
    let mut val = (v2 as u64) << (7 * parts.len());
    for (ix, v) in parts.iter().enumerate() {
        val += ((0b10000000 & v) as u64) << (ix * 7)
    }
    Ok((input, val))
}

fn function_sigs(input: &[u8]) -> Result<Vec<FuncSig>> {
    length_count(leb28, func_sig)(input)
}

fn function_indices(input: &[u8]) -> Result<Vec<u64>> {
    length_count(leb28, leb28)(input)
}

fn memory(input: &[u8]) -> Result<Vec<Limits>> {
    length_count(leb28, limits)(input)
}

fn globals(input: &[u8]) -> Result<Vec<Global>> {
    length_count(leb28, global)(input)
}

fn section(input: &[u8]) -> Result<Section> {
    let (input, id) = map_opt(u8, SectionId::from_u8)(input)?;
    let (input, section_data) = length_data(leb28)(input)?;

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
            let (_, exports) = length_count(leb28, export)(section_data)?;
            Section::Export(exports)
        }
        SectionId::Code => {
            let (_, code) = length_count(leb28, code)(section_data)?;
            Section::Code(code)
        }
        SectionId::Data => {
            let (_, data) = length_count(leb28, data)(section_data)?;
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
    let (input, version) = le_u32(input)?;
    let (input, sections) = many1(section)(input)?;
    Ok((input, Binary { version, sections }))
}

fn parse_wasm(input: &[u8]) -> anyhow::Result<Binary, nom::error::Error<&[u8]>> {
    let (_, out) = terminated(binary, eof)(input).finish()?;
    Ok(out)
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let code = std::fs::read(cli.file)?;
    let binary = parse_wasm(&code).map_err(|e| anyhow::format_err!("{:?}", e.code))?;
    println!("{binary:?}");
    Ok(())
}
