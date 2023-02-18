// Useful links:
// https://pengowray.github.io/wasm-ops/
// https://coinexsmartchain.medium.com/wasm-introduction-part-1-binary-format-57895d851580

use std::{fs::File, path::PathBuf};

use nom::{
    bytes::complete::{tag, take, take_while},
    combinator::{eof, map_opt},
    error::dbg_dmp,
    multi::{count, length_count, length_data, many1},
    number::complete::le_u32,
    number::complete::u8,
    sequence::terminated,
    Finish, IResult,
};

use clap::{Parser, Subcommand};
use num_traits::FromPrimitive;

#[derive(Parser)]
struct Cli {
    file: PathBuf,
}

type Result<'a, T> = IResult<&'a [u8], T>;

enum OpCode {
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
struct Binary {
    version: u32,
    sections: Vec<Section>,
}

#[derive(Debug)]
struct AnonSection {
    id: SectionId,
    data: Vec<u8>,
}

#[derive(Debug)]
struct Limits {
    min: u64,
    max: Option<u64>,
}

#[derive(Debug)]
enum Section {
    Types(Vec<FuncSig>),
    Functions(Vec<u64>),
    Memory(Vec<Limits>),
    Anon(AnonSection),
}

#[derive(Debug, num_derive::FromPrimitive, num_derive::ToPrimitive)]
enum SectionId {
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

#[derive(Debug, num_derive::FromPrimitive, num_derive::ToPrimitive)]
enum Value {
    I32 = 0x7f,
    I64 = 0x7e,
    F32 = 0x7d,
    F64 = 0x7c,
}

fn value(input: &[u8]) -> Result<Value> {
    map_opt(u8, Value::from_u8)(input)
}

#[derive(Debug)]
struct FuncSig {
    params: Vec<Value>,
    results: Vec<Value>,
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

fn func_sig(input: &[u8]) -> Result<FuncSig> {
    let (input, _) = tag(&[0x60])(input)?;
    let (input, nparams) = leb28(input)?;
    let (input, params) = count(value, nparams as usize)(input)?;
    let (input, nresults) = leb28(input)?;
    let (input, results) = count(value, nresults as usize)(input)?;
    Ok((input, FuncSig { params, results }))
}

fn function_sigs(input: &[u8]) -> Result<Vec<FuncSig>> {
    length_count(leb28, func_sig)(input)
}

fn function_indices(input: &[u8]) -> Result<Vec<u64>> {
    length_count(leb28, leb28)(input)
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

fn memory(input: &[u8]) -> Result<Vec<Limits>> {
    length_count(leb28, limits)(input)
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
