#![feature(test)]
extern crate test;

use std::{
    collections::{HashMap},
    env,
    fs::{self, File},
    io::{BufRead, BufReader},
};

#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

fn i16_to_digits(mut n: i16) -> String {
    let mut v = vec![0; 16];
    for i in (0..16).rev() {
        v[i] = (n & 1) as u8 + b'0';
        n >>= 1;
    }
    unsafe { String::from_utf8_unchecked(v) }
}

fn build_symbol_table() -> HashMap<String, i16> {
    let mut symbols = HashMap::new();
    symbols.insert("SP".to_owned(), 0);
    symbols.insert("LCL".to_owned(), 1);
    symbols.insert("ARG".to_owned(), 2);
    symbols.insert("THIS".to_owned(), 3);
    symbols.insert("THAT".to_owned(), 4);
    symbols.insert("SCREEN".to_owned(), 0x4000);
    symbols.insert("KBD".to_owned(), 0x6000);

    for r in 0..16 {
        symbols.insert("R".to_owned() + &r.to_string(), r);
    }

    symbols
}

fn build_lines_symbols(reader: BufReader<File>, symbols: &mut HashMap<String, i16>) -> Vec<String> {
    let mut out = Vec::with_capacity(16); // the file is likely to be at least 16 lines long, so preallocate
    let mut i = 0;
    for line in reader.lines() {
        let mut line = line.unwrap();
        if let Some(char) = line.find('/') {
            line.truncate(char);
        }
        line.retain(|c| c != ' ');
        if line.is_empty() {
            continue;
        } else if line.starts_with('(') {
            line.retain(|c| c != '(' && c != ')');
            symbols.insert(line, i);
        } else {
            out.push(line);
            i += 1;
        }
    }
    out
}

fn parse_symbol(line: &mut String, symbols: &mut HashMap<String, i16>, symbolstart: &mut i16) {
    let symbol = line.split_off(1);
    let num = symbol.parse::<i16>();
    *line = i16_to_digits(match num {
        Ok(n) => n,
        _ => *symbols.entry(symbol).or_insert_with(|| {
            let o = *symbolstart;
            *symbolstart += 1;
            o
        }),
    });
}

fn parse_expression(line: &mut String) {
    let x = line.find('=');
    let y = line.find(';');
    let dest = &line[..x.unwrap_or(0)];
    let comp = &line[x.map(|x| x + 1).unwrap_or(0)..y.unwrap_or_else(|| line.len())];
    let jmp = &line[y.map(|x| x + 1).unwrap_or_else(|| line.len())..];
    let mut out = String::with_capacity(16); // preallocate for perf
    out.push_str("111");
    out.push_str(match comp {
        "0" => "0101010",
        "1" => "0111111",
        "-1" => "0111010",
        "D" => "0001100",
        "A" => "0110000",
        "!D" => "0001101",
        "!A" => "0110001",
        "-D" => "0001111",
        "-A" => "0110011",
        "D+1" => "0011111",
        "A+1" => "0110111",
        "D-1" => "0001110",
        "A-1" => "0110010",
        "D+A" => "0000010",
        "D-A" => "0010011",
        "A-D" => "0000111",
        "D&A" => "0000000",
        "D|A" => "0010101",
        "M" => "1110000",
        "!M" => "1110001",
        "-M" => "1110011",
        "M+1" => "1110111",
        "M-1" => "1110010",
        "D+M" => "1000010",
        "D-M" => "1010011",
        "M-D" => "1000111",
        "D&M" => "1000000",
        "D|M" => "1010101",
        _ => unreachable!(),
    });
    out.push_str(match dest {
        "M" => "001",
        "D" => "010",
        "MD" => "011",
        "A" => "100",
        "AM" => "101",
        "AD" => "110",
        "AMD" => "111",
        _ => "000",
    });
    out.push_str(match jmp {
        "JGT" => "001",
        "JEQ" => "010",
        "JGE" => "011",
        "JLT" => "100",
        "JNE" => "101",
        "JLE" => "110",
        "JMP" => "111",
        _ => "000",
    });
    *line = out;
}

fn main() {
    let file_name = env::args().nth(1).expect("Pass a file to compile");
    let file = File::open(&file_name).expect("Couldn't read file");
    let reader = BufReader::new(file);

    let mut symbols = build_symbol_table();
    let mut out = build_lines_symbols(reader, &mut symbols);

    let mut symbolstart = 16;
    for line in &mut out {
        if line.starts_with('@') {
            parse_symbol(line, &mut symbols, &mut symbolstart);
        } else {
            parse_expression(line);
        }
    }

    fs::write(file_name.replace(".asm", ".hack"), out.join("\n")).expect("Couldn't write to file");
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn equal() {
        assert_eq!(
            i16_to_digits(0b01010101011110),
            format!("{:016b}", 0b01010101011110)
        )
    }

    #[bench]
    fn bench_u16_loop(b: &mut Bencher) {
        b.iter(|| i16_to_digits(-0b01010101011110));
    }

    #[bench]
    fn bench_u16_loop2(b: &mut Bencher) {
        b.iter(|| i16_to_digits2(-0b01010101011110));
    }

    #[bench]
    fn bench_u16_format(b: &mut Bencher) {
        b.iter(|| format!("{:016b}", -0b01010101011110));
    }
}
