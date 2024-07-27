#![feature(test)]
extern crate test;

use std::{
    collections::HashMap,
    env,
    fs::{self, File},
    io::{BufRead, BufReader},
};

// The built in macos allocator is slow, so we use jemalloc, a lightweight allocator instead
#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

// We use a hyper optimized integer to binary (see benchmarks below)
fn i16_to_digits(mut n: i16) -> String {
    let mut v = vec![0; 16];
    for i in (0..16).rev() {
        v[i] = (n & 1) as u8 + b'0';
        n >>= 1;
    }
    unsafe { String::from_utf8_unchecked(v) }
}

// We initialize the symbol table, adding in symbols for R0-R15 (this table will be modified when we build lines)
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

// Update the symbol table with labels, and return the file (lines) with stripped labels, whitespace, and comments
fn build_lines_symbols(reader: BufReader<File>, symbols: &mut HashMap<String, i16>) -> Vec<String> {
    let mut out = Vec::with_capacity(16); // the file is likely to be at least 16 lines long, so preallocate
    let mut i = 0; // This is the line number (without labels)
    for line in reader.lines() {
        let mut line = line.unwrap();
        if let Some(char) = line.find('/') {
            // Strip anything after a / (because a '/' can't be used anywhere else, so just assume that this is a comment)
            line.truncate(char);
        }
        line.retain(|c| c != ' ' && c != '\t'); // Strip whitespace
        if line.is_empty() { // If the line is now empty, skip it
            continue;
        } else if line.starts_with('(') { // Label
            line.retain(|c| c != '(' && c != ')'); // Strip parentheses
            symbols.insert(line, i); // Add to symbol table
        } else {
            out.push(line); // Add the line to the buffer of lines
            i += 1;
        }
    }
    out
}

// Take a string of the form @ASDF or @123 and update the line with the binary representation
fn parse_symbol(line: &mut String, symbols: &mut HashMap<String, i16>, symbolstart: &mut i16) {
    let symbol = line.split_off(1); // Remove the @ symbol from the front
    let num = symbol.parse::<i16>(); // Attempt to parse the symbol as a number
    *line = i16_to_digits(match num {
        Ok(n) => n,
        _ => *symbols.entry(symbol).or_insert_with(|| {
            let o = *symbolstart; // If the symbol doesn't exist
            *symbolstart += 1; // Update the symbol start for the next symbol
            o // Use the current symbol start
        }),
    });
}

// Take a string of the form M=A+D;JMP and update the line with the binary representation of the C-expression
fn parse_expression(line: &mut String) {
    let equals = line.find('=');
    let semicolon = line.find(';');
    let dest = &line[..equals.unwrap_or(0)]; // Read from the beginning to the first '=' (or return empty string if no '=')
    let comp = &line[equals.map(|x| x + 1).unwrap_or(0)..semicolon.unwrap_or_else(|| line.len())]; // Read from after the beginning to the first '=' to the first ';' (with sensible defaults when no '=' or ';')
    let jmp = &line[semicolon.map(|x| x + 1).unwrap_or_else(|| line.len())..]; // Read from after the first semicolon to the end
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
        "D+1" | "1+D" => "0011111",
        "A+1" | "1+A" => "0110111",
        "D-1" => "0001110",
        "A-1" => "0110010",
        "D+A" | "A+D" => "0000010",
        "D-A" => "0010011",
        "A-D" => "0000111",
        "D&A" | "A&D" => "0000000",
        "D|A" | "A|D" => "0010101",
        "M" => "1110000",
        "!M" => "1110001",
        "-M" => "1110011",
        "M+1" | "1+M" => "1110111",
        "M-1" => "1110010",
        "D+M" | "M+D" => "1000010",
        "D-M" => "1010011",
        "M-D" => "1000111",
        "D&M" | "M&D" => "1000000",
        "D|M" | "M|D" => "1010101",
        _ => unreachable!(),
    });
    out.push_str(match dest {
        "M" => "001",
        "D" => "010",
        "MD" | "DM" => "011",
        "A" => "100",
        "AM" | "MA" => "101",
        "AD" | "DA" => "110",
        "AMD" | "MAD" | "DAM" | "MDA" | "DMA" | "ADM" => "111",
        "" => "000",
        _ => unreachable!(),
    });
    out.push_str(match jmp {
        "JGT" => "001",
        "JEQ" => "010",
        "JGE" => "011",
        "JLT" => "100",
        "JNE" => "101",
        "JLE" => "110",
        "JMP" => "111",
        "" => "000",
        _ => unreachable!(),
    });
    *line = out;
}

fn main() {
    let file_name = env::args().nth(1).expect("Pass a file to compile");
    let file = File::open(&file_name).expect("Couldn't read file");
    let reader = BufReader::new(file);

    let mut symbols = build_symbol_table();
    let mut out = build_lines_symbols(reader, &mut symbols);

    let mut symbolstart = 16; // Next available symbol location
    for line in &mut out {
        if line.starts_with('@') { // If the line starts with @, it's a symbol (A-instruction)
            parse_symbol(line, &mut symbols, &mut symbolstart);
        } else { // Otherwise, it's a C-expression
            parse_expression(line);
        }
    }

    fs::write(file_name.replace(".asm", ".hack"), out.join("\n")).expect("Couldn't write to file");
}

// BENCHMARKS...
// It turns out that a slow part of this program is converting symbols to binary, so I optimized that with i16 to digits
// The other slow part is allocation, which is why we preallocate and use jemalloc
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
    fn bench_u16_format(b: &mut Bencher) {
        b.iter(|| format!("{:016b}", -0b01010101011110));
    }
}
