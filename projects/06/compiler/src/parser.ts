import { readFileSync, writeFileSync } from "fs";

type Table = { [key: string]: string };
// prettier-ignore
const compTable: Table = {
  "0":   "0101010",
  "1":   "0111111",
  "-1":  "0111010",
  "D":   "0001100",
  "A":   "0110000", "M":   "1110000",
  "!D":  "0001101",
  "!A":  "0110001", "!M":  "1110001",
  "-D":  "0001111",
  "-A":  "0110011", "-M":  "1110011", 
  "D+1": "0011111",
  "A+1": "0110111", "M+1": "1110111",
  "D-1": "0001110",
  "A-1": "0110010", "M-1": "1110010",
  "D+A": "0000010", "D+M": "1000010",
  "D-A": "0010011", "D-M": "1010011",
  "A-D": "0000111", "M-D": "1000111",
  "D&A": "0000000", "D&M": "1000000",
  "D|A": "0010101", "D|M": "1010101",
};

// prettier-ignore
const destTable: Table = {
  "":    "000",
  "M":   "001",
  "D":   "010",
  "MD":  "011",
  "A":   "100",
  "AM":  "101",
  "AD":  "110",
  "AMD": "111",
}

// prettier-ignore
const jmpTable: Table = {
  "":    "000",
  "JGT": "001",
  "JEQ": "010",
  "JGE": "011",
  "JLT": "100",
  "JNE": "101",
  "JLE": "110",
  "JMP": "111",
}

const file = process.argv[2];
const lines = readFileSync(file, "utf8")
  .replace(/\/\/.*/g, "")
  .replace(/[ \t]/g, "")
  .replace(/[\r\n]+/g, "\n")
  .trim()
  .split("\n");

type SymbolTable = { [key: string]: number };
function buildSymbolTable(): SymbolTable {
  // prettier-ignore
  const symbols: SymbolTable = {
    SP:     0x0000,
    LCL:    0x0001,
    ARG:    0x0002,
    THIS:   0x0003,
    THAT:   0x0004,
    SCREEN: 0x4000,
    KBD:    0x6000,
  };

  for (let r = 0; r < 16; r++) symbols["R" + r] = r;

  let i = 0;
  for (const line of lines) {
    if (line.startsWith("(")) {
      symbols[line.slice(1, -1)] = i;
    } else {
      i++;
    }
  }
  return symbols;
}

function convertToBinary(symbols: SymbolTable) {
  let symbolstart = 16;
  let out = "";
  for (const line of lines) {
    if (line.startsWith("@")) {
      const symbol = line.slice(1);
      const num = Number.parseInt(symbol);
      if (!(symbol in symbols) && Number.isNaN(num)) {
        symbols[symbol] = symbolstart++;
      }
      out += (symbols[symbol] ?? num).toString(2).padStart(16, "0") + "\n";
    } else if (!line.startsWith("(")) {
      const x = line.split("=");
      const dest = x[x.length - 2] ?? "";
      const condjmp = x[x.length - 1].split(";");
      const cond = condjmp[0];
      const jmp = condjmp[1] ?? "";
      out += "111" + compTable[cond] + destTable[dest] + jmpTable[jmp] + "\n";
    }
  }
  return out;
}

let symbols = buildSymbolTable();
let binary = convertToBinary(symbols);
writeFileSync(file.replace(/\.asm$/, ".hack"), binary);
