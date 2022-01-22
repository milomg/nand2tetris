import { readFileSync, writeFileSync } from "fs";
import { parse } from "path";

const file = process.argv[2];
const lines = readFileSync(file, "utf8")
  .replace(/[\r\n]+/g, "\n")
  .split("\n")
  .map((x) => x.trim())
  .filter((x) => !x.startsWith("//") && x != "");

const fileName = parse(file).name;

const table = {
  local: "LCL",
  argument: "ARG",
  this: "THIS",
  that: "THAT",
};

const inlineTable = {
  pointer0: "THIS",
  pointer1: "THAT",
  temp0: "R5",
  temp1: "R6",
  temp2: "R7",
  temp3: "R8",
  temp4: "R9",
  temp5: "R10",
  temp6: "R11",
  temp7: "R12",
};

const output = [];
function write(strings: TemplateStringsArray, ...values: Array<string | number>) {
  let result = "";
  for (let i = 0; i < strings.length; i++) {
    result += strings[i];
    if (i < values.length) {
      result += values[i];
    }
  }
  output.push(result.replace(/^\s+/gm, ""));
}

function writePush(location: string, offset: string) {
  if (!["constant", "local", "argument", "this", "that", "temp", "pointer", "static"].includes(location)) {
    throw new Error(`Unknown location (${location}) in line: push ${location} ${offset}`);
  }

  if (location == "pointer" || location == "temp") {
    write`@${inlineTable[location + offset]}
          D=M`;
  } else if (location == "constant") {
    write`@${offset}
          D=A`;
  } else if (location == "static") {
    write`@${fileName}.${offset}
          D=M`;
  } else {
    write`@${table[location]}
          D=M
          @${offset}
          A=D+A
          D=M`;
  }
  write`@SP
        M=M+1
        A=M-1
        M=D`;
}

function writePop(location: string, offset: string) {
  if (!["local", "argument", "this", "that", "temp", "pointer", "static"].includes(location)) {
    throw new Error(`Unknown location (${location}) in line: pop ${location} ${offset}`);
  }

  let normalCase = ["local", "argument", "this", "that"].includes(location);

  if (normalCase) {
    write`@${table[location]}
          D=M
          @${offset}
          D=D+A
          @R13
          M=D`;
  }

  write`@SP
        AM=M-1
        D=M`;

  if (normalCase) {
    write`@R13
          A=M`;
  } else {
    if (location == "static") {
      write`@${fileName}.${offset}`;
    } else {
      write`@${inlineTable[location + offset]}`;
    }
  }

  write`M=D`;
}

function writeArithmetic(type: string) {
  const arithmeticTable = { add: "+", sub: "-", and: "&", or: "|" };
  write`@SP
        AM=M-1
        D=M
        A=A-1
        M=M${arithmeticTable[type]}D`;
}

function writeBit(type: string) {
  const bitTable = { neg: "-", not: "!" };
  write`@SP
        A=M-1
        M=${bitTable[type]}M`;
}


function writeBoolean(type: string, counter: number) {
  const booleanTable = { eq: "JEQ", lt: "JLT", gt: "JGT" };
  write`@SP
        AM=M-1
        D=M
        A=A-1
        D=M-D
        M=0
        @TRUTHY.${counter}
        D;${booleanTable[type]}
        @DONE.${counter}
        0;JMP
        (TRUTHY.${counter})
        @SP
        A=M-1
        M=-1
        (DONE.${counter})`;
}

let counter = 0;
for (const line of lines) {
  let [type, location, offset] = line.split(/\s+/g);
  if ("push" == type) {
    writePush(location, offset);
  } else if ("pop" == type) {
    writePop(location, offset);
  } else if (["add", "sub", "and", "or"].includes(type)) {
    writeArithmetic(type);
  } else if (["neg", "not"].includes(type)) {
    writeBit(type);
  } else if (["eq", "lt", "gt"].includes(type)) {
    writeBoolean(type, counter);
    counter++;
  } else {
    throw new Error(`Unknown command: ${line}`);
  }
}

writeFileSync(file.replace(/\.vm$/, ".asm"), output.join("\n"));
