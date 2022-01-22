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

function writePush(location: string, offset: string) {
  if (!["constant", "local", "argument", "this", "that", "temp", "pointer", "static"].includes(location)) {
    throw new Error(`Unknown location (${location}) in line: push ${location} ${offset}`);
  }

  if (location == "pointer" || location == "temp") {
    output.push("@" + inlineTable[location + offset]);
    output.push("D=M");
  } else if (location == "constant") {
    output.push("@" + offset);
    output.push("D=A");
  } else if (location == "static") {
    output.push(`@${fileName}.${offset}`);
    output.push("D=M");
  } else {
    output.push("@" + table[location]);
    output.push("D=M");
    output.push("@" + offset);
    output.push("A=D+A");
    output.push("D=M");
  }
  output.push("@SP");
  output.push("M=M+1");
  output.push("A=M-1");
  output.push("M=D");
}

function writePop(location: string, offset: string) {
  if (!["local", "argument", "this", "that", "temp", "pointer", "static"].includes(location)) {
    throw new Error(`Unknown location (${location}) in line: pop ${location} ${offset}`);
  }

  let normalCase = ["local", "argument", "this", "that"].includes(location);

  if (normalCase) {
    output.push("@" + table[location]);
    output.push("D=M");
    output.push("@" + offset);
    output.push("D=D+A");
    output.push("@R13");
    output.push("M=D");
  }

  output.push("@SP");
  output.push("AM=M-1");
  output.push("D=M");

  if (normalCase) {
    output.push("@R13");
    output.push("A=M");
  } else {
    if (location == "static") {
      output.push(`@${fileName}.${offset}`);
    } else {
      output.push("@" + inlineTable[location + offset]);
    }
  }

  output.push("M=D");
}

function writeArithmetic(type: string) {
  const arithmeticTable = { add: "+", sub: "-", and: "&", or: "|" };
  output.push("@SP");
  output.push("AM=M-1");
  output.push("D=M");
  output.push("A=A-1");
  output.push(`M=M${arithmeticTable[type]}D`);
}

function writeBit(type: string) {
  const bitTable = { neg: "-", not: "!" };
  output.push("@SP");
  output.push("A=M-1");
  output.push(`M=${bitTable[type]}M`);
}

let counter = 0;
function writeBoolean(type: string) {
  const booleanTable = { eq: "JEQ", lt: "JLT", gt: "JGT" };
  output.push("@SP");
  output.push("AM=M-1");
  output.push("D=M");
  output.push("A=A-1");
  output.push("D=M-D");
  output.push("M=0");
  output.push("@TRUTHY." + counter);
  output.push(`D;${booleanTable[type]}`);
  output.push("@DONE." + counter);
  output.push("0;JMP");
  output.push("(TRUTHY." + counter + ")");
  output.push("@SP");
  output.push("A=M-1");
  output.push("M=-1");
  output.push("(DONE." + counter + ")");
  counter++;
}

for (const line of lines) {
  let [type, location, offset] = line.split(/ +/g);
  if ("push" == type) {
    writePush(location, offset);
  } else if ("pop" == type) {
    writePop(location, offset);
  } else if (["add", "sub", "and", "or"].includes(type)) {
    writeArithmetic(type);
  } else if (["neg", "not"].includes(type)) {
    writeBit(type);
  } else if (["eq", "lt", "gt"].includes(type)) {
    writeBoolean(type);
  } else {
    console.log("UNKNOWN", line);
  }
}

writeFileSync(file.replace(/\.vm$/, ".asm"), output.join("\n"));
