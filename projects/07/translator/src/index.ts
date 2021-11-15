import { readFileSync, writeFileSync } from "fs";

const file = process.argv[2];
const lines = readFileSync(file, "utf8")
  .replace(/[\r\n]+/g, "\n")
  .split("\n")
  .map((x) => x.trim())
  .filter((x) => !x.startsWith("//") && x != "");

const output = [];
let counter = 0;
for (const line of lines) {
  let pieces = line.split(/ +/g);
  if (pieces[0] == "push" && pieces[1] == "constant") {
    output.push("@" + pieces[2]);
    output.push("D=A");
    output.push("@SP");
    output.push("A=M");
    output.push("M=D");
    output.push("@SP");
    output.push("M=M+1");
  } else if (pieces[0] == "add" || pieces[0] == "sub" || pieces[0] == "and" || pieces[0] == "or") {
    output.push("@SP");
    output.push("AM=M-1");
    output.push("D=M");
    output.push("A=A-1");
    output.push(`M=M${{add: "+", sub: "-", and: "&", or: "|"}[pieces[0]]}D`);
  }  else if (pieces[0] == "neg" || pieces[0] == "not") {
    output.push("@SP");
    output.push("A=M-1");
    output.push(`M=${{neg:"-", not:"!"}[pieces[0]]}M`);
  } else if (pieces[0] == "eq" || pieces[0] == "lt" || pieces[0] == "gt") {
    output.push("@SP");
    output.push("AM=M-1");
    output.push("D=M");
    output.push("A=A-1");
    output.push("D=M-D");
    output.push("@TRUTHY." + counter);
    output.push({ eq: "D;JEQ", lt: "D;JLT", gt: "D;JGT" }[pieces[0]]);
    output.push("(FALSY." + counter + ")");
    output.push("@SP");
    output.push("A=M-1");
    output.push("M=0");
    output.push("@DONE." + counter);
    output.push("0;JMP");
    output.push("(TRUTHY." + counter + ")");
    output.push("@SP");
    output.push("A=M-1");
    output.push("M=-1");
    output.push("(DONE." + counter + ")");
    counter++;
  } else {
    console.log("UNKNOWN");
  }
}

writeFileSync(file.replace(/\.vm$/, ".asm"), output.join("\n"));
