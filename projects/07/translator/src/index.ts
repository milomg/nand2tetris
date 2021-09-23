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
    if (pieces[0] == "push" && pieces[1] == "constant"){
        output.push("@"+pieces[2]);
        output.push("D=A");
        output.push("@SP");
        output.push("A=M");
        output.push("M=D");
        output.push("@SP");
        output.push("M=M+1");
    } else if (pieces[0] == "add") {
        output.push("@SP");
        output.push("AM=M-1");
        output.push("D=M");
        output.push("A=A-1");
        output.push("M=M+D");
    } else if (pieces[0] == "sub") {
        output.push("@SP");
        output.push("AM=M-1");
        output.push("D=M");
        output.push("A=A-1");
        output.push("M=M-D");
    } else if (pieces[0] == "and") {
        output.push("@SP");
        output.push("AM=M-1");
        output.push("D=M");
        output.push("A=A-1");
        output.push("M=D&M");
    } else if (pieces[0] == "or") {
        output.push("@SP");
        output.push("AM=M-1");
        output.push("D=M");
        output.push("A=A-1");
        output.push("M=D|M");
    } else if (pieces[0] == "neg") {
        output.push("@SP");
        output.push("A=M-1");
        output.push("M=-M");
    } else if (pieces[0] == "not") {
        output.push("@SP");
        output.push("A=M-1");
        output.push("M=!M");
    } else if (pieces[0] == "eq") {
        output.push("@SP");
        output.push("AM=M-1");
        output.push("D=M");
        output.push("A=A-1");
        output.push("D=D-M");
        output.push("(TRUTHY."+counter+")")
        output.push("(FALSY."+counter+")")
        output.push("(DONE."+counter+")")
        counter++;
    }
}

console.log(output.join("\n"));
