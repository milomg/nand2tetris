import { readFileSync, writeFileSync } from "fs";
import { parse } from "path";

// TODO: Is it an issue that files that end in // are not trimmed correctly?
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

// This is made by just concatenating location+offset.
const inlineTable: { [key: string]: string } = {
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

// This is an array of strings because that turns out to be faster than concatenating to a large string.
const output: string[] = [];

// Write is a tag template literal which means that it can be called using write`bla bla` instead of write("bla bla").
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

// Deal with some typescript issues so that I can use dependent types :)
function includes<S, R extends S>(array: ReadonlyArray<R>, searchElement: S): searchElement is R {
  return array.includes(searchElement as unknown as R);
}

// Push the value of a variable at location and offset onto the stack.
function writePush(location: string, offset: string) {
  // You may notice "as const" in a few places throughout the code, that is a way to get TypeScript to use dependent types.
  if (!includes(["constant", "local", "argument", "this", "that", "temp", "pointer", "static"] as const, location)) {
    throw new Error(`Unknown location (${location}) in line: push ${location} ${offset}`);
  }

  if (location == "pointer" || location == "temp") {
    // Unfortunately I wasn't able to dependently type inlineTable, so I check if it is undefined and throw an error.
    const address = inlineTable[location + offset];
    if (address == undefined) {
      throw new Error(`Unknown offset (${offset}) in line: push ${location} ${offset}`);
    }

    write`@${address}
          D=M`;
  } else if (location == "constant") {
    write`@${offset}
          D=A`;
  } else if (location == "static") {
    // This is described on page 143
    write`@${fileName}.${offset}
          D=M`;
  } else {
    // In the normal case, we add the offset to the address stored in table[location].
    write`@${table[location]}
          D=M
          @${offset}
          A=D+A
          D=M`;
  }

  // Push the value of D onto the stack
  write`@SP
        M=M+1
        A=M-1
        M=D`;
}

function writePop(location: string, offset: string) {
  if (!includes(["local", "argument", "this", "that", "temp", "pointer", "static"] as const, location)) {
    throw new Error(`Unknown location (${location}) in line: pop ${location} ${offset}`);
  }

  // In the normal case, we have to write to R13 because we have to add address+constant (we special case the other cases to avoid that).
  const normalCase = includes(["local", "argument", "this", "that"] as const, location);

  if (normalCase) {
    write`@${table[location]}
          D=M
          @${offset}
          D=D+A
          @R13
          M=D`;
  }

  // We pop the top value of the stack to D.
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
      // Fun feature of VSCode time, hover over location to view the possible options.
      const address = inlineTable[location + offset];
      if (address == undefined) {
        throw new Error(`Unknown offset (${offset}) in line: pop ${location} ${offset}`);
      }
      write`@${address}`;
    }
  }

  // Finally, we end by writing D to the address.
  write`M=D`;
}

// Arithmetic operations pop the last items off the stack and apply the operation to the item below.
function writeArithmetic(type: "add" | "sub" | "and" | "or") {
  const arithmeticTable = { add: "+", sub: "-", and: "&", or: "|" };
  write`@SP
        AM=M-1
        D=M
        A=A-1
        M=M${arithmeticTable[type]}D`;
}

// Bit doesn't need to change the stack pointer, so we just go to *(SP-1) and then apply the bit operation.
function writeBit(type: "neg" | "not") {
  const bitTable = { neg: "-", not: "!" };
  write`@SP
        A=M-1
        M=${bitTable[type]}M`;
}

// Comparison operations pop the last items off the stack and then jump to the label to decide whether to push -1.
function writeBoolean(type: "eq" | "lt" | "gt", counter: number) {
  const booleanTable = { eq: "JEQ", lt: "JLT", gt: "JGT" };
  write`@SP
        AM=M-1
        D=M
        A=A-1
        D=M-D
        M=0
        @TRUTHY.${fileName}.${counter}
        D;${booleanTable[type]}
        @DONE.${fileName}.${counter}
        0;JMP
        (TRUTHY.${fileName}.${counter})
        @SP
        A=M-1
        M=-1
        (DONE.${fileName}.${counter})`;
}

let counter = 0;
for (const line of lines) {
  const [type, location, offset] = line.split(/\s+/g); // Split the line into three parts, if there are fewer than three parts, the others are undefined.
  if ("push" == type) {
    writePush(location, offset);
  } else if ("pop" == type) {
    writePop(location, offset);
  } else if (includes(["add", "sub", "and", "or"] as const, type)) {
    writeArithmetic(type);
  } else if (includes(["neg", "not"] as const, type)) {
    writeBit(type);
  } else if (includes(["eq", "lt", "gt"] as const, type)) {
    writeBoolean(type, counter);
    counter++;
  } else {
    throw new Error(`Unknown command: ${line}`);
  }
}

writeFileSync(file.replace(/\.vm$/, ".asm"), output.join("\n"));
