import * as fs from "fs";

let file = fs.readFileSync("./utils/sm82_instructions.json");
let json = JSON.parse(file.toString());

let opcodes = json.opcodes;
let map = opcodes
  .map((x) => `(${x.opcode}, ${x.name}),\n`)
  .reduce((acc, val) => acc + val, "");

let names = opcodes.map((x) => `${x.name},\n`);
names = new Set(names);
names = Array.from(names).reduce((acc, val) => acc + val, "");

console.log(names);
console.log(map);
