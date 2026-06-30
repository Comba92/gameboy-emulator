import * as fs from 'fs'

let file = fs.readFileSync('./instr.json')
let json = JSON.parse(file.toString())

const extract = (instr) => {
  let ops = Object.keys(instr)
  let names = [... new Set(
    Object.values(instr).map(v => v.mnemonic)
  )]

  let res = names.map(n => {
    let opcodes = []
    Object.entries(instr).forEach(([op, i]) => {
      if (i.mnemonic === n) {
        let opcode = parseInt(op, 16)
        opcodes.push('0x' + opcode.toString(16))
      }
    })

    return [n, opcodes]
  })

  return res
}

let res1 = extract(json.unprefixed)

res1 = res1.map(i => {
  let s = `${i[1].join(' | ')} => self.${i[0].toLowerCase()}(&instr.operands),`
  return s
}).join('\n')

let res2 = extract(json.cbprefixed)

res2 = res2.map(i => {
  let s = `${i[1].join(' | ')} => self.${i[0].toLowerCase()}(&instr.operands),`
  return s
}).join('\n')

console.log(res1, res2)