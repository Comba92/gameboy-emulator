import * as fs from 'fs'

let file = fs.readFileSync('./utils/instr.json')
let json = JSON.parse(file.toString())

const JUMPS = ["JR", "JP", "CALL"]
const NO_WRITE = ["ADD", "ADC", "SUB", "SBC", "CP", "BIT", "JP", "JR", "CALL"]
const A_DST = ["ADC", "ADD", "SUB", "SBC", "AND", "OR", "XOR", "CP"]

function remapOperand(instrName, operand) {
  let res = ''
  switch (operand.name) {
    case "A": 
    case "B": 
    case "D": 
    case "E": 
    case "F": 
    case "H": 
    case "L": 
      res = operand.name
      break

    case "C":
      if (JUMPS.includes(instrName)) {
        res = "CARRY"
      } else if (!operand.immediate) {
        res = "c_indirect"
      } else {
        res = "C"
      }
      break

    case "AF": 
    case "BC": 
    case "DE": 
    case "HL": 
    case "SP":
      res = operand.name
      if (operand.increment) {
        res += "_inc"
      } else if (operand.decrement) {
        res += "_dec"
      }

      if (!operand.immediate) {
        res += "_indirect"
      }
      break

    case "n8":
    case "n16":
    case "e8":
    case "a8":
    case "a16":   
      if (operand.immediate) {
        res += 'immediate' + operand.name.slice(1)
      } else {
        res += 'indirect'
          + (operand.name.includes('n') ? '_zero' : '_abs')
          + operand.name.slice(1)
      }
      if (operand.increment) {
        res += "_inc"
      } else if (operand.decrement) {
        res += "_dec"
      }

      break

    case "N": res = "N"; break;
    case "H": res = "HCARRY"; break;
    case "Z": res = "Z"; break;
    case "NZ": res = "NZ"; break;
    case "NC": res = "NCARRY"; break;
    case "NH": res = "NHCARRY"; break;

    default: 
      if (operand.name.startsWith("$")) { // rst
        res = operand.name.slice(1)
      } else if (operand) { // bit
        res = operand.name
      }
  }

  return res.toLowerCase()
}

let unprefixed = json.unprefixed
for (let [opcode, instr] of Object.entries(unprefixed)) {
  let name = instr.mnemonic
  if (A_DST.includes(name)) {
    instr.operands.shift(1)
  }
  
  let self = ''
  if (!name.includes('RST') && !name.includes('BIT')) {
    self = 'Self::'
  }
  
  let write = (instr.operands.length > 1 && !NO_WRITE.includes(name)) || ['INC', 'DEC'].includes(name)
  let operands = instr.operands.map((o, i) => {
    return self + ((write && i == 0) ? 'set_' : '')
      + remapOperand(name, o)
  })

  if (['INC', 'DEC'].includes(name)) {
    operands.push(operands[0])
    operands[1] = operands[1].replace('set_', '')
  }

  console.log(`${opcode} => self.${name.toLowerCase()}(${operands}),`)
}

let cbprefixed = json.cbprefixed
for (let [opcode, instr] of Object.entries(cbprefixed)) {
  let name = instr.mnemonic
  if (A_DST.includes(name)) {
    instr.operands.shift(1)
  }

  
  let write = (instr.operands.length > 1 && !NO_WRITE.includes(name)) || ['RLC', 'RRC', 'RL', 'SLA', 'RR', 'RRC', 'SRA', 'SRL', 'SWAP'].includes(name)
  let operands = instr.operands.map((o, i) => {
    let self = isNaN(o.name) ? 'Self::' : ''
    return self 
      + ((write && i == 0) && isNaN(o.name) ? 'set_' : '') 
      + remapOperand(name, o)
  })

  if (['RLC', 'RRC', 'RL', 'SLA', 'RR', 'RRC', 'SRA', 'SRL', 'SWAP'].includes(name)) {
    operands.push(operands[0])
    operands[1] = operands[1].replace('set_', '')
  }

  if (['RES', 'SET'].includes(name)) {
    operands.push(operands[1])
    operands[1] = operands[1].replace('::', '::set_')
  }

  console.log(`${opcode} => self.${name.toLowerCase()}(${operands}),`)
}