package virt6502

import "fmt"

const crashOnUndocumentOpcode = false

func (vc *Virt6502) undocumentedOpcode() {
	if crashOnUndocumentOpcode {
		vc.Err(fmt.Errorf("Undocumented opcode 0x%02x at 0x%04x", vc.Read(vc.PC), vc.PC))
	}
}

func (vc *Virt6502) opFn(cycles uint, instLen uint16, fn func()) {
	fn()
	vc.PC += instLen
	vc.RunCycles(cycles)
}

func (vc *Virt6502) setRegOp(numCycles uint, instLen uint16, dst *byte, src byte, flagFn func(byte)) {
	*dst = src
	vc.PC += instLen
	vc.RunCycles(numCycles)
	flagFn(*dst)
}
func (vc *Virt6502) storeOp(numCycles uint, instLen uint16, addr uint16, val byte, flagFn func(byte)) {
	vc.Write(addr, val)
	vc.PC += instLen
	vc.RunCycles(numCycles)
	flagFn(val)
}
func (vc *Virt6502) cmpOp(nCycles uint, instLen uint16, reg byte, val byte) {
	vc.RunCycles(nCycles)
	vc.PC += instLen
	vc.setZeroNeg(reg - val)
	vc.setCarryFlag(reg >= val)
}
func (vc *Virt6502) jmpOp(nCycles uint, instLen uint16, newPC uint16) {
	vc.RunCycles(nCycles)
	vc.PC = newPC
}
func (vc *Virt6502) branchOpRel(test bool) {
	if test {
		offs := int8(vc.Read(vc.PC + 1))
		newPC := uint16(int(vc.PC+2) + int(offs))
		if newPC&0xff00 != vc.PC&0xff00 {
			vc.RunCycles(4)
		} else {
			vc.RunCycles(3)
		}
		vc.PC = newPC
	} else {
		vc.opFn(2, 2, func() {})
	}
}

// vc.PC must be in right place, obviously
func (vc *Virt6502) getYPostIndexedAddr() (uint16, uint) {
	zPageLowAddr := uint16(vc.Read(vc.PC + 1))
	zPageHighAddr := uint16(vc.Read(vc.PC+1) + 1) // wraps at 0xff
	baseAddr := (uint16(vc.Read(zPageHighAddr)) << 8) | uint16(vc.Read(zPageLowAddr))
	addr := baseAddr + uint16(vc.Y)
	if addr&0xff00 != baseAddr&0xff00 { // if not same page, takes extra cycle
		return addr, 1
	}
	return addr, 0
}
func (vc *Virt6502) getXPreIndexedAddr() uint16 {
	zPageLowAddr := uint16(vc.Read(vc.PC+1) + vc.X)      // wraps at 0xff
	zPageHighAddr := uint16(vc.Read(vc.PC+1) + vc.X + 1) // wraps at 0xff
	return (uint16(vc.Read(zPageHighAddr)) << 8) | uint16(vc.Read(zPageLowAddr))
}
func (vc *Virt6502) getZeroPageAddr() uint16 {
	return uint16(vc.Read(vc.PC + 1))
}
func (vc *Virt6502) getIndexedZeroPageAddr(idx byte) uint16 {
	return uint16(vc.Read(vc.PC+1) + idx) // wraps at 0xff
}
func (vc *Virt6502) getAbsoluteAddr() uint16 {
	return vc.Read16(vc.PC + 1)
}
func (vc *Virt6502) getIndexedAbsoluteAddr(idx byte) (uint16, uint) {
	base := vc.Read16(vc.PC + 1)
	addr := base + uint16(idx)
	if base&0xff00 != addr&0xff00 { // if not same page, takes extra cycle
		return addr, 1
	}
	return addr, 0
}
func (vc *Virt6502) getIndirectJmpAddr() uint16 {
	// hw bug! similar to other indexing wrapping issues...
	operandAddr := vc.getAbsoluteAddr()
	highAddr := (operandAddr & 0xff00) | ((operandAddr + 1) & 0xff) // lo-byte wraps at 0xff
	return (uint16(vc.Read(highAddr)) << 8) | uint16(vc.Read(operandAddr))
}

var opcodeNames = []string{

	// LOWERCASE == undocumented

	// 0      1      2      3      4      5      6      7      8      9      A      B      C      D      E      F
	"BRK", "ORA", "XXX", "slo", "nop", "ORA", "ASL", "slo", "PHP", "ORA", "ASL", "aac", "XXX", "ORA", "ASL", "slo",
	"BPL", "ORA", "XXX", "slo", "nop", "ORA", "ASL", "slo", "CLC", "ORA", "XXX", "slo", "XXX", "ORA", "ASL", "slo",
	"JSR", "AND", "XXX", "XXX", "BIT", "AND", "ROL", "XXX", "PLP", "AND", "ROL", "aac", "BIT", "AND", "ROL", "XXX",
	"BMI", "AND", "XXX", "XXX", "nop", "AND", "ROL", "XXX", "SEC", "AND", "XXX", "XXX", "XXX", "AND", "ROL", "XXX",
	"RTI", "EOR", "XXX", "XXX", "nop", "EOR", "LSR", "XXX", "PHA", "EOR", "LSR", "asr", "JMP", "EOR", "LSR", "XXX",
	"BVC", "EOR", "XXX", "XXX", "nop", "EOR", "LSR", "XXX", "CLI", "EOR", "XXX", "XXX", "XXX", "EOR", "LSR", "XXX",
	"RTS", "ADC", "XXX", "XXX", "nop", "ADC", "ROR", "XXX", "PLA", "ADC", "ROR", "arr", "JMP", "ADC", "ROR", "XXX",
	"BVS", "ADC", "XXX", "XXX", "nop", "ADC", "ROR", "XXX", "SEI", "ADC", "XXX", "XXX", "XXX", "ADC", "ROR", "XXX",
	"nop", "STA", "nop", "XXX", "STY", "STA", "STX", "XXX", "DEY", "XXX", "TXA", "XXX", "STY", "STA", "STX", "XXX",
	"BCC", "STA", "XXX", "XXX", "STY", "STA", "STX", "XXX", "TYA", "STA", "TXS", "XXX", "XXX", "STA", "XXX", "XXX",
	"LDY", "LDA", "LDX", "XXX", "LDY", "LDA", "LDX", "XXX", "TAY", "LDA", "TAX", "XXX", "LDY", "LDA", "LDX", "XXX",
	"BCS", "LDA", "XXX", "XXX", "LDY", "LDA", "LDX", "XXX", "CLV", "LDA", "TSX", "las", "LDY", "LDA", "LDX", "XXX",
	"CPY", "CMP", "nop", "XXX", "CPY", "CMP", "DEC", "XXX", "INY", "CMP", "DEX", "axs", "CPY", "CMP", "DEC", "XXX",
	"BNE", "CMP", "XXX", "XXX", "nop", "CMP", "DEC", "XXX", "CLD", "CMP", "XXX", "XXX", "XXX", "CMP", "DEC", "XXX",
	"CPX", "SBC", "nop", "isc", "CPX", "SBC", "INC", "isc", "INX", "SBC", "NOP", "XXX", "CPX", "SBC", "INC", "isc",
	"BEQ", "SBC", "XXX", "isc", "nop", "SBC", "INC", "isc", "SED", "SBC", "XXX", "isc", "XXX", "SBC", "INC", "isc",
}

func (vc *Virt6502) stepOpcode() {

	opcode := vc.Read(vc.PC)
	switch opcode {
	case 0x00: // BRK
		vc.opFn(7, 1, func() { vc.BRK = true })
	case 0x01: // ORA (indirect,x)
		addr := vc.getXPreIndexedAddr()
		vc.setRegOp(6, 2, &vc.A, vc.A|vc.Read(addr), vc.setZeroNeg)
	case 0x03: // SLO (indirect,x) (UNDOCUMENTED)
		vc.undocumentedOpcode()
		addr := vc.getXPreIndexedAddr()
		shifted := vc.aslAndSetFlags(vc.Read(addr))
		vc.storeOp(8, 2, addr, shifted, vc.setNoFlags)
		vc.setRegOp(0, 0, &vc.A, vc.A|shifted, vc.setZeroNeg)
	case 0x04: // 2-nop (UNDOCUMENTED)
		vc.opFn(3, 2, vc.undocumentedOpcode)
	case 0x05: // ORA zeropage
		addr := vc.getZeroPageAddr()
		vc.setRegOp(3, 2, &vc.A, vc.A|vc.Read(addr), vc.setZeroNeg)
	case 0x06: // ASL zeropage
		addr := vc.getZeroPageAddr()
		vc.storeOp(5, 2, addr, vc.aslAndSetFlags(vc.Read(addr)), vc.setNoFlags)
	case 0x07: // SLO zeropage (UNDOCUMENTED)
		vc.undocumentedOpcode()
		addr := vc.getZeroPageAddr()
		shifted := vc.aslAndSetFlags(vc.Read(addr))
		vc.storeOp(5, 2, addr, shifted, vc.setNoFlags)
		vc.setRegOp(0, 0, &vc.A, vc.A|shifted, vc.setZeroNeg)
	case 0x08: // PHP
		vc.opFn(3, 1, func() { vc.Push(vc.P | FlagOnStack | FlagBrk) })
	case 0x09: // ORA imm
		addr := vc.PC + 1
		vc.setRegOp(2, 2, &vc.A, vc.A|vc.Read(addr), vc.setZeroNeg)
	case 0x0a: // ASL A
		vc.opFn(2, 1, func() { vc.A = vc.aslAndSetFlags(vc.A) })
	case 0x0b: // AAC imm (UNDOCUMENTED)
		result := vc.Read(vc.PC+1) & vc.A
		vc.setRegOp(2, 2, &vc.A, result, vc.setZeroNeg)
		vc.setCarryFlag(result&0x80 != 0)
	case 0x0c: // 3-nop (UNDOCUMENTED)
		vc.opFn(4, 3, vc.undocumentedOpcode)
	case 0x0d: // ORA absolute
		addr := vc.getAbsoluteAddr()
		vc.setRegOp(4, 3, &vc.A, vc.A|vc.Read(addr), vc.setZeroNeg)
	case 0x0e: // ASL absolute
		addr := vc.getAbsoluteAddr()
		vc.storeOp(6, 3, addr, vc.aslAndSetFlags(vc.Read(addr)), vc.setNoFlags)
	case 0x0f: // SLO absolute (UNDOCUMENTED)
		vc.undocumentedOpcode()
		addr := vc.getAbsoluteAddr()
		shifted := vc.aslAndSetFlags(vc.Read(addr))
		vc.storeOp(6, 3, addr, shifted, vc.setNoFlags)
		vc.setRegOp(0, 0, &vc.A, vc.A|shifted, vc.setZeroNeg)

	case 0x10: // BPL
		vc.branchOpRel(vc.P&FlagNeg == 0)
	case 0x11: // ORA (indirect),y
		addr, cycles := vc.getYPostIndexedAddr()
		vc.setRegOp(5+cycles, 2, &vc.A, vc.A|vc.Read(addr), vc.setZeroNeg)
	case 0x13: // SLO (indirect),y (UNDOCUMENTED)
		vc.undocumentedOpcode()
		addr, _ := vc.getYPostIndexedAddr()
		shifted := vc.aslAndSetFlags(vc.Read(addr))
		vc.storeOp(8, 2, addr, shifted, vc.setNoFlags)
		vc.setRegOp(0, 0, &vc.A, vc.A|shifted, vc.setZeroNeg)
	case 0x14: // 2-nop (UNDOCUMENTED)
		vc.opFn(4, 2, vc.undocumentedOpcode)
	case 0x15: // ORA zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.setRegOp(4, 2, &vc.A, vc.A|vc.Read(addr), vc.setZeroNeg)
	case 0x16: // ASL zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.storeOp(6, 2, addr, vc.aslAndSetFlags(vc.Read(addr)), vc.setNoFlags)
	case 0x17: // SLO zeropage,x (UNDOCUMENTED)
		vc.undocumentedOpcode()
		addr := vc.getIndexedZeroPageAddr(vc.X)
		shifted := vc.aslAndSetFlags(vc.Read(addr))
		vc.storeOp(6, 2, addr, shifted, vc.setNoFlags)
		vc.setRegOp(0, 0, &vc.A, vc.A|shifted, vc.setZeroNeg)
	case 0x18: // CLC
		vc.opFn(2, 1, func() { vc.P &^= FlagCarry })
	case 0x19: // ORA absolute,y
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.Y)
		vc.setRegOp(4+cycles, 3, &vc.A, vc.A|vc.Read(addr), vc.setZeroNeg)
	case 0x1a: // 1-nop (UNDOCUMENTED)
		vc.opFn(2, 1, vc.undocumentedOpcode)
	case 0x1b: // SLO absolute,y (UNDOCUMENTED)
		vc.undocumentedOpcode()
		addr, _ := vc.getIndexedAbsoluteAddr(vc.Y)
		shifted := vc.aslAndSetFlags(vc.Read(addr))
		vc.storeOp(7, 3, addr, shifted, vc.setNoFlags)
		vc.setRegOp(0, 0, &vc.A, vc.A|shifted, vc.setZeroNeg)
	case 0x1c: // 3-nop (UNDOCUMENTED)
		_, cycles := vc.getIndexedAbsoluteAddr(vc.X)
		vc.opFn(4+cycles, 3, vc.undocumentedOpcode)
	case 0x1d: // ORA absolute,x
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.X)
		vc.setRegOp(4+cycles, 3, &vc.A, vc.A|vc.Read(addr), vc.setZeroNeg)
	case 0x1e: // ASL absolute,x
		addr, _ := vc.getIndexedAbsoluteAddr(vc.X)
		vc.storeOp(7, 3, addr, vc.aslAndSetFlags(vc.Read(addr)), vc.setNoFlags)
	case 0x1f: // SLO absolute,x (UNDOCUMENTED)
		vc.undocumentedOpcode()
		addr, _ := vc.getIndexedAbsoluteAddr(vc.X)
		shifted := vc.aslAndSetFlags(vc.Read(addr))
		vc.storeOp(7, 3, addr, shifted, vc.setNoFlags)
		vc.setRegOp(0, 0, &vc.A, vc.A|shifted, vc.setZeroNeg)

	case 0x20: // JSR (jump and store return addr)
		vc.Push16(vc.PC + 2)
		vc.jmpOp(6, 3, vc.getAbsoluteAddr())
	case 0x21: // AND (indirect,x)
		addr := vc.getXPreIndexedAddr()
		vc.setRegOp(6, 2, &vc.A, vc.A&vc.Read(addr), vc.setZeroNeg)
	case 0x24: // BIT zeropage
		addr := vc.getZeroPageAddr()
		vc.opFn(3, 2, func() { vc.bitAndSetFlags(vc.Read(addr)) })
	case 0x25: // AND zeropage
		addr := vc.getZeroPageAddr()
		vc.setRegOp(3, 2, &vc.A, vc.A&vc.Read(addr), vc.setZeroNeg)
	case 0x26: // ROL zeropage
		addr := vc.getZeroPageAddr()
		vc.storeOp(5, 2, addr, vc.rolAndSetFlags(vc.Read(addr)), vc.setNoFlags)
	case 0x28: // PLP
		flags := vc.Pop() &^ (FlagBrk | FlagOnStack)
		vc.setRegOp(4, 1, &vc.P, flags, vc.setNoFlags)
	case 0x29: // AND imm
		addr := vc.PC + 1
		vc.setRegOp(2, 2, &vc.A, vc.A&vc.Read(addr), vc.setZeroNeg)
	case 0x2a: // ROL A
		vc.opFn(2, 1, func() { vc.A = vc.rolAndSetFlags(vc.A) })
	case 0x2b: // AAC imm (UNDOCUMENTED)
		result := vc.Read(vc.PC+1) & vc.A
		vc.setRegOp(2, 2, &vc.A, result, vc.setZeroNeg)
		vc.setCarryFlag(result&0x80 != 0)
	case 0x2c: // BIT absolute
		addr := vc.getAbsoluteAddr()
		vc.opFn(4, 3, func() { vc.bitAndSetFlags(vc.Read(addr)) })
	case 0x2d: // AND absolute
		addr := vc.getAbsoluteAddr()
		vc.setRegOp(4, 3, &vc.A, vc.A&vc.Read(addr), vc.setZeroNeg)
	case 0x2e: // ROL absolute
		addr := vc.getAbsoluteAddr()
		vc.storeOp(6, 3, addr, vc.rolAndSetFlags(vc.Read(addr)), vc.setNoFlags)

	case 0x30: // BMI
		vc.branchOpRel(vc.P&FlagNeg == FlagNeg)
	case 0x31: // AND (indirect),y
		addr, cycles := vc.getYPostIndexedAddr()
		vc.setRegOp(5+cycles, 2, &vc.A, vc.A&vc.Read(addr), vc.setZeroNeg)
	case 0x34: // 2-nop (UNDOCUMENTED)
		vc.opFn(4, 2, vc.undocumentedOpcode)
	case 0x35: // AND zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.setRegOp(4, 2, &vc.A, vc.A&vc.Read(addr), vc.setZeroNeg)
	case 0x36: // ROL zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.storeOp(6, 2, addr, vc.rolAndSetFlags(vc.Read(addr)), vc.setNoFlags)
	case 0x38: // SEC
		vc.opFn(2, 1, func() { vc.P |= FlagCarry })
	case 0x39: // AND absolute,y
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.Y)
		vc.setRegOp(4+cycles, 3, &vc.A, vc.A&vc.Read(addr), vc.setZeroNeg)
	case 0x3a: // 1-nop (UNDOCUMENTED)
		vc.opFn(2, 1, vc.undocumentedOpcode)
	case 0x3c: // 3-nop (UNDOCUMENTED)
		_, cycles := vc.getIndexedAbsoluteAddr(vc.X)
		vc.opFn(4+cycles, 3, vc.undocumentedOpcode)
	case 0x3d: // AND absolute,x
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.X)
		vc.setRegOp(4+cycles, 3, &vc.A, vc.A&vc.Read(addr), vc.setZeroNeg)
	case 0x3e: // ROL absolute,x
		addr, _ := vc.getIndexedAbsoluteAddr(vc.X)
		vc.storeOp(7, 3, addr, vc.rolAndSetFlags(vc.Read(addr)), vc.setNoFlags)

	case 0x40: // RTI (return from interrupt)
		vc.P = vc.Pop() &^ (FlagBrk | FlagOnStack)
		vc.LastStepsP = vc.P                         // no lag from RTI
		vc.opFn(6, 0, func() { vc.PC = vc.Pop16() }) // real instLen 1, but we don't want to step past newPC (unlike RTS)
	case 0x41: // EOR (indirect,x)
		addr := vc.getXPreIndexedAddr()
		vc.setRegOp(6, 2, &vc.A, vc.A^vc.Read(addr), vc.setZeroNeg)
	case 0x44: // 2-nop (UNDOCUMENTED)
		vc.opFn(3, 2, vc.undocumentedOpcode)
	case 0x45: // EOR zeropage
		addr := vc.getZeroPageAddr()
		vc.setRegOp(3, 2, &vc.A, vc.A^vc.Read(addr), vc.setZeroNeg)
	case 0x46: // LSR zeropage
		addr := vc.getZeroPageAddr()
		vc.storeOp(5, 2, addr, vc.lsrAndSetFlags(vc.Read(addr)), vc.setNoFlags)
	case 0x48: // PHA
		vc.opFn(3, 1, func() { vc.Push(vc.A) })
	case 0x49: // EOR imm
		addr := vc.PC + 1
		vc.setRegOp(2, 2, &vc.A, vc.A^vc.Read(addr), vc.setZeroNeg)
	case 0x4a: // LSR A
		vc.opFn(2, 1, func() { vc.A = vc.lsrAndSetFlags(vc.A) })
	case 0x4b: // ASR imm (UNDOCUMENTED)
		addr := vc.PC + 1
		vc.setRegOp(2, 2, &vc.A, vc.A&vc.Read(addr), vc.setZeroNeg)
		vc.A = vc.lsrAndSetFlags(vc.A)
	case 0x4c: // JMP absolute
		vc.jmpOp(3, 3, vc.getAbsoluteAddr())
	case 0x4d: // EOR absolute
		addr := vc.getAbsoluteAddr()
		vc.setRegOp(4, 3, &vc.A, vc.A^vc.Read(addr), vc.setZeroNeg)
	case 0x4e: // LSR absolute
		addr := vc.getAbsoluteAddr()
		vc.storeOp(6, 3, addr, vc.lsrAndSetFlags(vc.Read(addr)), vc.setNoFlags)

	case 0x50: // BVC
		vc.branchOpRel(vc.P&FlagOverflow == 0)
	case 0x51: // EOR (indirect),y
		addr, cycles := vc.getYPostIndexedAddr()
		vc.setRegOp(5+cycles, 2, &vc.A, vc.A^vc.Read(addr), vc.setZeroNeg)
	case 0x54: // 2-nop (UNDOCUMENTED)
		vc.opFn(4, 2, vc.undocumentedOpcode)
	case 0x55: // EOR zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.setRegOp(4, 2, &vc.A, vc.A^vc.Read(addr), vc.setZeroNeg)
	case 0x56: // LSR zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.storeOp(6, 2, addr, vc.lsrAndSetFlags(vc.Read(addr)), vc.setNoFlags)
	case 0x58: // CLI
		vc.opFn(2, 1, func() { vc.P &^= FlagIrqDisabled })
	case 0x59: // EOR absolute,y
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.Y)
		vc.setRegOp(4+cycles, 3, &vc.A, vc.A^vc.Read(addr), vc.setZeroNeg)
	case 0x5a: // 1-nop (UNDOCUMENTED)
		vc.opFn(2, 1, vc.undocumentedOpcode)
	case 0x5c: // 3-nop (UNDOCUMENTED)
		_, cycles := vc.getIndexedAbsoluteAddr(vc.X)
		vc.opFn(4+cycles, 3, vc.undocumentedOpcode)
	case 0x5d: // EOR absolute,x
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.X)
		vc.setRegOp(4+cycles, 3, &vc.A, vc.A^vc.Read(addr), vc.setZeroNeg)
	case 0x5e: // LSR absolute,x
		addr, _ := vc.getIndexedAbsoluteAddr(vc.X)
		vc.storeOp(7, 3, addr, vc.lsrAndSetFlags(vc.Read(addr)), vc.setNoFlags)

	case 0x60: // RTS (return from subroutine)
		vc.opFn(6, 1, func() { vc.PC = vc.Pop16() }) // opFn adds 1 to PC, so does real 6502
	case 0x61: // ADC (indirect,x)
		addr := vc.getXPreIndexedAddr()
		vc.opFn(6, 2, func() { vc.A = vc.adcAndSetFlags(vc.Read(addr)) })
	case 0x64: // 2-nop (UNDOCUMENTED)
		vc.opFn(3, 2, vc.undocumentedOpcode)
	case 0x65: // ADC zeropage
		addr := vc.getZeroPageAddr()
		vc.opFn(3, 2, func() { vc.A = vc.adcAndSetFlags(vc.Read(addr)) })
	case 0x66: // ROR zeropage
		addr := vc.getZeroPageAddr()
		vc.storeOp(5, 2, addr, vc.rorAndSetFlags(vc.Read(addr)), vc.setNoFlags)
	case 0x68: // PLA
		vc.setRegOp(4, 1, &vc.A, vc.Pop(), vc.setZeroNeg)
	case 0x69: // ADC imm
		addr := vc.PC + 1
		vc.opFn(2, 2, func() { vc.A = vc.adcAndSetFlags(vc.Read(addr)) })
	case 0x6a: // ROR A
		vc.opFn(2, 1, func() { vc.A = vc.rorAndSetFlags(vc.A) })
	case 0x6b: // ARR imm (UNDOCUMENTED)
		addr := vc.PC+1
		vc.setRegOp(2, 2, &vc.A, vc.A&vc.Read(addr), vc.setZeroNeg)
		vc.A = vc.rorAndSetFlags(vc.A)
		vc.setCarryFlag(vc.A&0x40 != 0)
		vc.setOverflowFlag(((vc.A<<1)^vc.A)&0x40 != 0)
	case 0x6c: // JMP (indirect)
		vc.jmpOp(5, 3, vc.getIndirectJmpAddr())
	case 0x6d: // ADC absolute
		addr := vc.getAbsoluteAddr()
		vc.opFn(4, 3, func() { vc.A = vc.adcAndSetFlags(vc.Read(addr)) })
	case 0x6e: // ROR absolute
		addr := vc.getAbsoluteAddr()
		vc.storeOp(6, 3, addr, vc.rorAndSetFlags(vc.Read(addr)), vc.setNoFlags)

	case 0x70: // BVS
		vc.branchOpRel(vc.P&FlagOverflow == FlagOverflow)
	case 0x71: // ADC (indirect),y
		addr, cycles := vc.getYPostIndexedAddr()
		vc.opFn(5+cycles, 2, func() { vc.A = vc.adcAndSetFlags(vc.Read(addr)) })
	case 0x74: // 2-nop (UNDOCUMENTED)
		vc.opFn(4, 2, vc.undocumentedOpcode)
	case 0x75: // ADC zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.opFn(4, 2, func() { vc.A = vc.adcAndSetFlags(vc.Read(addr)) })
	case 0x76: // ROR zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.storeOp(6, 2, addr, vc.rorAndSetFlags(vc.Read(addr)), vc.setNoFlags)
	case 0x78: // SEI
		vc.opFn(2, 1, func() { vc.P |= FlagIrqDisabled })
	case 0x79: // ADC absolute,y
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.Y)
		vc.opFn(4+cycles, 3, func() { vc.A = vc.adcAndSetFlags(vc.Read(addr)) })
	case 0x7a: // 1-nop (UNDOCUMENTED)
		vc.opFn(2, 1, vc.undocumentedOpcode)
	case 0x7c: // 3-nop (UNDOCUMENTED)
		_, cycles := vc.getIndexedAbsoluteAddr(vc.X)
		vc.opFn(4+cycles, 3, vc.undocumentedOpcode)
	case 0x7d: // ADC absolute,x
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.X)
		vc.opFn(4+cycles, 3, func() { vc.A = vc.adcAndSetFlags(vc.Read(addr)) })
	case 0x7e: // ROR absolute,x
		addr, _ := vc.getIndexedAbsoluteAddr(vc.X)
		vc.storeOp(7, 3, addr, vc.rorAndSetFlags(vc.Read(addr)), vc.setNoFlags)

	case 0x80: // 2-nop (UNDOCUMENTED)
		vc.opFn(2, 2, vc.undocumentedOpcode)
	case 0x81: // STA (indirect,x)
		vc.storeOp(6, 2, vc.getXPreIndexedAddr(), vc.A, vc.setNoFlags)
	case 0x82: // 2-nop (UNDOCUMENTED)
		vc.opFn(2, 2, vc.undocumentedOpcode)
	case 0x84: // STY zeropage
		vc.storeOp(3, 2, vc.getZeroPageAddr(), vc.Y, vc.setNoFlags)
	case 0x85: // STA zeropage
		vc.storeOp(3, 2, vc.getZeroPageAddr(), vc.A, vc.setNoFlags)
	case 0x86: // STX zeropage
		vc.storeOp(3, 2, vc.getZeroPageAddr(), vc.X, vc.setNoFlags)
	case 0x88: // DEY
		vc.setRegOp(2, 1, &vc.Y, vc.Y-1, vc.setZeroNeg)
	case 0x89: // 2-nop (UNDOCUMENTED)
		vc.opFn(2, 2, vc.undocumentedOpcode)
	case 0x8a: // TXA
		vc.setRegOp(2, 1, &vc.A, vc.X, vc.setZeroNeg)
	case 0x8c: // STY absolute
		vc.storeOp(4, 3, vc.getAbsoluteAddr(), vc.Y, vc.setNoFlags)
	case 0x8d: // STA absolute
		vc.storeOp(4, 3, vc.getAbsoluteAddr(), vc.A, vc.setNoFlags)
	case 0x8e: // STX absolute
		vc.storeOp(4, 3, vc.getAbsoluteAddr(), vc.X, vc.setNoFlags)

	case 0x90: // BCC
		vc.branchOpRel(vc.P&FlagCarry == 0)
	case 0x91: // STA (indirect),y
		addr, _ := vc.getYPostIndexedAddr()
		vc.storeOp(6, 2, addr, vc.A, vc.setNoFlags)
	case 0x94: // STY zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.storeOp(4, 2, addr, vc.Y, vc.setNoFlags)
	case 0x95: // STA zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.storeOp(4, 2, addr, vc.A, vc.setNoFlags)
	case 0x96: // STX zeropage,y
		addr := vc.getIndexedZeroPageAddr(vc.Y)
		vc.storeOp(4, 2, addr, vc.X, vc.setNoFlags)
	case 0x98: // TYA
		vc.setRegOp(2, 1, &vc.A, vc.Y, vc.setZeroNeg)
	case 0x99: // STA absolute,y
		addr, _ := vc.getIndexedAbsoluteAddr(vc.Y)
		vc.storeOp(5, 3, addr, vc.A, vc.setNoFlags)
	case 0x9a: // TXS
		vc.setRegOp(2, 1, &vc.S, vc.X, vc.setNoFlags)
	case 0x9d: // STA absolute,x
		addr, _ := vc.getIndexedAbsoluteAddr(vc.X)
		vc.storeOp(5, 3, addr, vc.A, vc.setNoFlags)

	case 0xa0: // LDY imm
		addr := vc.PC + 1
		vc.setRegOp(2, 2, &vc.Y, vc.Read(addr), vc.setZeroNeg)
	case 0xa1: // LDA (indirect,x)
		addr := vc.getXPreIndexedAddr()
		vc.setRegOp(6, 2, &vc.A, vc.Read(addr), vc.setZeroNeg)
	case 0xa2: // LDX imm
		addr := vc.PC + 1
		vc.setRegOp(2, 2, &vc.X, vc.Read(addr), vc.setZeroNeg)
	case 0xa4: // LDY zeropage
		addr := vc.getZeroPageAddr()
		vc.setRegOp(3, 2, &vc.Y, vc.Read(addr), vc.setZeroNeg)
	case 0xa5: // LDA zeropage
		addr := vc.getZeroPageAddr()
		vc.setRegOp(3, 2, &vc.A, vc.Read(addr), vc.setZeroNeg)
	case 0xa6: // LDX zeropage
		addr := vc.getZeroPageAddr()
		vc.setRegOp(3, 2, &vc.X, vc.Read(addr), vc.setZeroNeg)
	case 0xa8: // TAY
		vc.setRegOp(2, 1, &vc.Y, vc.A, vc.setZeroNeg)
	case 0xa9: // LDA imm
		addr := vc.PC + 1
		vc.setRegOp(2, 2, &vc.A, vc.Read(addr), vc.setZeroNeg)
	case 0xaa: // TAX
		vc.setRegOp(2, 1, &vc.X, vc.A, vc.setZeroNeg)
	case 0xac: // LDY absolute
		addr := vc.getAbsoluteAddr()
		vc.setRegOp(4, 3, &vc.Y, vc.Read(addr), vc.setZeroNeg)
	case 0xad: // LDA absolute
		addr := vc.getAbsoluteAddr()
		vc.setRegOp(4, 3, &vc.A, vc.Read(addr), vc.setZeroNeg)
	case 0xae: // LDX absolute
		addr := vc.getAbsoluteAddr()
		vc.setRegOp(4, 3, &vc.X, vc.Read(addr), vc.setZeroNeg)

	case 0xb0: // BCS
		vc.branchOpRel(vc.P&FlagCarry == FlagCarry)
	case 0xb1: // LDA (indirect),y
		addr, cycles := vc.getYPostIndexedAddr()
		vc.setRegOp(5+cycles, 2, &vc.A, vc.Read(addr), vc.setZeroNeg)
	case 0xb4: // LDY zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.setRegOp(4, 2, &vc.Y, vc.Read(addr), vc.setZeroNeg)
	case 0xb5: // LDA zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.setRegOp(4, 2, &vc.A, vc.Read(addr), vc.setZeroNeg)
	case 0xb6: // LDX zeropage,y
		addr := vc.getIndexedZeroPageAddr(vc.Y)
		vc.setRegOp(4, 2, &vc.X, vc.Read(addr), vc.setZeroNeg)
	case 0xb8: // CLV
		vc.opFn(2, 1, func() { vc.P &^= FlagOverflow })
	case 0xb9: // LDA absolute, y
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.Y)
		vc.setRegOp(4+cycles, 3, &vc.A, vc.Read(addr), vc.setZeroNeg)
	case 0xba: // TSX
		vc.setRegOp(2, 1, &vc.X, vc.S, vc.setZeroNeg)
	case 0xbb: // LAS absolute, y (UNDOCUMENTED)
		vc.undocumentedOpcode()
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.Y)
		vc.setRegOp(4+cycles, 3, &vc.A, vc.Read(addr)&vc.S, vc.setZeroNeg)
	case 0xbc: // LDY absolute,x
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.X)
		vc.setRegOp(4+cycles, 3, &vc.Y, vc.Read(addr), vc.setZeroNeg)
	case 0xbd: // LDA absolute,x
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.X)
		vc.setRegOp(4+cycles, 3, &vc.A, vc.Read(addr), vc.setZeroNeg)
	case 0xbe: // LDX absolute,y
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.Y)
		vc.setRegOp(4+cycles, 3, &vc.X, vc.Read(addr), vc.setZeroNeg)

	case 0xc0: // CPY imm
		addr := vc.PC + 1
		vc.cmpOp(2, 2, vc.Y, vc.Read(addr))
	case 0xc1: // CMP (indirect,x)
		addr := vc.getXPreIndexedAddr()
		vc.cmpOp(6, 2, vc.A, vc.Read(addr))
	case 0xc2: // 2-nop (UNDOCUMENTED)
		vc.opFn(2, 2, vc.undocumentedOpcode)
	case 0xc4: // CPY zeropage
		addr := vc.getZeroPageAddr()
		vc.cmpOp(3, 2, vc.Y, vc.Read(addr))
	case 0xc5: // CMP zeropage
		addr := vc.getZeroPageAddr()
		vc.cmpOp(3, 2, vc.A, vc.Read(addr))
	case 0xc6: // DEC zeropage
		addr := vc.getZeroPageAddr()
		vc.storeOp(5, 2, addr, vc.Read(addr)-1, vc.setZeroNeg)
	case 0xc8: // INY
		vc.setRegOp(2, 1, &vc.Y, vc.Y+1, vc.setZeroNeg)
	case 0xc9: // CMP imm
		addr := vc.PC + 1
		vc.cmpOp(2, 2, vc.A, vc.Read(addr))
	case 0xca: // DEX
		vc.setRegOp(2, 1, &vc.X, vc.X-1, vc.setZeroNeg)
	case 0xcb: // AXS (UNDOCUMENTED)
		reg := vc.X & vc.A
		val := vc.Read(vc.PC + 1)
		vc.cmpOp(2, 2, reg, val)
		vc.X = reg-val
	case 0xcc: // CPY imm
		addr := vc.getAbsoluteAddr()
		vc.cmpOp(4, 3, vc.Y, vc.Read(addr))
	case 0xcd: // CMP abosolute
		addr := vc.getAbsoluteAddr()
		vc.cmpOp(4, 3, vc.A, vc.Read(addr))
	case 0xce: // DEC absolute
		addr := vc.getAbsoluteAddr()
		vc.storeOp(6, 3, addr, vc.Read(addr)-1, vc.setZeroNeg)

	case 0xd0: // BNE
		vc.branchOpRel(vc.P&FlagZero == 0)
	case 0xd1: // CMP (indirect),y
		addr, cycles := vc.getYPostIndexedAddr()
		vc.cmpOp(5+cycles, 2, vc.A, vc.Read(addr))
	case 0xd4: // 2-nop (UNDOCUMENTED)
		vc.opFn(4, 2, vc.undocumentedOpcode)
	case 0xd5: // CMP zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.cmpOp(4, 2, vc.A, vc.Read(addr))
	case 0xd6: // DEC zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.storeOp(6, 2, addr, vc.Read(addr)-1, vc.setZeroNeg)
	case 0xd8: // CLD
		vc.opFn(2, 1, func() { vc.P &^= FlagDecimal })
	case 0xd9: // CMP absolute,y
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.Y)
		vc.cmpOp(4+cycles, 3, vc.A, vc.Read(addr))
	case 0xda: // 1-nop (UNDOCUMENTED)
		vc.opFn(2, 1, vc.undocumentedOpcode)
	case 0xdc: // 3-nop (UNDOCUMENTED)
		_, cycles := vc.getIndexedAbsoluteAddr(vc.X)
		vc.opFn(4+cycles, 3, vc.undocumentedOpcode)
	case 0xdd: // CMP absolute,x
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.X)
		vc.cmpOp(4+cycles, 3, vc.A, vc.Read(addr))
	case 0xde: // DEC absolute,x
		addr, _ := vc.getIndexedAbsoluteAddr(vc.X)
		vc.storeOp(7, 3, addr, vc.Read(addr)-1, vc.setZeroNeg)

	case 0xe0: // CPX imm
		addr := vc.PC + 1
		vc.cmpOp(2, 2, vc.X, vc.Read(addr))
	case 0xe1: // SBC (indirect,x)
		addr := vc.getXPreIndexedAddr()
		vc.opFn(6, 2, func() { vc.A = vc.sbcAndSetFlags(vc.Read(addr)) })
	case 0xe2: // 2-nop (UNDOCUMENTED)
		vc.opFn(2, 2, vc.undocumentedOpcode)
	case 0xe3: // ISC (indirect,x) (UNDOCUMENTED)
		addr := vc.getXPreIndexedAddr()
		val := vc.Read(addr)+1
		vc.storeOp(8, 2, addr, val, vc.setZeroNeg)
		vc.opFn(0, 0, func() { vc.A = vc.sbcAndSetFlags(val) })
	case 0xe5: // SBC zeropage
		addr := vc.getZeroPageAddr()
		vc.opFn(3, 2, func() { vc.A = vc.sbcAndSetFlags(vc.Read(addr)) })
	case 0xe4: // CPX zeropage
		addr := vc.getZeroPageAddr()
		vc.cmpOp(3, 2, vc.X, vc.Read(addr))
	case 0xe6: // INC zeropage
		addr := vc.getZeroPageAddr()
		vc.storeOp(5, 2, addr, vc.Read(addr)+1, vc.setZeroNeg)
	case 0xe7: // ISC zeropage (UNDOCUMENTED)
		addr := vc.getZeroPageAddr()
		val := vc.Read(addr)+1
		vc.storeOp(5, 2, addr, val, vc.setZeroNeg)
		vc.opFn(0, 0, func() { vc.A = vc.sbcAndSetFlags(val) })
	case 0xe8: // INX
		vc.setRegOp(2, 1, &vc.X, vc.X+1, vc.setZeroNeg)
	case 0xe9: // SBC imm
		val := vc.Read(vc.PC + 1)
		vc.opFn(2, 2, func() { vc.A = vc.sbcAndSetFlags(val) })
	case 0xea: // NOP
		vc.opFn(2, 1, func() {})
	case 0xeb: // SBC imm (UNDOCUMENTED)
		val := vc.Read(vc.PC + 1)
		vc.opFn(2, 2, func() { vc.A = vc.sbcAndSetFlags(val) })
	case 0xec: // CPX absolute
		addr := vc.getAbsoluteAddr()
		vc.cmpOp(4, 3, vc.X, vc.Read(addr))
	case 0xed: // SBC absolute
		addr := vc.getAbsoluteAddr()
		vc.opFn(4, 3, func() { vc.A = vc.sbcAndSetFlags(vc.Read(addr)) })
	case 0xee: // INC absolute
		addr := vc.getAbsoluteAddr()
		vc.storeOp(6, 3, addr, vc.Read(addr)+1, vc.setZeroNeg)
	case 0xef: // ISC absolute (UNDOCUMENTED)
		addr := vc.getAbsoluteAddr()
		val := vc.Read(addr)+1
		vc.storeOp(6, 3, addr, val, vc.setZeroNeg)
		vc.opFn(0, 0, func() { vc.A = vc.sbcAndSetFlags(val) })

	case 0xf0: // BEQ
		vc.branchOpRel(vc.P&FlagZero == FlagZero)
	case 0xf1: // SBC (indirect),y
		addr, cycles := vc.getYPostIndexedAddr()
		vc.opFn(5+cycles, 2, func() { vc.A = vc.sbcAndSetFlags(vc.Read(addr)) })
	case 0xf3: // ISC (indirect),y (UNDOCUMENTED)
		addr, _ := vc.getYPostIndexedAddr()
		val := vc.Read(addr)+1
		vc.storeOp(8, 2, addr, val, vc.setZeroNeg)
		vc.opFn(0, 0, func() { vc.A = vc.sbcAndSetFlags(val) })
	case 0xf4: // 2-nop (UNDOCUMENTED)
		vc.opFn(4, 2, vc.undocumentedOpcode)
	case 0xf5: // SBC zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.opFn(4, 2, func() { vc.A = vc.sbcAndSetFlags(vc.Read(addr)) })
	case 0xf6: // INC zeropage,x
		addr := vc.getIndexedZeroPageAddr(vc.X)
		vc.storeOp(6, 2, addr, vc.Read(addr)+1, vc.setZeroNeg)
	case 0xf7: // ISC zeropage,x (UNDOCUMENTED)
		addr := vc.getIndexedZeroPageAddr(vc.X)
		val := vc.Read(addr)+1
		vc.storeOp(6, 2, addr, val, vc.setZeroNeg)
		vc.opFn(0, 0, func() { vc.A = vc.sbcAndSetFlags(val) })
	case 0xf8: // SED
		vc.opFn(2, 1, func() { vc.P |= FlagDecimal })
	case 0xf9: // SBC absolute,y
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.Y)
		vc.opFn(4+cycles, 3, func() { vc.A = vc.sbcAndSetFlags(vc.Read(addr)) })
	case 0xfa: // 1-nop (UNDOCUMENTED)
		vc.opFn(2, 1, vc.undocumentedOpcode)
	case 0xfb: // ISC absolute,x (UNDOCUMENTED)
		addr, _ := vc.getIndexedAbsoluteAddr(vc.Y)
		val := vc.Read(addr)+1
		vc.storeOp(7, 3, addr, val, vc.setZeroNeg)
		vc.opFn(0, 0, func() { vc.A = vc.sbcAndSetFlags(val) })
	case 0xfc: // 3-nop (UNDOCUMENTED)
		_, cycles := vc.getIndexedAbsoluteAddr(vc.X)
		vc.opFn(4+cycles, 3, vc.undocumentedOpcode)
	case 0xfd: // SBC absolute,x
		addr, cycles := vc.getIndexedAbsoluteAddr(vc.X)
		vc.opFn(4+cycles, 3, func() { vc.A = vc.sbcAndSetFlags(vc.Read(addr)) })
	case 0xfe: // INC absolute,x
		addr, _ := vc.getIndexedAbsoluteAddr(vc.X)
		vc.storeOp(7, 3, addr, vc.Read(addr)+1, vc.setZeroNeg)
	case 0xff: // ISC absolute,x (UNDOCUMENTED)
		addr, _ := vc.getIndexedAbsoluteAddr(vc.X)
		val := vc.Read(addr)+1
		vc.storeOp(7, 3, addr, val, vc.setZeroNeg)
		vc.opFn(0, 0, func() { vc.A = vc.sbcAndSetFlags(val) })

	default:
		vc.Err(fmt.Errorf("unimplemented opcode 0x%02x", opcode))
	}
}

func boolByte(b bool) byte {
	if b {
		return 1
	}
	return 0
}

func (vc *Virt6502) inDecimalMode() bool {
	return !vc.IgnoreDecimalMode && vc.P&FlagDecimal == FlagDecimal
}

func (vc *Virt6502) adcAndSetFlags(val byte) byte {

	carry := boolByte(vc.P&FlagCarry == FlagCarry)
	n1A, n1Val := vc.A&0x0f, val&0x0f
	n1Result := (n1A + n1Val + carry) & 0x0f

	var halfCarry byte
	if vc.inDecimalMode() {
		halfCarry = boolByte(n1A+n1Val+carry > 0x09)
	} else {
		halfCarry = (n1A + n1Val + carry) >> 4
	}

	n2A, n2Val := vc.A>>4, val>>4
	n2Result := (n2A + n2Val + halfCarry) & 0x0f

	var carryOut byte
	if vc.inDecimalMode() {
		carryOut = boolByte(n2A+n2Val+halfCarry > 0x09)
	} else {
		carryOut = (n2A + n2Val + halfCarry) >> 4
	}

	vc.setZeroFlag(n1Result == 0 && n2Result == 0)
	vc.setNegFlag(n2Result&0x08 == 0x08)
	vc.setCarryFlag(carryOut == 1)
	n2ResultForV := (int8(n2A<<4) >> 4) + (int8(n2Val<<4) >> 4) + int8(halfCarry)
	vc.setOverflowFlag(n2ResultForV < -8 || n2ResultForV > 7)

	if vc.inDecimalMode() {
		if halfCarry == 1 {
			n1Result = (n1Result + 0x06) & 0x0f
		}
		if carryOut == 1 {
			n2Result = (n2Result + 0x06) & 0x0f
		}
	}

	result := n1Result | (n2Result << 4)

	return result
}

func (vc *Virt6502) sbcAndSetFlags(val byte) byte {

	// NOTE: remember, carry is inverted for SBC
	carry := boolByte(vc.P&FlagCarry == 0)
	n1A, n1Val := vc.A&0x0f, val&0x0f
	n1Result := (n1A - n1Val - carry) & 0x0f

	// NOTE: no half-carry dec mode adjust needed for sbc, only adc
	halfCarry := boolByte(n1A-n1Val-carry > 0x0f)

	n2A, n2Val := vc.A>>4, val>>4
	n2Result := (n2A - n2Val - halfCarry) & 0x0f

	var carryOut byte
	if vc.inDecimalMode() {
		carryOut = boolByte(n2A-n2Val-halfCarry > 0x09)
	} else {
		carryOut = (n2A - n2Val - halfCarry) >> 4
	}

	vc.setZeroFlag(n1Result == 0 && n2Result == 0)
	vc.setNegFlag(n2Result&0x08 == 0x08)
	vc.setCarryFlag(carryOut == 0)
	n2ResultForV := (int8(n2A<<4) >> 4) - (int8(n2Val<<4) >> 4) - int8(halfCarry)
	vc.setOverflowFlag(n2ResultForV < -8 || n2ResultForV > 7)

	if vc.inDecimalMode() {
		if halfCarry == 1 {
			n1Result = (n1Result - 0x06) & 0x0f
		}
		if carryOut == 1 {
			n2Result = (n2Result - 0x06) & 0x0f
		}
	}

	result := n1Result | (n2Result << 4)

	return result
}

func (vc *Virt6502) aslAndSetFlags(val byte) byte {
	result := val << 1
	vc.setCarryFlag(val&0x80 == 0x80)
	vc.setZeroNeg(result)
	return result
}

func (vc *Virt6502) lsrAndSetFlags(val byte) byte {
	result := val >> 1
	vc.setCarryFlag(val&0x01 == 0x01)
	vc.setZeroNeg(result)
	return result
}

func (vc *Virt6502) rorAndSetFlags(val byte) byte {
	result := val >> 1
	if vc.P&FlagCarry == FlagCarry {
		result |= 0x80
	}
	vc.setCarryFlag(val&0x01 == 0x01)
	vc.setZeroNeg(result)
	return result
}

func (vc *Virt6502) rolAndSetFlags(val byte) byte {
	result := val << 1
	if vc.P&FlagCarry == FlagCarry {
		result |= 0x01
	}
	vc.setCarryFlag(val&0x80 == 0x80)
	vc.setZeroNeg(result)
	return result
}

func (vc *Virt6502) bitAndSetFlags(val byte) {
	vc.P &^= 0xC0
	vc.P |= val & 0xC0
	vc.setZeroFlag(vc.A&val == 0)
}
