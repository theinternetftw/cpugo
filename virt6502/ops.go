package virt6502

import "fmt"

// the "do" prefix means this includes runCycle()s

func (vc *Virt6502) doReadCycle(addr uint16) byte {
	b := vc.Read(addr)
	vc.RunCycles(1)
	return b
}

func (vc *Virt6502) doWriteCycle(addr uint16, val byte) {
	vc.Write(addr, val)
	vc.RunCycles(1)
}

func (vc *Virt6502) doPCFetchCycle() byte {
	b := vc.doReadCycle(vc.PC)
	vc.fetchBuf.push(b)
	vc.PC++
	return b
}

func (vc *Virt6502) doPushCycle(val byte) {
	vc.doWriteCycle(0x100+uint16(vc.S), val)
	vc.S--
}
func (vc *Virt6502) doPullCycle() byte {
	vc.S++
	val := vc.doReadCycle(0x100+uint16(vc.S))
	return val
}
func (vc *Virt6502) doStackReadCycle() {
	_ = vc.doReadCycle(0x100+uint16(vc.S))
}

func to16(hi, lo byte) uint16 {
	return (uint16(hi) << 8) | uint16(lo)
}
func (vc *Virt6502) doPCFetch16() uint16 {
	lo := vc.doPCFetchCycle()
	hi := vc.doPCFetchCycle()
	return to16(hi, lo)
}
func (vc *Virt6502) doPCFetchLoHi() (byte, byte) {
	lo := vc.doPCFetchCycle()
	hi := vc.doPCFetchCycle()
	return lo, hi
}

func (vc *Virt6502) doPCReadCycle() {
	_ = vc.doReadCycle(vc.PC)
}

func (vc *Virt6502) doNoMemOp(fn func()) {
	_ = vc.doReadCycle(vc.PC)
	fn()
}

func (vc *Virt6502) doReadModWrite(addr uint16, fn func (byte)byte) {
	val := vc.doReadCycle(addr)
	vc.doWriteCycle(addr, val)
	vc.doWriteCycle(addr, fn(val))
}

func (vc *Virt6502) doZeroPageRead() byte {
	addr := uint16(vc.doPCFetchCycle())
	return vc.doReadCycle(addr)
}
func (vc *Virt6502) doZeroPageReadModWrite(fn func (byte)byte) {
	addr := uint16(vc.doPCFetchCycle())
	vc.doReadModWrite(addr, fn)
}
func (vc *Virt6502) doZeroPageWrite(val byte) {
	addr := uint16(vc.doPCFetchCycle())
	vc.doWriteCycle(addr, val)
}

func (vc *Virt6502) doFirstFixupRead(lo, hi, idx byte) (byte, uint16, bool) {
	addr := to16(hi, lo+idx)
	val := vc.doReadCycle(addr)
	wrapped := int(lo)+int(idx) > 0xff
	if wrapped {
		addr += 0x100
	}
	return val, addr, wrapped
}

func (vc *Virt6502) doAbsRead() byte {
	addr := vc.doPCFetch16()
	return vc.doReadCycle(addr)
}
func (vc *Virt6502) doAbsReadModWrite(fn func(byte)byte) {
	addr := vc.doPCFetch16()
	vc.doReadModWrite(addr, fn)
}
func (vc *Virt6502) doAbsWrite(val byte) {
	addr := vc.doPCFetch16()
	vc.doWriteCycle(addr, val)
}

func (vc *Virt6502) doIndexedAbsRead(idx byte) byte {
	lo, hi := vc.doPCFetchLoHi()
	val, addr, wrapped := vc.doFirstFixupRead(lo, hi, idx)
	if wrapped {
		val = vc.doReadCycle(addr)
	}
	return val
}
func (vc *Virt6502) doIndexedAbsReadModWrite(idx byte, fn func(byte) byte) {
	lo, hi := vc.doPCFetchLoHi()
	_, addr, _ := vc.doFirstFixupRead(lo, hi, idx)
	vc.doReadModWrite(addr, fn)
}
func (vc *Virt6502) doIndexedAbsWrite(idx byte, val byte) {
	lo, hi := vc.doPCFetchLoHi()
	_, addr, _ := vc.doFirstFixupRead(lo, hi, idx)
	vc.doWriteCycle(addr, val)
}

func (vc *Virt6502) doIndexedZeroPageAddrFetch(idx byte) uint16 {
	base := vc.doPCFetchCycle()
	_ = vc.doReadCycle(uint16(base))
	return uint16(base + idx) // wraps at 0xff
}
func (vc *Virt6502) doIndexedZeroPageRead(idx byte) byte {
	addr := vc.doIndexedZeroPageAddrFetch(idx)
	return vc.doReadCycle(addr)
}
func (vc *Virt6502) doIndexedZeroPageReadModWrite(idx byte, fn func(byte) byte) {
	addr := vc.doIndexedZeroPageAddrFetch(idx)
	vc.doReadModWrite(addr, fn)
}
func (vc *Virt6502) doIndexedZeroPageWrite(idx byte, val byte) {
	addr := vc.doIndexedZeroPageAddrFetch(idx)
	vc.doWriteCycle(addr, val)
}

func (vc *Virt6502) doXPreIndexedAddrFetch() uint16 {
	ptr := vc.doPCFetchCycle()
	_ = vc.doReadCycle(uint16(ptr))
	zPageLowAddr := uint16(ptr + vc.X)      // wraps at 0xff
	zPageHighAddr := uint16(ptr + vc.X + 1) // wraps at 0xff
	lo := vc.doReadCycle(zPageLowAddr)
	hi := vc.doReadCycle(zPageHighAddr)
	return to16(hi, lo)
}
func (vc *Virt6502) doXPreIndexedRead() byte {
	addr := vc.doXPreIndexedAddrFetch()
	val := vc.doReadCycle(addr)
	return val
}
func (vc *Virt6502) doXPreIndexedReadModWrite(fn func(byte) byte) {
	addr := vc.doXPreIndexedAddrFetch()
	vc.doReadModWrite(addr, fn)
}
func (vc *Virt6502) doXPreIndexedWrite(val byte) {
	addr := vc.doXPreIndexedAddrFetch()
	vc.doWriteCycle(addr, val)
}

func (vc *Virt6502) doYPostIndexedFirstFixupRead() (byte, uint16, bool) {
	ptr := vc.doPCFetchCycle()
	zPageLowAddr := uint16(ptr)
	zPageHighAddr := uint16(ptr + 1) // wraps at 0xff
	lo := vc.doReadCycle(zPageLowAddr)
	hi := vc.doReadCycle(zPageHighAddr)
	val, addr, wrapped := vc.doFirstFixupRead(lo, hi, vc.Y)
	return val, addr, wrapped
}
func (vc *Virt6502) doYPostIndexedRead() byte {
	val, addr, wrapped := vc.doYPostIndexedFirstFixupRead()
	if wrapped {
		val = vc.doReadCycle(addr)
	}
	return val
}
func (vc *Virt6502) doYPostIndexedReadModWrite(fn func(byte)byte) {
	_, addr, _ := vc.doYPostIndexedFirstFixupRead()
	vc.doReadModWrite(addr, fn)
}
func (vc *Virt6502) doYPostIndexedWrite(val byte) {
	_, addr, _ := vc.doYPostIndexedFirstFixupRead()
	vc.doWriteCycle(addr, val)
}

func (vc *Virt6502) doIndirectJmpAddrRead() uint16 {
	loAddr := vc.doPCFetch16()
	// hw bug! similar to other indexing wrapping issues...
	hiAddr := (loAddr & 0xff00) | ((loAddr + 1) & 0xff) // lo-byte wraps at 0xff
	lo := vc.doReadCycle(loAddr)
	hi := vc.doReadCycle(hiAddr)
	return to16(hi, lo)
}

func (vc *Virt6502) doInterruptPushJmp(addr uint16, flags byte) {
	vc.doPushCycle(byte(vc.PC >> 8))
	vc.doPushCycle(byte(vc.PC))
	vc.doPushCycle(flags)
	vc.P |= FlagIrqDisabled
	lo := vc.doReadCycle(addr)
	hi := vc.doReadCycle(addr+1)
	vc.PC = to16(hi, lo)
}
// TODO: skipped BRKs
func (vc *Virt6502) doBRK() {
	_ = vc.doPCFetchCycle()
	vc.doInterruptPushJmp(0xfffe, vc.P | FlagBrk | FlagAlwaysSet)
}
func (vc *Virt6502) doIRQ() {
	vc.doPCReadCycle()
	vc.doPCReadCycle()
	vc.doInterruptPushJmp(0xfffe, vc.P | FlagAlwaysSet)
}
func (vc *Virt6502) doNMI() {
	vc.doPCReadCycle()
	vc.doPCReadCycle()
	vc.doInterruptPushJmp(0xfffa, vc.P | FlagAlwaysSet)
}
func (vc *Virt6502) doRESET() {
	for i := 0; i < 3; i++ {
		vc.doStackReadCycle()
		vc.S--
	}
	vc.P |= FlagIrqDisabled
	lo := vc.doReadCycle(0xfffc)
	hi := vc.doReadCycle(0xfffc+1)
	vc.PC = to16(hi, lo)
}

func (vc *Virt6502) doRTI() {
	vc.P = vc.doPullOp() &^ FlagBrk
	lo := vc.doPullCycle()
	hi := vc.doPullCycle()
	vc.PC = to16(hi, lo)
	vc.LastStepsP = vc.P // NOTE: this updates the interruptsEnabled logic
}

func (vc *Virt6502) doRTS() {
	lo := vc.doPullOp()
	hi := vc.doPullCycle()
	vc.PC = to16(hi, lo)
	_ = vc.doPCFetchCycle()
}

func (vc *Virt6502) doPushOp(val byte) {
	vc.doPCReadCycle()
	vc.doPushCycle(val)
}

func (vc *Virt6502) doPullOp() byte {
	vc.doPCReadCycle()
	vc.doStackReadCycle()
	return vc.doPullCycle()
}

func (vc *Virt6502) doJSR() {
	lo := vc.doPCFetchCycle()
	vc.doStackReadCycle()
	vc.doPushCycle(byte(vc.PC >> 8))
	vc.doPushCycle(byte(vc.PC))
	hi := vc.doPCFetchCycle()
	vc.PC = to16(hi, lo)
}

func (vc *Virt6502) doBranchRel(test bool) {
	offset := int8(vc.doPCFetchCycle())
	if test {
		vc.doPCReadCycle() // "failed pipelined" read
		lo := int(vc.PC&0xff) + int(offset)
		newPC := (vc.PC&0xff00) | uint16(lo&0xff)
		if lo < 0 || lo > 0xff {
			// NOTE: Not quite the way it happens in silicon (read cycle below
			// happens regardless), but this is the way it needs to be done to
			// avoid having to think about pipelining (when the read is correct,
			// we just merge it into the opcode read that happens next Step()
			// NOTE: A bit inaccurate. By just using the std opcode read for the
			// pipelined fetch step, there's a PC read cycle that is missing
			// anytime we IRQ or NMI after any branch (taken or no).
			_ = vc.doReadCycle(newPC)
			newPC = uint16(int(vc.PC) + int(offset))
		}
		vc.PC = newPC
	} else {
		// The pipelined fetch cycle is sim'd by just reading
		// the next op normally
	}
}

func (vc *Virt6502) setRegOp(dst *byte, src byte, flagFn func(byte)) {
	*dst = src
	flagFn(*dst)
}
func (vc *Virt6502) cmpOp(reg byte, val byte) {
	vc.setZeroNeg(reg - val)
	vc.setCarryFlag(reg >= val)
}

var opcodeNames = [256]string{

	// LOWERCASE == undocumented

	// 0      1      2      3      4      5      6      7      8      9      A      B      C      D      E      F
	"BRK", "ORA", "kil", "slo", "skb", "ORA", "ASL", "slo", "PHP", "ORA", "ASL", "aac", "skw", "ORA", "ASL", "slo",
	"BPL", "ORA", "kil", "slo", "skb", "ORA", "ASL", "slo", "CLC", "ORA", "nop", "slo", "skw", "ORA", "ASL", "slo",
	"JSR", "AND", "kil", "rla", "BIT", "AND", "ROL", "rla", "PLP", "AND", "ROL", "aac", "BIT", "AND", "ROL", "rla",
	"BMI", "AND", "kil", "rla", "skb", "AND", "ROL", "rla", "SEC", "AND", "nop", "rla", "skw", "AND", "ROL", "rla",
	"RTI", "EOR", "kil", "sre", "skb", "EOR", "LSR", "sre", "PHA", "EOR", "LSR", "asr", "JMP", "EOR", "LSR", "sre",
	"BVC", "EOR", "kil", "sre", "skb", "EOR", "LSR", "sre", "CLI", "EOR", "nop", "sre", "skw", "EOR", "LSR", "sre",
	"RTS", "ADC", "kil", "rra", "skb", "ADC", "ROR", "rra", "PLA", "ADC", "ROR", "arr", "JMP", "ADC", "ROR", "rra",
	"BVS", "ADC", "kil", "rra", "skb", "ADC", "ROR", "rra", "SEI", "ADC", "nop", "rra", "skw", "ADC", "ROR", "rra",
	"skb", "STA", "skb", "axs", "STY", "STA", "STX", "axs", "DEY", "skb", "TXA", "xxx", "STY", "STA", "STX", "axs",
	"BCC", "STA", "kil", "xxx", "STY", "STA", "STX", "axs", "TYA", "STA", "TXS", "xxx", "xxx", "STA", "xxx", "xxx",
	"LDY", "LDA", "LDX", "lax", "LDY", "LDA", "LDX", "lax", "TAY", "LDA", "TAX", "lax", "LDY", "LDA", "LDX", "lax",
	"BCS", "LDA", "kil", "lax", "LDY", "LDA", "LDX", "lax", "CLV", "LDA", "TSX", "las", "LDY", "LDA", "LDX", "lax",
	"CPY", "CMP", "skb", "dcm", "CPY", "CMP", "DEC", "dcm", "INY", "CMP", "DEX", "sax", "CPY", "CMP", "DEC", "dcm",
	"BNE", "CMP", "kil", "dcm", "skb", "CMP", "DEC", "dcm", "CLD", "CMP", "nop", "dcm", "skw", "CMP", "DEC", "dcm",
	"CPX", "SBC", "skb", "isc", "CPX", "SBC", "INC", "isc", "INX", "SBC", "NOP", "sbc", "CPX", "SBC", "INC", "isc",
	"BEQ", "SBC", "kil", "isc", "skb", "SBC", "INC", "isc", "SED", "SBC", "nop", "isc", "skw", "SBC", "INC", "isc",
}

func IsUndocumentedOpcode(opcode byte) bool {
	// TODO: more systematic storage of undoc info
	return opcodeNames[opcode][0] >= 'a'
}

func (vc *Virt6502) stepOpcode() {

	opcode := vc.doPCFetchCycle()

	switch opcode {
	case 0x00: // BRK
		vc.doBRK()
	case 0x01: // ORA (indirect,x)
		val := vc.doXPreIndexedRead()
		vc.setRegOp(&vc.A, vc.A|val, vc.setZeroNeg)
	case 0x03: // slo (indirect,x) (UNDOCUMENTED)
		vc.doXPreIndexedReadModWrite(vc.sloAndSetFlags)
	case 0x04: // skb zeropage (UNDOCUMENTED)
		_ = vc.doZeroPageRead()
	case 0x05: // ORA zeropage
		val := vc.doZeroPageRead()
		vc.setRegOp(&vc.A, vc.A|val, vc.setZeroNeg)
	case 0x06: // ASL zeropage
		vc.doZeroPageReadModWrite(vc.aslAndSetFlags)
	case 0x07: // slo zeropage (UNDOCUMENTED)
		vc.doZeroPageReadModWrite(vc.sloAndSetFlags)
	case 0x08: // PHP
		vc.doPushOp(vc.P | FlagAlwaysSet | FlagBrk)
	case 0x09: // ORA imm
		val := vc.doPCFetchCycle()
		vc.setRegOp(&vc.A, vc.A|val, vc.setZeroNeg)
	case 0x0a: // ASL A
		vc.doNoMemOp(func() { vc.A = vc.aslAndSetFlags(vc.A) })
	case 0x0b: // aac imm (UNDOCUMENTED)
		val := vc.doPCFetchCycle()
		vc.setRegOp(&vc.A, vc.A&val, vc.setZeroNeg)
		vc.setCarryFlag(vc.A&0x80 != 0)
	case 0x0c: // skw absolute (UNDOCUMENTED)
		_ = vc.doAbsRead()
	case 0x0d: // ORA absolute
		val := vc.doAbsRead()
		vc.setRegOp(&vc.A, vc.A|val, vc.setZeroNeg)
	case 0x0e: // ASL absolute
		vc.doAbsReadModWrite(vc.aslAndSetFlags)
	case 0x0f: // slo absolute (UNDOCUMENTED)
		vc.doAbsReadModWrite(vc.sloAndSetFlags)

	case 0x10: // BPL
		vc.doBranchRel(vc.P&FlagNeg == 0)
	case 0x11: // ORA (indirect),y
		val := vc.doYPostIndexedRead()
		vc.setRegOp(&vc.A, vc.A|val, vc.setZeroNeg)
	case 0x13: // slo (indirect),y (UNDOCUMENTED)
		vc.doYPostIndexedReadModWrite(vc.sloAndSetFlags)
	case 0x14: // skb zeropage,x (UNDOCUMENTED)
		_ = vc.doIndexedZeroPageRead(vc.X)
	case 0x15: // ORA zeropage,x
		val := vc.doIndexedZeroPageRead(vc.X)
		vc.setRegOp(&vc.A, vc.A|val, vc.setZeroNeg)
	case 0x16: // ASL zeropage,x
		vc.doIndexedZeroPageReadModWrite(vc.X, vc.aslAndSetFlags)
	case 0x17: // slo zeropage,x (UNDOCUMENTED)
		vc.doIndexedZeroPageReadModWrite(vc.X, vc.sloAndSetFlags)
	case 0x18: // CLC
		vc.doNoMemOp(func() { vc.P &^= FlagCarry })
	case 0x19: // ORA absolute,y
		val := vc.doIndexedAbsRead(vc.Y)
		vc.setRegOp(&vc.A, vc.A|val, vc.setZeroNeg)
	case 0x1a: // nop (UNDOCUMENTED)
		vc.doNoMemOp(func() {})
	case 0x1b: // slo absolute,y (UNDOCUMENTED)
		vc.doIndexedAbsReadModWrite(vc.Y, vc.sloAndSetFlags)
	case 0x1c: // skw absolute,x (UNDOCUMENTED)
		_ = vc.doIndexedAbsRead(vc.X)
	case 0x1d: // ORA absolute,x
		val := vc.doIndexedAbsRead(vc.X)
		vc.setRegOp(&vc.A, vc.A|val, vc.setZeroNeg)
	case 0x1e: // ASL absolute,x
		vc.doIndexedAbsReadModWrite(vc.X, vc.aslAndSetFlags)
	case 0x1f: // slo absolute,x (UNDOCUMENTED)
		vc.doIndexedAbsReadModWrite(vc.X, vc.sloAndSetFlags)

	case 0x20: // JSR (jump and store return addr)
		vc.doJSR()
	case 0x21: // AND (indirect,x)
		val := vc.doXPreIndexedRead()
		vc.setRegOp(&vc.A, vc.A&val, vc.setZeroNeg)
	case 0x23: // rla (indirect,x) (UNDOCUMENTED)
		vc.doXPreIndexedReadModWrite(vc.rlaAndSetFlags)
	case 0x24: // BIT zeropage
		val := vc.doZeroPageRead()
		vc.bitAndSetFlags(val)
	case 0x25: // AND zeropage
		val := vc.doZeroPageRead()
		vc.setRegOp(&vc.A, vc.A&val, vc.setZeroNeg)
	case 0x26: // ROL zeropage
		vc.doZeroPageReadModWrite(vc.rolAndSetFlags)
	case 0x27: // rla zeropage (UNDOCUMENTED)
		vc.doZeroPageReadModWrite(vc.rlaAndSetFlags)
	case 0x28: // PLP
		flags := vc.doPullOp()
		vc.P = flags &^ FlagBrk
	case 0x29: // AND imm
		val := vc.doPCFetchCycle()
		vc.setRegOp(&vc.A, vc.A&val, vc.setZeroNeg)
	case 0x2a: // ROL A
		vc.doNoMemOp(func() { vc.A = vc.rolAndSetFlags(vc.A) })
	case 0x2b: // aac imm (UNDOCUMENTED)
		val := vc.doPCFetchCycle()
		vc.setRegOp(&vc.A, vc.A&val, vc.setZeroNeg)
		vc.setCarryFlag(vc.A&0x80 != 0)
	case 0x2c: // BIT absolute
		val := vc.doAbsRead()
		vc.bitAndSetFlags(val)
	case 0x2d: // AND absolute
		val := vc.doAbsRead()
		vc.setRegOp(&vc.A, vc.A&val, vc.setZeroNeg)
	case 0x2e: // ROL absolute
		vc.doAbsReadModWrite(vc.rolAndSetFlags)
	case 0x2f: // rla absolute (UNDOCUMENTED)
		vc.doAbsReadModWrite(vc.rlaAndSetFlags)

	case 0x30: // BMI
		vc.doBranchRel(vc.P&FlagNeg == FlagNeg)
	case 0x31: // AND (indirect),y
		val := vc.doYPostIndexedRead()
		vc.setRegOp(&vc.A, vc.A&val, vc.setZeroNeg)
	case 0x33: // rla (indirect),y (UNDOCUMENTED)
		vc.doYPostIndexedReadModWrite(vc.rlaAndSetFlags)
	case 0x34: // skb zeropage,x (UNDOCUMENTED)
		_ = vc.doIndexedZeroPageRead(vc.X)
	case 0x35: // AND zeropage,x
		val := vc.doIndexedZeroPageRead(vc.X)
		vc.setRegOp(&vc.A, vc.A&val, vc.setZeroNeg)
	case 0x36: // ROL zeropage,x
		vc.doIndexedZeroPageReadModWrite(vc.X, vc.rolAndSetFlags)
	case 0x37: // rla zeropage,x (UNDOCUMENTED)
		vc.doIndexedZeroPageReadModWrite(vc.X, vc.rlaAndSetFlags)
	case 0x38: // SEC
		vc.doNoMemOp(func() { vc.P |= FlagCarry })
	case 0x39: // AND absolute,y
		val := vc.doIndexedAbsRead(vc.Y)
		vc.setRegOp(&vc.A, vc.A&val, vc.setZeroNeg)
	case 0x3a: // nop (UNDOCUMENTED)
		vc.doNoMemOp(func() {})
	case 0x3b: // rla absolute,y (UNDOCUMENTED)
		vc.doIndexedAbsReadModWrite(vc.Y, vc.rlaAndSetFlags)
	case 0x3c: // skw absolute,x (UNDOCUMENTED)
		_ = vc.doIndexedAbsRead(vc.X)
	case 0x3d: // AND absolute,x
		val := vc.doIndexedAbsRead(vc.X)
		vc.setRegOp(&vc.A, vc.A&val, vc.setZeroNeg)
	case 0x3e: // ROL absolute,x
		vc.doIndexedAbsReadModWrite(vc.X, vc.rolAndSetFlags)
	case 0x3f: // rla absolute,x (UNDOCUMENTED)
		vc.doIndexedAbsReadModWrite(vc.X, vc.rlaAndSetFlags)

	case 0x40: // RTI (return from interrupt)
		vc.doRTI()
	case 0x41: // EOR (indirect,x)
		val := vc.doXPreIndexedRead()
		vc.setRegOp(&vc.A, vc.A^val, vc.setZeroNeg)
	case 0x43: // sre (indirect,x) (UNDOCUMENTED)
		vc.doXPreIndexedReadModWrite(vc.sreAndSetFlags)
	case 0x44: // skb zeropage (UNDOCUMENTED)
		_ = vc.doZeroPageRead()
	case 0x45: // EOR zeropage
		val := vc.doZeroPageRead()
		vc.setRegOp(&vc.A, vc.A^val, vc.setZeroNeg)
	case 0x46: // LSR zeropage
		vc.doZeroPageReadModWrite(vc.lsrAndSetFlags)
	case 0x47: // sre zeropage (UNDOCUMENTED)
		vc.doZeroPageReadModWrite(vc.sreAndSetFlags)
	case 0x48: // PHA
		vc.doPushOp(vc.A)
	case 0x49: // EOR imm
		val := vc.doPCFetchCycle()
		vc.setRegOp(&vc.A, vc.A^val, vc.setZeroNeg)
	case 0x4a: // LSR A
		vc.doNoMemOp(func() { vc.A = vc.lsrAndSetFlags(vc.A) })
	case 0x4b: // asr imm (UNDOCUMENTED)
		val := vc.doPCFetchCycle()
		vc.setRegOp(&vc.A, vc.A&val, vc.setZeroNeg)
		vc.A = vc.lsrAndSetFlags(vc.A)
	case 0x4c: // JMP absolute
		vc.PC = vc.doPCFetch16()
	case 0x4d: // EOR absolute
		val := vc.doAbsRead()
		vc.setRegOp(&vc.A, vc.A^val, vc.setZeroNeg)
	case 0x4e: // LSR absolute
		vc.doAbsReadModWrite(vc.lsrAndSetFlags)
	case 0x4f: // sre absolute (UNDOCUMENTED)
		vc.doAbsReadModWrite(vc.sreAndSetFlags)

	case 0x50: // BVC
		vc.doBranchRel(vc.P&FlagOverflow == 0)
	case 0x51: // EOR (indirect),y
		val := vc.doYPostIndexedRead()
		vc.setRegOp(&vc.A, vc.A^val, vc.setZeroNeg)
	case 0x53: // sre (indirect),y (UNDOCUMENTED)
		vc.doYPostIndexedReadModWrite(vc.sreAndSetFlags)
	case 0x54: // skb zeropage,x (UNDOCUMENTED)
		_ = vc.doIndexedZeroPageRead(vc.X)
	case 0x55: // EOR zeropage,x
		val := vc.doIndexedZeroPageRead(vc.X)
		vc.setRegOp(&vc.A, vc.A^val, vc.setZeroNeg)
	case 0x56: // LSR zeropage,x
		vc.doIndexedZeroPageReadModWrite(vc.X, vc.lsrAndSetFlags)
	case 0x57: // sre zeropage,x (UNDOCUMENTED)
		vc.doIndexedZeroPageReadModWrite(vc.X, vc.sreAndSetFlags)
	case 0x58: // CLI
		vc.doNoMemOp(func() { vc.P &^= FlagIrqDisabled })
	case 0x59: // EOR absolute,y
		val := vc.doIndexedAbsRead(vc.Y)
		vc.setRegOp(&vc.A, vc.A^val, vc.setZeroNeg)
	case 0x5a: // nop (UNDOCUMENTED)
		vc.doNoMemOp(func() {})
	case 0x5b: // sre absolute,y (UNDOCUMENTED)
		vc.doIndexedAbsReadModWrite(vc.Y, vc.sreAndSetFlags)
	case 0x5c: // skw absolute,x (UNDOCUMENTED)
		_ = vc.doIndexedAbsRead(vc.X)
	case 0x5d: // EOR absolute,x
		val := vc.doIndexedAbsRead(vc.X)
		vc.setRegOp(&vc.A, vc.A^val, vc.setZeroNeg)
	case 0x5e: // LSR absolute,x
		vc.doIndexedAbsReadModWrite(vc.X, vc.lsrAndSetFlags)
	case 0x5f: // sre absolute,x (UNDOCUMENTED)
		vc.doIndexedAbsReadModWrite(vc.X, vc.sreAndSetFlags)

	case 0x60: // RTS (return from subroutine)
		vc.doRTS()
	case 0x61: // ADC (indirect,x)
		val := vc.doXPreIndexedRead()
		vc.A = vc.adcAndSetFlags(val)
	case 0x63: // rra (indirect,x) (UNDOCUMENTED)
		vc.doXPreIndexedReadModWrite(vc.rraAndSetFlags)
	case 0x64: // skb zeropage (UNDOCUMENTED)
		_ = vc.doZeroPageRead()
	case 0x65: // ADC zeropage
		val := vc.doZeroPageRead()
		vc.A = vc.adcAndSetFlags(val)
	case 0x66: // ROR zeropage
		vc.doZeroPageReadModWrite(vc.rorAndSetFlags)
	case 0x67: // rra zeropage (UNDOCUMENTED)
		vc.doZeroPageReadModWrite(vc.rraAndSetFlags)
	case 0x68: // PLA
		result := vc.doPullOp()
		vc.setRegOp(&vc.A, result, vc.setZeroNeg)
	case 0x69: // ADC imm
		val := vc.doPCFetchCycle()
		vc.A = vc.adcAndSetFlags(val)
	case 0x6a: // ROR A
		vc.doNoMemOp(func() { vc.A = vc.rorAndSetFlags(vc.A) })
	case 0x6b: // arr imm (UNDOCUMENTED)
		val := vc.doPCFetchCycle()
		vc.setRegOp(&vc.A, vc.A&val, vc.setZeroNeg)
		vc.A = vc.rorAndSetFlags(vc.A)
		vc.setCarryFlag(vc.A&0x40 != 0)
		vc.setOverflowFlag(((vc.A<<1)^vc.A)&0x40 != 0)
	case 0x6c: // JMP (indirect)
		vc.PC = vc.doIndirectJmpAddrRead()
	case 0x6d: // ADC absolute
		val := vc.doAbsRead()
		vc.A = vc.adcAndSetFlags(val)
	case 0x6e: // ROR absolute
		vc.doAbsReadModWrite(vc.rorAndSetFlags)
	case 0x6f: // rra absolute (UNDOCUMENTED)
		vc.doAbsReadModWrite(vc.rraAndSetFlags)

	case 0x70: // BVS
		vc.doBranchRel(vc.P&FlagOverflow == FlagOverflow)
	case 0x71: // ADC (indirect),y
		val := vc.doYPostIndexedRead()
		vc.A = vc.adcAndSetFlags(val)
	case 0x73: // rra (indirect),y (UNDOCUMENTED)
		vc.doYPostIndexedReadModWrite(vc.rraAndSetFlags)
	case 0x74: // skb zeropage,x (UNDOCUMENTED)
		_ = vc.doIndexedZeroPageRead(vc.X)
	case 0x75: // ADC zeropage,x
		val := vc.doIndexedZeroPageRead(vc.X)
		vc.A = vc.adcAndSetFlags(val)
	case 0x76: // ROR zeropage,x
		vc.doIndexedZeroPageReadModWrite(vc.X, vc.rorAndSetFlags)
	case 0x77: // rra zeropage,x (UNDOCUMENTED)
		vc.doIndexedZeroPageReadModWrite(vc.X, vc.rraAndSetFlags)
	case 0x78: // SEI
		vc.doNoMemOp(func() { vc.P |= FlagIrqDisabled })
	case 0x79: // ADC absolute,y
		val := vc.doIndexedAbsRead(vc.Y)
		vc.A = vc.adcAndSetFlags(val)
	case 0x7a: // nop (UNDOCUMENTED)
		vc.doNoMemOp(func() {})
	case 0x7b: // rra absolute,y (UNDOCUMENTED)
		vc.doIndexedAbsReadModWrite(vc.Y, vc.rraAndSetFlags)
	case 0x7c: // skw absolute,x (UNDOCUMENTED)
		_ = vc.doIndexedAbsRead(vc.X)
	case 0x7d: // ADC absolute,x
		val := vc.doIndexedAbsRead(vc.X)
		vc.A = vc.adcAndSetFlags(val)
	case 0x7e: // ROR absolute,x
		vc.doIndexedAbsReadModWrite(vc.X, vc.rorAndSetFlags)
	case 0x7f: // rra absolute,x (UNDOCUMENTED)
		vc.doIndexedAbsReadModWrite(vc.X, vc.rraAndSetFlags)

	case 0x80: // skb imm (UNDOCUMENTED)
		_ = vc.doPCFetchCycle()
	case 0x81: // STA (indirect,x)
		vc.doXPreIndexedWrite(vc.A)
	case 0x82: // skb imm (UNDOCUMENTED)
		_ = vc.doPCFetchCycle()
	case 0x83: // axs (indirect,x) (UNDOCUMENTED)
		vc.doXPreIndexedWrite(vc.X & vc.A)
	case 0x84: // STY zeropage
		vc.doZeroPageWrite(vc.Y)
	case 0x85: // STA zeropage
		vc.doZeroPageWrite(vc.A)
	case 0x86: // STX zeropage
		vc.doZeroPageWrite(vc.X)
	case 0x87: // axs zeropage (UNDOCUMENTED)
		vc.doZeroPageWrite(vc.X & vc.A)
	case 0x88: // DEY
		vc.doNoMemOp(func() { vc.setRegOp(&vc.Y, vc.Y-1, vc.setZeroNeg) })
	case 0x89: // skb imm (UNDOCUMENTED)
		_ = vc.doPCFetchCycle()
	case 0x8a: // TXA
		vc.doNoMemOp(func() { vc.setRegOp(&vc.A, vc.X, vc.setZeroNeg) })
	// case 0x8b:
	// machine specific?
	case 0x8c: // STY absolute
		vc.doAbsWrite(vc.Y)
	case 0x8d: // STA absolute
		vc.doAbsWrite(vc.A)
	case 0x8e: // STX absolute
		vc.doAbsWrite(vc.X)
	case 0x8f: // axs absolute (UNDOCUMENTED)
		vc.doAbsWrite(vc.X & vc.A)

	case 0x90: // BCC
		vc.doBranchRel(vc.P&FlagCarry == 0)
	case 0x91: // STA (indirect),y
		vc.doYPostIndexedWrite(vc.A)
	// case 0x93:
	// if you really want to implement this one, look at this:
	// https://forums.nesdev.com/viewtopic.php?f=3&t=10698&start=15#p121151
	case 0x94: // STY zeropage,x
		vc.doIndexedZeroPageWrite(vc.X, vc.Y)
	case 0x95: // STA zeropage,x
		vc.doIndexedZeroPageWrite(vc.X, vc.A)
	case 0x96: // STX zeropage,y
		vc.doIndexedZeroPageWrite(vc.Y, vc.X)
	case 0x97: // axs zeropage,y (UNDOCUMENTED)
		vc.doIndexedZeroPageWrite(vc.Y, vc.X & vc.A)
	case 0x98: // TYA
		vc.doNoMemOp(func() { vc.setRegOp(&vc.A, vc.Y, vc.setZeroNeg) })
	case 0x99: // STA absolute,y
		vc.doIndexedAbsWrite(vc.Y, vc.A)
	case 0x9a: // TXS
		vc.doNoMemOp(func() { vc.setRegOp(&vc.S, vc.X, vc.setNoFlags) })
	// case 0x9b:
	// if you really want to implement this one, look at this:
	// https://forums.nesdev.com/viewtopic.php?f=3&t=10698&start=15#p121151
	// case 0x9c:
	// if you really want to implement this one, look at this:
	// https://forums.nesdev.com/viewtopic.php?f=3&t=10698&start=15#p121151
	case 0x9d: // STA absolute,x
		vc.doIndexedAbsWrite(vc.X, vc.A)
	// case 0x9e:
	// if you really want to implement this one, look at this:
	// https://forums.nesdev.com/viewtopic.php?f=3&t=10698&start=15#p121151
	// case 0x9f:
	// if you really want to implement this one, look at this:
	// https://forums.nesdev.com/viewtopic.php?f=3&t=10698&start=15#p121151

	case 0xa0: // LDY imm
		val := vc.doPCFetchCycle()
		vc.setRegOp(&vc.Y, val, vc.setZeroNeg)
	case 0xa1: // LDA (indirect,x)
		val := vc.doXPreIndexedRead()
		vc.setRegOp(&vc.A, val, vc.setZeroNeg)
	case 0xa2: // LDX imm
		val := vc.doPCFetchCycle()
		vc.setRegOp(&vc.X, val, vc.setZeroNeg)
	case 0xa3: // lax (indirect,x) (UNDOCUMENTED)
		val := vc.doXPreIndexedRead()
		vc.setRegOp(&vc.A, val, vc.setZeroNeg)
		vc.X = vc.A
	case 0xa4: // LDY zeropage
		val := vc.doZeroPageRead()
		vc.setRegOp(&vc.Y, val, vc.setZeroNeg)
	case 0xa5: // LDA zeropage
		val := vc.doZeroPageRead()
		vc.setRegOp(&vc.A, val, vc.setZeroNeg)
	case 0xa6: // LDX zeropage
		val := vc.doZeroPageRead()
		vc.setRegOp(&vc.X, val, vc.setZeroNeg)
	case 0xa7: // lax zeropage (UNDOCUMENTED)
		val := vc.doZeroPageRead()
		vc.setRegOp(&vc.A, val, vc.setZeroNeg)
		vc.X = vc.A
	case 0xa8: // TAY
		vc.doNoMemOp(func() { vc.setRegOp(&vc.Y, vc.A, vc.setZeroNeg) })
	case 0xa9: // LDA imm
		val := vc.doPCFetchCycle()
		vc.setRegOp(&vc.A, val, vc.setZeroNeg)
	case 0xaa: // TAX
		vc.doNoMemOp(func() { vc.setRegOp(&vc.X, vc.A, vc.setZeroNeg) })
	case 0xab: // lax imm (UNDOCUMENTED) (TODO: Is this right? Only for NES? Some docs have different (weirder) ops for this byte...)
		val := vc.doPCFetchCycle()
		vc.setRegOp(&vc.A, val, vc.setZeroNeg)
		vc.X = vc.A
	case 0xac: // LDY absolute
		val := vc.doAbsRead()
		vc.setRegOp(&vc.Y, val, vc.setZeroNeg)
	case 0xad: // LDA absolute
		val := vc.doAbsRead()
		vc.setRegOp(&vc.A, val, vc.setZeroNeg)
	case 0xae: // LDX absolute
		val := vc.doAbsRead()
		vc.setRegOp(&vc.X, val, vc.setZeroNeg)
	case 0xaf: // lax absolute (UNDOCUMENTED)
		val := vc.doAbsRead()
		vc.setRegOp(&vc.A, val, vc.setZeroNeg)
		vc.X = vc.A

	case 0xb0: // BCS
		vc.doBranchRel(vc.P&FlagCarry == FlagCarry)
	case 0xb1: // LDA (indirect),y
		val := vc.doYPostIndexedRead()
		vc.setRegOp(&vc.A, val, vc.setZeroNeg)
	case 0xb3: // lax (indirect),y (UNDOCUMENTED)
		val := vc.doYPostIndexedRead()
		vc.setRegOp(&vc.A, val, vc.setZeroNeg)
		vc.X = vc.A
	case 0xb4: // LDY zeropage,x
		val := vc.doIndexedZeroPageRead(vc.X)
		vc.setRegOp(&vc.Y, val, vc.setZeroNeg)
	case 0xb5: // LDA zeropage,x
		val := vc.doIndexedZeroPageRead(vc.X)
		vc.setRegOp(&vc.A, val, vc.setZeroNeg)
	case 0xb6: // LDX zeropage,y
		val := vc.doIndexedZeroPageRead(vc.Y)
		vc.setRegOp(&vc.X, val, vc.setZeroNeg)
	case 0xb7: // lax zeropage,y (UNDOCUMENTED)
		val := vc.doIndexedZeroPageRead(vc.Y)
		vc.setRegOp(&vc.A, val, vc.setZeroNeg)
		vc.X = vc.A
	case 0xb8: // CLV
		vc.doNoMemOp(func() { vc.P &^= FlagOverflow })
	case 0xb9: // LDA absolute,y
		val := vc.doIndexedAbsRead(vc.Y)
		vc.setRegOp(&vc.A, val, vc.setZeroNeg)
	case 0xba: // TSX
		vc.doNoMemOp(func() { vc.setRegOp(&vc.X, vc.S, vc.setZeroNeg) })
	case 0xbb: // las absolute,y (UNDOCUMENTED)
		val := vc.doIndexedAbsRead(vc.Y)
		vc.setRegOp(&vc.A, val&vc.S, vc.setZeroNeg)
	case 0xbc: // LDY absolute,x
		val := vc.doIndexedAbsRead(vc.X)
		vc.setRegOp(&vc.Y, val, vc.setZeroNeg)
	case 0xbd: // LDA absolute,x
		val := vc.doIndexedAbsRead(vc.X)
		vc.setRegOp(&vc.A, val, vc.setZeroNeg)
	case 0xbe: // LDX absolute,y
		val := vc.doIndexedAbsRead(vc.Y)
		vc.setRegOp(&vc.X, val, vc.setZeroNeg)
	case 0xbf: // lax absolute,y (UNDOCUMENTED)
		val := vc.doIndexedAbsRead(vc.Y)
		vc.setRegOp(&vc.A, val, vc.setZeroNeg)
		vc.X = vc.A

	case 0xc0: // CPY imm
		val := vc.doPCFetchCycle()
		vc.cmpOp(vc.Y, val)
	case 0xc1: // CMP (indirect,x)
		val := vc.doXPreIndexedRead()
		vc.cmpOp(vc.A, val)
	case 0xc2: // skb imm (UNDOCUMENTED)
		_ = vc.doPCFetchCycle()
	case 0xc3: // dcm (indirect,x) (UNDOCUMENTED)
		vc.doXPreIndexedReadModWrite(vc.dcmAndSetFlags)
	case 0xc4: // CPY zeropage
		val := vc.doZeroPageRead()
		vc.cmpOp(vc.Y, val)
	case 0xc5: // CMP zeropage
		val := vc.doZeroPageRead()
		vc.cmpOp(vc.A, val)
	case 0xc6: // DEC zeropage
		vc.doZeroPageReadModWrite(vc.decAndSetFlags)
	case 0xc7: // dcm zeropage (UNDOCUMENTED)
		vc.doZeroPageReadModWrite(vc.dcmAndSetFlags)
	case 0xc8: // INY
		vc.doNoMemOp(func() { vc.setRegOp(&vc.Y, vc.Y+1, vc.setZeroNeg) })
	case 0xc9: // CMP imm
		val := vc.doPCFetchCycle()
		vc.cmpOp(vc.A, val)
	case 0xca: // DEX
		vc.doNoMemOp(func() { vc.setRegOp(&vc.X, vc.X-1, vc.setZeroNeg) })
	case 0xcb: // sax (UNDOCUMENTED)
		val := vc.doPCFetchCycle()
		reg := vc.X & vc.A
		vc.cmpOp(reg, val)
		vc.X = reg-val
	case 0xcc: // CPY absolute
		val := vc.doAbsRead()
		vc.cmpOp(vc.Y, val)
	case 0xcd: // CMP absolute
		val := vc.doAbsRead()
		vc.cmpOp(vc.A, val)
	case 0xce: // DEC absolute
		vc.doAbsReadModWrite(vc.decAndSetFlags)
	case 0xcf: // dcm absolute (UNDOCUMENTED)
		vc.doAbsReadModWrite(vc.dcmAndSetFlags)

	case 0xd0: // BNE
		vc.doBranchRel(vc.P&FlagZero == 0)
	case 0xd1: // CMP (indirect),y
		val := vc.doYPostIndexedRead()
		vc.cmpOp(vc.A, val)
	case 0xd3: // dcm (indirect),y (UNDOCUMENTED)
		vc.doYPostIndexedReadModWrite(vc.dcmAndSetFlags)
	case 0xd4: // skb zeropage,x (UNDOCUMENTED)
		_ = vc.doIndexedZeroPageRead(vc.X)
	case 0xd5: // CMP zeropage,x
		val := vc.doIndexedZeroPageRead(vc.X)
		vc.cmpOp(vc.A, val)
	case 0xd6: // DEC zeropage,x
		vc.doIndexedZeroPageReadModWrite(vc.X, vc.decAndSetFlags)
	case 0xd7: // dcm zeropage,x (UNDOCUMENTED)
		vc.doIndexedZeroPageReadModWrite(vc.X, vc.dcmAndSetFlags)
	case 0xd8: // CLD
		vc.doNoMemOp(func() { vc.P &^= FlagDecimal })
	case 0xd9: // CMP absolute,y
		val := vc.doIndexedAbsRead(vc.Y)
		vc.cmpOp(vc.A, val)
	case 0xda: // nop (UNDOCUMENTED)
		vc.doNoMemOp(func() {})
	case 0xdb: // dcm absolute,y (UNDOCUMENTED)
		vc.doIndexedAbsReadModWrite(vc.Y, vc.dcmAndSetFlags)
	case 0xdc: // skw absolute,x (UNDOCUMENTED)
		_ = vc.doIndexedAbsRead(vc.X)
	case 0xdd: // CMP absolute,x
		val := vc.doIndexedAbsRead(vc.X)
		vc.cmpOp(vc.A, val)
	case 0xde: // DEC absolute,x
		vc.doIndexedAbsReadModWrite(vc.X, vc.decAndSetFlags)
	case 0xdf: // dcm absolute,x (UNDOCUMENTED)
		vc.doIndexedAbsReadModWrite(vc.X, vc.dcmAndSetFlags)

	case 0xe0: // CPX imm
		val := vc.doPCFetchCycle()
		vc.cmpOp(vc.X, val)
	case 0xe1: // SBC (indirect,x)
		val := vc.doXPreIndexedRead()
		vc.A = vc.sbcAndSetFlags(val)
	case 0xe2: // skb imm (UNDOCUMENTED)
		_ = vc.doPCFetchCycle()
	case 0xe3: // isc (indirect,x) (UNDOCUMENTED)
		vc.doXPreIndexedReadModWrite(vc.iscAndSetFlags)
	case 0xe5: // SBC zeropage
		val := vc.doZeroPageRead()
		vc.A = vc.sbcAndSetFlags(val)
	case 0xe4: // CPX zeropage
		val := vc.doZeroPageRead()
		vc.cmpOp(vc.X, val)
	case 0xe6: // INC zeropage
		vc.doZeroPageReadModWrite(vc.incAndSetFlags)
	case 0xe7: // isc zeropage (UNDOCUMENTED)
		vc.doZeroPageReadModWrite(vc.iscAndSetFlags)
	case 0xe8: // INX
		vc.doNoMemOp(func() { vc.setRegOp(&vc.X, vc.X+1, vc.setZeroNeg) })
	case 0xe9: // SBC imm
		val := vc.doPCFetchCycle()
		vc.A = vc.sbcAndSetFlags(val)
	case 0xea: // NOP
		vc.doNoMemOp(func() {})
	case 0xeb: // sbc imm (UNDOCUMENTED)
		val := vc.doPCFetchCycle()
		vc.A = vc.sbcAndSetFlags(val)
	case 0xec: // CPX absolute
		val := vc.doAbsRead()
		vc.cmpOp(vc.X, val)
	case 0xed: // SBC absolute
		val := vc.doAbsRead()
		vc.A = vc.sbcAndSetFlags(val)
	case 0xee: // INC absolute
		vc.doAbsReadModWrite(vc.incAndSetFlags)
	case 0xef: // isc absolute (UNDOCUMENTED)
		vc.doAbsReadModWrite(vc.iscAndSetFlags)

	case 0xf0: // BEQ
		vc.doBranchRel(vc.P&FlagZero == FlagZero)
	case 0xf1: // SBC (indirect),y
		val := vc.doYPostIndexedRead()
		vc.A = vc.sbcAndSetFlags(val)
	case 0xf3: // isc (indirect),y (UNDOCUMENTED)
		vc.doYPostIndexedReadModWrite(vc.iscAndSetFlags)
	case 0xf4: // skb zeropage,x (UNDOCUMENTED)
		_ = vc.doIndexedZeroPageRead(vc.X)
	case 0xf5: // SBC zeropage,x
		val := vc.doIndexedZeroPageRead(vc.X)
		vc.A = vc.sbcAndSetFlags(val)
	case 0xf6: // INC zeropage,x
		vc.doIndexedZeroPageReadModWrite(vc.X, vc.incAndSetFlags)
	case 0xf7: // isc zeropage,x (UNDOCUMENTED)
		vc.doIndexedZeroPageReadModWrite(vc.X, vc.iscAndSetFlags)
	case 0xf8: // SED
		vc.doNoMemOp(func() { vc.P |= FlagDecimal })
	case 0xf9: // SBC absolute,y
		val := vc.doIndexedAbsRead(vc.Y)
		vc.A = vc.sbcAndSetFlags(val)
	case 0xfa: // nop (UNDOCUMENTED)
		vc.doNoMemOp(func() {})
	case 0xfb: // isc absolute,x (UNDOCUMENTED)
		vc.doIndexedAbsReadModWrite(vc.Y, vc.iscAndSetFlags)
	case 0xfc: // skw absolute,x (UNDOCUMENTED)
		_ = vc.doIndexedAbsRead(vc.X)
	case 0xfd: // SBC absolute,x
		val := vc.doIndexedAbsRead(vc.X)
		vc.A = vc.sbcAndSetFlags(val)
	case 0xfe: // INC absolute,x
		vc.doIndexedAbsReadModWrite(vc.X, vc.incAndSetFlags)
	case 0xff: // isc absolute,x (UNDOCUMENTED)
		vc.doIndexedAbsReadModWrite(vc.X, vc.iscAndSetFlags)

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

func (vc *Virt6502) sloAndSetFlags(val byte) byte {
	shifted := vc.aslAndSetFlags(val)
	vc.setRegOp(&vc.A, vc.A|shifted, vc.setZeroNeg)
	return shifted
}

func (vc *Virt6502) lsrAndSetFlags(val byte) byte {
	result := val >> 1
	vc.setCarryFlag(val&0x01 == 0x01)
	vc.setZeroNeg(result)
	return result
}

func (vc *Virt6502) sreAndSetFlags(val byte) byte {
	shifted := vc.lsrAndSetFlags(val)
	vc.setRegOp(&vc.A, vc.A^shifted, vc.setZeroNeg)
	return shifted
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


func (vc *Virt6502) rraAndSetFlags(val byte) byte {
	rotated := vc.rorAndSetFlags(val)
	vc.A = vc.adcAndSetFlags(rotated)
	return rotated
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

func (vc *Virt6502) rlaAndSetFlags(val byte) byte {
	rotated := vc.rolAndSetFlags(val)
	vc.setRegOp(&vc.A, vc.A&rotated, vc.setZeroNeg)
	return rotated
}

func (vc *Virt6502) bitAndSetFlags(val byte) {
	vc.P &^= 0xC0
	vc.P |= val & 0xC0
	vc.setZeroFlag(vc.A&val == 0)
}

func (vc *Virt6502) decAndSetFlags(val byte) byte {
	result := val-1
	vc.setZeroNeg(result)
	return result
}

func (vc *Virt6502) dcmAndSetFlags(val byte) byte {
	result := val-1
	vc.cmpOp(vc.A, result)
	return result
}

func (vc *Virt6502) incAndSetFlags(val byte) byte {
	result := val+1
	vc.setZeroNeg(result)
	return result
}

func (vc *Virt6502) iscAndSetFlags(val byte) byte {
	result := val+1
	vc.A = vc.sbcAndSetFlags(result)
	return result
}
