package virt6502

import "fmt"

const (
	flagNeg         = 0x80
	flagOverflow    = 0x40
	flagOnStack     = 0x20
	flagBrk         = 0x10
	flagDecimal     = 0x08
	flagIrqDisabled = 0x04
	flagZero        = 0x02
	flagCarry       = 0x01
)

type Virt6502 struct {
	PC            uint16
	P, A, X, Y, S byte

	IRQ, BRK, NMI, RESET bool
	LastStepsP           byte

	RunCycles func(uint)
	Write     func(uint16, byte)
	Read      func(uint16) byte
	Err       func(error)

	Steps uint64
}

func (vc *Virt6502) push16(val uint16) {
	vc.push(byte(val >> 8))
	vc.push(byte(val))
}
func (vc *Virt6502) push(val byte) {
	vc.Write(0x100+uint16(vc.S), val)
	vc.S--
}

func (vc *Virt6502) pop16() uint16 {
	val := uint16(vc.pop())
	val |= uint16(vc.pop()) << 8
	return val
}
func (vc *Virt6502) pop() byte {
	vc.S++
	result := vc.Read(0x100 + uint16(vc.S))
	return result
}

// interrupt info lags behind actual P flag,
// so we need the delay provided by having
// a LastStepsP
func (vc *Virt6502) interruptsEnabled() bool {
	return vc.LastStepsP&flagIrqDisabled == 0
}

func (vc *Virt6502) handleInterrupts() {
	if vc.RESET {
		vc.RESET = false
		vc.PC = vc.Read16(0xfffc)
		vc.S -= 3
		vc.P |= flagIrqDisabled
	} else if vc.BRK {
		vc.BRK = false
		vc.push16(vc.PC + 1)
		vc.push(vc.P | flagBrk | flagOnStack)
		vc.P |= flagIrqDisabled
		vc.PC = vc.Read16(0xfffe)
	} else if vc.NMI {
		vc.NMI = false
		vc.push16(vc.PC)
		vc.push(vc.P | flagOnStack)
		vc.P |= flagIrqDisabled
		vc.PC = vc.Read16(0xfffa)
	} else if vc.IRQ {
		vc.IRQ = false
		if vc.interruptsEnabled() {
			vc.push16(vc.PC)
			vc.push(vc.P | flagOnStack)
			vc.P |= flagIrqDisabled
			vc.PC = vc.Read16(0xfffe)
		}
	}
	vc.LastStepsP = vc.P
}

func (vc *Virt6502) Step() {
	vc.Steps++
	vc.handleInterrupts()
	vc.stepOpcode()
}

func (vc *Virt6502) Read16(addr uint16) uint16 {
	low := uint16(vc.Read(addr))
	high := uint16(vc.Read(addr + 1))
	return (high << 8) | low
}

func (vc *Virt6502) Write16(addr uint16, val uint16) {
	vc.Write(addr, byte(val))
	vc.Write(addr+1, byte(val>>8))
}

func (vc *Virt6502) DebugStatusLine() string {
	opcode := vc.Read(vc.PC)
	//b2, b3 := vc.Read(vc.PC+1), vc.Read(vc.PC+2)
	//sp := 0x100 + uint16(vc.S)
	//s1, s2, s3 := vc.Read(sp), vc.Read(sp+1), vc.Read(sp+2)
	return fmt.Sprintf("Steps: %09d ", vc.Steps) +
		fmt.Sprintf("PC:%04x ", vc.PC) +
		//fmt.Sprintf("*PC[:3]:%02x%02x%02x ", opcode, b2, b3) +
		//fmt.Sprintf("*S[:3]:%02x%02x%02x ", s1, s2, s3) +
		fmt.Sprintf("opcode:%v ", opcodeNames[opcode]) +
		fmt.Sprintf("A:%02x ", vc.A) +
		fmt.Sprintf("X:%02x ", vc.X) +
		fmt.Sprintf("Y:%02x ", vc.Y) +
		fmt.Sprintf("P:%02x ", vc.P) +
		fmt.Sprintf("S:%02x ", vc.S)
}

func (vc *Virt6502) setOverflowFlag(test bool) {
	if test {
		vc.P |= flagOverflow
	} else {
		vc.P &^= flagOverflow
	}
}

func (vc *Virt6502) setCarryFlag(test bool) {
	if test {
		vc.P |= flagCarry
	} else {
		vc.P &^= flagCarry
	}
}

func (vc *Virt6502) setZeroFlag(test bool) {
	if test {
		vc.P |= flagZero
	} else {
		vc.P &^= flagZero
	}
}

func (vc *Virt6502) setNegFlag(test bool) {
	if test {
		vc.P |= flagNeg
	} else {
		vc.P &^= flagNeg
	}
}

func (vc *Virt6502) setZeroNeg(val byte) {
	if val == 0 {
		vc.P |= flagZero
	} else {
		vc.P &^= flagZero
	}
	if val&0x80 == 0x80 {
		vc.P |= flagNeg
	} else {
		vc.P &^= flagNeg
	}
}

func (vc *Virt6502) setNoFlags(val byte) {}
