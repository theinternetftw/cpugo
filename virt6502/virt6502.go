package virt6502

import "fmt"

const (
	FlagNeg         = 0x80
	FlagOverflow    = 0x40
	FlagOnStack     = 0x20
	FlagBrk         = 0x10
	FlagDecimal     = 0x08
	FlagIrqDisabled = 0x04
	FlagZero        = 0x02
	FlagCarry       = 0x01
)

type Virt6502 struct {
	PC            uint16
	P, A, X, Y, S byte

	// IgnoreDecimalMode allows you to force the 6502 to behave like the NES version.
	IgnoreDecimalMode bool

	IRQ, BRK, NMI, RESET bool
	LastStepsP           byte

	RunCycles func(uint) `json:"-"`
	Write     func(uint16, byte) `json:"-"`
	Read      func(uint16) byte `json:"-"`
	Err       func(error) `json:"-"`

	Steps uint64
}

func (vc *Virt6502) Push16(val uint16) {
	vc.Push(byte(val >> 8))
	vc.Push(byte(val))
}
func (vc *Virt6502) Push(val byte) {
	vc.Write(0x100+uint16(vc.S), val)
	vc.S--
}

func (vc *Virt6502) Pop16() uint16 {
	val := uint16(vc.Pop())
	val |= uint16(vc.Pop()) << 8
	return val
}
func (vc *Virt6502) Pop() byte {
	vc.S++
	result := vc.Read(0x100 + uint16(vc.S))
	return result
}

// interrupt info lags behind actual P flag,
// so we need the delay provided by having
// a LastStepsP
func (vc *Virt6502) interruptsEnabled() bool {
	return vc.LastStepsP&FlagIrqDisabled == 0
}

func (vc *Virt6502) handleInterrupts() {
	if vc.RESET {
		vc.RESET = false
		vc.PC = vc.Read16(0xfffc)
		vc.S -= 3
		vc.P |= FlagIrqDisabled
	} else if vc.BRK {
		vc.BRK = false
		vc.Push16(vc.PC + 1)
		vc.Push(vc.P | FlagBrk | FlagOnStack)
		vc.P |= FlagIrqDisabled
		vc.PC = vc.Read16(0xfffe)
	} else if vc.NMI {
		vc.NMI = false
		vc.Push16(vc.PC)
		vc.Push(vc.P | FlagOnStack)
		vc.P |= FlagIrqDisabled
		vc.PC = vc.Read16(0xfffa)
	} else if vc.IRQ {
		vc.IRQ = false
		if vc.interruptsEnabled() {
			vc.Push16(vc.PC)
			vc.Push(vc.P | FlagOnStack)
			vc.P |= FlagIrqDisabled
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
		fmt.Sprintf("opcode:%v/0x%02x ", opcodeNames[opcode], opcode) +
		fmt.Sprintf("A:%02x ", vc.A) +
		fmt.Sprintf("X:%02x ", vc.X) +
		fmt.Sprintf("Y:%02x ", vc.Y) +
		fmt.Sprintf("P:%02x ", vc.P) +
		fmt.Sprintf("S:%02x ", vc.S)
}

func (vc *Virt6502) setFlag(test bool, flag byte) {
	if test {
		vc.P |= flag
	} else {
		vc.P &^= flag
	}
}

func (vc *Virt6502) setOverflowFlag(test bool) {
	vc.setFlag(test, FlagOverflow)
}
func (vc *Virt6502) setCarryFlag(test bool) {
	vc.setFlag(test, FlagCarry)
}
func (vc *Virt6502) setZeroFlag(test bool) {
	vc.setFlag(test, FlagZero)
}
func (vc *Virt6502) setNegFlag(test bool) {
	vc.setFlag(test, FlagNeg)
}

func (vc *Virt6502) setZeroNeg(val byte) {
	vc.setFlag(val == 0, FlagZero)
	vc.setFlag(val&0x80 != 0, FlagNeg)
}

func (vc *Virt6502) setNoFlags(val byte) {}
