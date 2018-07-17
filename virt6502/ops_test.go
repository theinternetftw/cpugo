package virt6502

import "fmt"
import "testing"

func fmtFlags(flags byte) string {
	out := []byte{'_', '_', '_', '_', '_', '_', '_', '_'}
	if flags&FlagNeg != 0 {
		out[0] = byte('N')
	}
	if flags&FlagOverflow != 0 {
		out[1] = byte('V')
	}
	if flags&FlagBrk != 0 {
		out[3] = byte('B')
	}
	if flags&FlagDecimal != 0 {
		out[4] = byte('D')
	}
	if flags&FlagIrqDisabled != 0 {
		out[5] = byte('I')
	}
	if flags&FlagZero != 0 {
		out[6] = byte('Z')
	}
	if flags&FlagCarry != 0 {
		out[7] = byte('C')
	}
	return fmt.Sprintf("0x%02x(%s)", flags, out)
}

type arithTest struct {
	regA       byte
	val        byte
	regP       byte
	result     byte
	resultRegP byte
}

func (a arithTest) String() string {
	return fmt.Sprintf(
		"0x%02x+0x%02x,C=%v==0x%02x,%s",
		a.regA,
		a.val,
		a.regP&FlagCarry == FlagCarry,
		a.result,
		fmtFlags(a.resultRegP),
	)
}

func (a arithTest) runTest(t *testing.T, fnToTest func(vc *Virt6502, val byte) byte) {
	name := fmt.Sprintf("%v", a)
	t.Run(name, func(t *testing.T) {
		//t.Parallel()

		vc := Virt6502{
			P: a.regP,
			A: a.regA,
		}
		result := fnToTest(&vc, a.val)
		if result != a.result {
			t.Errorf("got result val of 0x%02x, expected 0x%02x", result, a.result)
		}
		if vc.P != a.resultRegP {
			t.Errorf("got result regP of %s, expected %s", fmtFlags(vc.P), fmtFlags(a.resultRegP))
		}
	})
}

func TestADCDecimalMode(t *testing.T) {
	const c = FlagCarry
	adcTests := []arithTest{
		{0x00, 0x00, 0, 0x00, FlagZero},
		{0x79, 0x00, c, 0x80, FlagNeg | FlagOverflow},
		{0x24, 0x56, 0, 0x80, FlagNeg | FlagOverflow},
		{0x93, 0x82, 0, 0x75, FlagOverflow | FlagCarry},
		{0x89, 0x76, 0, 0x65, FlagCarry},
		{0x89, 0x76, c, 0x66, FlagZero | FlagCarry},
		{0x80, 0xf0, 0, 0xd0, FlagOverflow | FlagCarry},
		{0x80, 0xfa, 0, 0xe0, FlagNeg | FlagCarry},
		{0x2f, 0x4f, 0, 0x74, 0},
		{0x6f, 0x00, c, 0x76, 0},
	}

	for _, test := range adcTests {
		test.regP |= FlagDecimal
		test.resultRegP |= FlagDecimal

		test.runTest(t, func(vc *Virt6502, val byte) byte {
			return vc.adcAndSetFlags(val)
		})
	}
}

func TestSBCDecimalMode(t *testing.T) {
	const c = FlagCarry // NOTE: remember carry is inverted for sbc
	sbcTests := []arithTest{
		{0x00, 0x00, 0, 0x99, FlagNeg},
		{0x00, 0x00, c, 0x00, FlagZero | FlagCarry},
		{0x00, 0x01, c, 0x99, FlagNeg},
		{0x0a, 0x00, c, 0x0a, FlagCarry},
		{0x0b, 0x00, 0, 0x0a, FlagCarry},
		{0x9a, 0x00, c, 0x9a, FlagNeg | FlagCarry},
		{0x9b, 0x00, 0, 0x9a, FlagNeg | FlagCarry},
	}

	for _, test := range sbcTests {
		test.regP |= FlagDecimal
		test.resultRegP |= FlagDecimal

		test.runTest(t, func(vc *Virt6502, val byte) byte {
			return vc.sbcAndSetFlags(val)
		})
	}
}
