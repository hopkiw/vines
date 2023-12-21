#include "./cpu.h"

void CPU::parse() {
}

/*

Logical and arithmetic commands:
ORA $09 $05 $15 $01 $11 $0D $1D $19
AND $29 $25 $35 $21 $31 $2D $3D $39
EOR $49 $45 $55 $41 $51 $4D $5D $59
ADC $69 $65 $75 $61 $71 $6D $7D $79
SBC $E9 $E5 $F5 $E1 $F1 $ED $FD $F9
CMP $C9 $C5 $D5 $C1 $D1 $CD $DD $D9
CPX $E0 $E4 $EC
CPY $C0 $C4 $CC
DEC $C6 $D6 $CE $DE
DEX $CA
DEY $88
INC $E6 $F6 $EE $FE
INX $E8
INY $C8
ASL $0A $06 $16 $0E $1E
ROL $2A $26 $36 $2E $3E
LSR $4A $46 $56 $4E $5E
ROR $6A $66 $76 $6E $7E

Move commands:
LDA $A9 $A5 $B5 $A1 $B1 $AD $BD $B9
STA $85 $95 $81 $91 $8D $9D $99
LDX $A2 $A6 $B6 $AE $BE
STX $86 $96 $8E
LDY $A0 $A4 $B4 $AC $BC
STY $84 $94 $8C
TAX $AA
TXA $8A
TAY $A8
TYA $98
TSX $BA
TXS $9A
PLA $68
PHA $48
PLP $28
PHP $08

Jump/Flag commands:
BPL $10
BMI $30
BVC $50
BVS $70
BCC $90
BCS $B0
BNE $D0
BEQ $F0
BRK $00
RTI $40
JSR $20
RTS $60
JMP $4C $6C
BIT $24 $2C
CLC $18
SEC $38
CLD $D8
SED $F8
CLI $58
SEI $78
CLV $B8
NOP $EA

*/