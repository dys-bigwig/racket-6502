foo = $42
start: LDX $01
  LDA $20,x
end:
  JMP start


.db $01 $0A $FF $CD
