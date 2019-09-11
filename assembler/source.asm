foo = $42
start:
  LDX #$22
  LDA $20,x
end:
  JMP start

db $01 $0A $FF $CD
