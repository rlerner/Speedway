game
.L00 ;  include div_mul.asm

.
 ; 

.L01 ;  rem Support Black and White Monitors and hide score based on new color setting.

.L02 ;  if switchbw then COLUBK  =  $00 : COLUPF  =  $0E else COLUBK =  $0E : COLUPF =  $80

 lda #8
 bit SWCHB
	BNE .skipL02
.condpart0
	LDA #$00
	STA COLUBK
	LDA #$0E
	STA COLUPF
 jmp .skipelse0
.skipL02
	LDA #$0E
	STA COLUBK
	LDA #$80
	STA COLUPF
.skipelse0
.L03 ;  if switchbw then scorecolor  =  $00 else scorecolor  =  $0E

 lda #8
 bit SWCHB
	BNE .skipL03
.condpart1
	LDA #$00
	STA scorecolor
 jmp .skipelse1
.skipL03
	LDA #$0E
	STA scorecolor
.skipelse1
.
 ; 

.
 ; 

.
 ; 

.L04 ;  dim timer  =  c

.
 ; 

.
 ; 

.
 ; 

.introloop
 ; introloop

.L05 ;  drawscreen

 jsr drawscreen
.L06 ;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel0
PF_data0
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %11111111, %11111111
	if (pfwidth>2)
	.byte %11111111, %11111111
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %11010001, %10111011
	if (pfwidth>2)
	.byte %00010111, %11001110
 endif
	.byte %00010001, %10101000
	if (pfwidth>2)
	.byte %10010100, %00001010
 endif
	.byte %11010001, %10111001
	if (pfwidth>2)
	.byte %01010110, %11001110
 endif
	.byte %00010001, %10011000
	if (pfwidth>2)
	.byte %00110100, %00000110
 endif
	.byte %11011101, %10101011
	if (pfwidth>2)
	.byte %00010111, %11001010
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %11111111, %11111111
	if (pfwidth>2)
	.byte %11111111, %11111111
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
pflabel0
	lda PF_data0,x
	sta playfield,x
	dex
	bpl pflabel0
.L07 ;  timer  =  timer  +  1

	INC timer
.L08 ;  if timer  =  128 then goto introloop2

	LDA timer
	CMP #128
     BNE .skipL08
.condpart2
 jmp .introloop2

.skipL08
.L09 ;  goto introloop

 jmp .introloop

.
 ; 

.introloop2
 ; introloop2

.L010 ;  timer  =  0

	LDA #0
	STA timer
.L011 ;  drawscreen

 jsr drawscreen
.L012 ;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel1
PF_data1
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %01101100, %11011011
	if (pfwidth>2)
	.byte %00010001, %10100100
 endif
	.byte %01001010, %01001001
	if (pfwidth>2)
	.byte %10010001, %10101010
 endif
	.byte %01101100, %01011011
	if (pfwidth>2)
	.byte %10010001, %01001110
 endif
	.byte %00101000, %01001001
	if (pfwidth>2)
	.byte %10010101, %01001010
 endif
	.byte %01101000, %11011011
	if (pfwidth>2)
	.byte %00001010, %01001010
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %11001100, %00110011
	if (pfwidth>2)
	.byte %11001100, %00110011
 endif
	.byte %00110011, %11001100
	if (pfwidth>2)
	.byte %00110011, %11001100
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
pflabel1
	lda PF_data1,x
	sta playfield,x
	dex
	bpl pflabel1
.L013 ;  if switchreset  ||  joy0fire then goto gameinit

 lda #1
 bit SWCHB
	BNE .skipL013
.condpart3
 jmp .condpart4
.skipL013
 bit INPT4
	BMI .skip0OR
.condpart4
 jmp .gameinit

.skip0OR
.L014 ;  goto introloop2

 jmp .introloop2

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.gameinit
 ; gameinit

.L015 ;  rem Detect B&W Switch and change colors of background, playfield, and score color respectively.

.L016 ;  if switchbw then COLUBK  =  $00 : COLUPF  =  $0E else COLUBK =  $05 : COLUPF =  $1E

 lda #8
 bit SWCHB
	BNE .skipL016
.condpart5
	LDA #$00
	STA COLUBK
	LDA #$0E
	STA COLUPF
 jmp .skipelse2
.skipL016
	LDA #$05
	STA COLUBK
	LDA #$1E
	STA COLUPF
.skipelse2
.L017 ;  if switchbw then scorecolor  =  $0E else scorecolor  =  $40

 lda #8
 bit SWCHB
	BNE .skipL017
.condpart6
	LDA #$0E
	STA scorecolor
 jmp .skipelse3
.skipL017
	LDA #$40
	STA scorecolor
.skipelse3
.
 ; 

.
 ; 

.L018 ;  AUDV0  =  0

	LDA #0
	STA AUDV0
.L019 ;  dim mph  =  a

.L020 ;  dim badguymph  =  b

.L021 ;  mph  =  0

	LDA #0
	STA mph
.L022 ;  badguymph  =  0

	LDA #0
	STA badguymph
.L023 ;  player0x  =  95

	LDA #95
	STA player0x
.L024 ;  player0y  =  70

	LDA #70
	STA player0y
.L025 ;  player1x  =  50

	LDA #50
	STA player1x
.L026 ;  player1y  =  20

	LDA #20
	STA player1y
.L027 ;  score  =  0

	LDA #$00
	STA score+2
	LDA #$00
	STA score+1
	LDA #$00
	STA score
.
 ; 

.L028 ;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel2
PF_data2
	.byte %11010100, %10000000
	if (pfwidth>2)
	.byte %00000000, %11110100
 endif
	.byte %10100100, %00000000
	if (pfwidth>2)
	.byte %00000000, %10010100
 endif
	.byte %11110100, %10000000
	if (pfwidth>2)
	.byte %00000000, %10010100
 endif
	.byte %00000100, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000100
 endif
	.byte %11110100, %10000000
	if (pfwidth>2)
	.byte %00000000, %11110100
 endif
	.byte %10100100, %00000000
	if (pfwidth>2)
	.byte %00000000, %10100100
 endif
	.byte %11110100, %10000000
	if (pfwidth>2)
	.byte %00000000, %11110100
 endif
	.byte %00000100, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000100
 endif
	.byte %10010100, %10000000
	if (pfwidth>2)
	.byte %00000000, %11110100
 endif
	.byte %10010100, %00000000
	if (pfwidth>2)
	.byte %00000000, %10100100
 endif
	.byte %11110100, %10000000
	if (pfwidth>2)
	.byte %00000000, %11010100
 endif
pflabel2
	lda PF_data2,x
	sta playfield,x
	dex
	bpl pflabel2
.
 ; 

.
 ; 

.L029 ;  player0:

	LDX #<playerL029_0
	STX player0pointerlo
	LDA #>playerL029_0
	STA player0pointerhi
	LDA #7
	STA player0height
.
 ; 

.L030 ;  player1:

	LDX #<playerL030_1
	STX player1pointerlo
	LDA #>playerL030_1
	STA player1pointerhi
	LDA #7
	STA player1height
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.mainloop
 ; mainloop

.
 ; 

.L031 ;  rem Joystick controls

.L032 ;  if joy0left then player0x  =  player0x  -  2

 bit SWCHA
	BVS .skipL032
.condpart7
	LDA player0x
	SEC
	SBC #2
	STA player0x
.skipL032
.L033 ;  if joy0right then player0x  =  player0x  +  2

 bit SWCHA
	BMI .skipL033
.condpart8
	LDA player0x
	CLC
	ADC #2
	STA player0x
.skipL033
.L034 ;  if joy0up then player0y  =  player0y  -  2

 lda #$10
 bit SWCHA
	BNE .skipL034
.condpart9
	LDA player0y
	SEC
	SBC #2
	STA player0y
.skipL034
.L035 ;  if joy0down then mph  =  mph  -  1

 lda #$20
 bit SWCHA
	BNE .skipL035
.condpart10
	DEC mph
.skipL035
.
 ; 

.
 ; 

.L036 ;  rem if player hits wall, then slam them back some and away from wall.

.L037 ;  if player0x >= 113 then player0y  =  player0y  +  2 : player0x  =  111 : mph = mph - 5

	LDA player0x
	CMP #113
     BCC .skipL037
.condpart11
	LDA player0y
	CLC
	ADC #2
	STA player0y
	LDA #111
	STA player0x
	LDA mph
	SEC
	SBC #5
	STA mph
.skipL037
.L038 ;  if player0x <= 41 then player0y  =  player0y  +  2 : player0x  =  43 : mph = mph - 5

	LDA #41
	CMP player0x
     BCC .skipL038
.condpart12
	LDA player0y
	CLC
	ADC #2
	STA player0y
	LDA #43
	STA player0x
	LDA mph
	SEC
	SBC #5
	STA mph
.skipL038
.
 ; 

.
 ; 

.L039 ;  rem if the car hits the boundary, then start speeding up

.L040 ;  if player0y < 70 then player0y  =  70 :  mph  =  mph  +  1

	LDA player0y
	CMP #70
     BCS .skipL040
.condpart13
	LDA #70
	STA player0y
	INC mph
.skipL040
.
 ; 

.L041 ;  rem set up vertical boundary to prevent driver from going off screen.

.L042 ;  if player0y > 87 then player0y  =  87

	LDA #87
	CMP player0y
     BCS .skipL042
.condpart14
	LDA #87
	STA player0y
.skipL042
.
 ; 

.L043 ;  if mph  <  1 then mph  =  1

	LDA mph
	CMP #1
     BCS .skipL043
.condpart15
	LDA #1
	STA mph
.skipL043
.L044 ;  if mph  >  100 then mph  =  100

	LDA #100
	CMP mph
     BCS .skipL044
.condpart16
	LDA #100
	STA mph
.skipL044
.
 ; 

.
 ; 

.L045 ;  badguymph  =  mph / 20

	LDA mph
	LDY #20
 jsr div8
	STA badguymph
.
 ; 

.L046 ;  if badguymph < 1 then badguymph  =  1

	LDA badguymph
	CMP #1
     BCS .skipL046
.condpart17
	LDA #1
	STA badguymph
.skipL046
.L047 ;  player1y  =  player1y  +  badguymph

	LDA player1y
	CLC
	ADC badguymph
	STA player1y
.
 ; 

.
 ; 

.L048 ;  if player1y  >  110 then player1y  =  0 : player1x  =   ( rand & 63 )  + 46

	LDA #110
	CMP player1y
     BCS .skipL048
.condpart18
	LDA #0
	STA player1y
; complex statement detected
 jsr randomize
	AND #63
	CLC
	ADC #46
	STA player1x
.skipL048
.
 ; 

.L049 ;  score  =  score  +  badguymph

	SED
	CLC
	LDA score+2
	ADC badguymph

	STA score+2
	LDA score+1
	ADC #0
	STA score+1
	LDA score
	ADC #0
	STA score
	CLD
.
 ; 

.L050 ;  if collision(player0,player1) then goto loseloop

	bit 	CXPPMM
	BPL .skipL050
.condpart19
 jmp .loseloop

.skipL050
.
 ; 

.
 ; 

.
 ; 

.renderloop
 ; renderloop

.
 ; 

.L051 ;  if !switchleftb then NUSIZ1  =  $06

 bit SWCHB
	BVC .skipL051
.condpart20
	LDA #$06
	STA NUSIZ1
.skipL051
.
 ; 

.L052 ;  drawscreen

 jsr drawscreen
.L053 ;  goto mainloop

 jmp .mainloop

.
 ; 

.
 ; 

.
 ; 

.loseloop
 ; loseloop

.L054 ;  drawscreen

 jsr drawscreen
.L055 ;  AUDV0  =  10

	LDA #10
	STA AUDV0
.L056 ;  AUDC0  =  8

	LDA #8
	STA AUDC0
.L057 ;  AUDF0  =  10

	LDA #10
	STA AUDF0
.
 ; 

.L058 ;  if joy0fire then goto gameinit

 bit INPT4
	BMI .skipL058
.condpart21
 jmp .gameinit

.skipL058
.
 ; 

.L059 ;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel3
PF_data3
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000011, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000011
 endif
	.byte %00000011, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000011
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00001111, %11111111
	if (pfwidth>2)
	.byte %11111111, %00001111
 endif
	.byte %00011100, %00000000
	if (pfwidth>2)
	.byte %00000000, %00011100
 endif
	.byte %00011000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00011000
 endif
	.byte %00011000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00011000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
pflabel3
	lda PF_data3,x
	sta playfield,x
	dex
	bpl pflabel3
.
 ; 

.L060 ;  goto loseloop

 jmp .loseloop

.
 ; 

 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL029_0
	.byte  %10111101
	.byte  %11111111
	.byte  %10111101
	.byte  %00111100
	.byte  %00111100
	.byte  %10111101
	.byte  %11111111
	.byte  %10011001
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL030_1
	.byte  %10011001
	.byte  %11110111
	.byte  %10101101
	.byte  %00110100
	.byte  %00101100
	.byte  %10110101
	.byte  %11101111
	.byte  %10110101
 if ECHOFIRST
       echo "    ",[(scoretable - *)]d , "bytes of ROM space left")
 endif 
ECHOFIRST = 1
 
 
 
