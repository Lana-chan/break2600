  processor 6502
  include "vcs.h"
  include "macro.h"

  seg
  ORG $F000
  
FRAME   = $80
POSX    = $81

Reset
  ldx #0
  lda #0
Clear
  sta 0,x
  inx
  bne Clear
  
; init
  lda #%00000001
  sta CTRLPF ; reflect playfield
  
  lda #%01001100
  sta COLUP0 ; paddle color
  
  lda #0
  sta FRAME ; clear variables
  lda #3
  sta POSX

StartOfFrame

; Start of vertical blank processing

  inc FRAME

  lda FRAME
  and #%00010000 ; mask frame count (delay)
  beq NoMove
  lda #0 ; clear frame
  sta FRAME
  inc POSX ; move right
  lda #11 ; right boundary is 11
  cmp POSX
  bcs NoMove
  lda #3 ; left boundary is 3
  sta POSX
NoMove
    
  lda #%00000000
  sta GRP0 ; clear sprite
  
  lda #%01011110
  sta COLUPF ; playfield color
  
  lda #%11111111
  sta PF0 ; make ceiling
  sta PF1
  sta PF2

  lda #0
  sta VBLANK

  lda #2
  sta VSYNC
            
; 3 scanlines of VSYNC signal...
  repeat 3
    sta WSYNC
  repend

  lda #0
  sta VSYNC

; 37 scanlines of vertical blank...
  repeat 37
    sta WSYNC
  repend

; picture start
  ldx #0

; top
  
TopLines ; top 8 scanlines playfield fade
  sta WSYNC
  sbc #1
  sta COLUPF
  inx
  cpx #8
  bne TopLines

; walls ; blank walls
  lda #%01011110
  sta COLUPF
  lda #%00010000
  sta PF0
  lda #0
  sta PF1
  sta PF2
  
MiddleLines
  sta WSYNC
  inx
  cpx #183
  bne MiddleLines
  
; bottom fade
  ldx POSX
WaitPos ; paddle positioning (very rudimentary)
  dex
  bne WaitPos
  sta RESP0
  sta WSYNC

  lda #%01111110 ; sprite top
  sta GRP0
  lda #%01011100
  sta COLUPF
  sta WSYNC
  lda #%11111111 ; sprite middle
  sta GRP0
  lda #%01011100
  
  repeat 6
    sta COLUPF
    sbc #2
    sta WSYNC
  repend
  
  sta COLUPF
  lda #%01111110 ; sprite bottom
  sta GRP0
  dex
  sta WSYNC

  lda #%01000010
  sta VBLANK ; end of screen - enter blanking

; 30 scanlines of overscan...
  repeat 30
    sta WSYNC
  repend
  
  jmp StartOfFrame

  ORG $FFFA

  .word Reset          ; NMI
  .word Reset          ; RESET
  .word Reset          ; IRQ

  END
