;===============================================================================
; Program Information
;===============================================================================

    ; Program:      Break2600
    ; Program by:   Svetlana Tovarisch http://mynameiser.in/
    ; Last Update:  March 11, 2016
    ;
    ; Atari 2600 programming exercise
    ;
    ; Thanks to Darrell Spice, Jr for the source for Collect which helped
    ; explain the hardware architecture.
    ; http://atariage.com/forums/blog/blog-148/cat-188-collect
    
;===============================================================================
; Initialize dasm
;===============================================================================

    ; Dasm supports a number of processors, this line tells dasm the code
    ; is for the 6502 CPU.  The Atari has a 6507, which is 6502 that's been
    ; put into a "reduced package".  This package limits the 6507 to an 8K
    ; address space and also removes support for external interrupts.
        PROCESSOR 6502
    
    ; vcs.h contains the standard definitions for TIA and RIOT registers 
        include vcs.h       
    
    ; macro.h contains commonly used routines which aid in coding
        include macro.h

    
;===============================================================================
; Define RAM Usage
;===============================================================================

    ; define a segment for variables
    ; .U means uninitialized, does not end up in ROM
        SEG.U VARS
    
    ; RAM starts at $80
        ORG $80
Frame:    ds 1
PosX:     ds 1
    
;===============================================================================
; Define Start of Cartridge
;===============================================================================

    ; define a segment for code
    SEG CODE    
    
    ; 2K ROM starts at $F800, 4K ROM starts at $F000
    ORG $F000

;===============================================================================
; Initialize Atari
;===============================================================================
    
InitSystem:
    ; CLEAN_START is a macro found in macro.h
    ; it sets all RAM, TIA registers and CPU registers to 0
        CLEAN_START   

        lda #%00000001
        sta CTRLPF ; reflect playfield
  
        lda #%01001100
        sta COLUP0 ; paddle color
        
    ; from here we "fall into" the main loop
    

;===============================================================================
; Main Program Loop
;===============================================================================

Main:
        jsr VerticalSync    ; Jump to SubRoutine VerticalSync
        jsr VerticalBlank   ; Jump to SubRoutine VerticalBlank
        jsr Kernel          ; Jump to SubRoutine Kernel
        jsr OverScan        ; Jump to SubRoutine OverScan
        jmp Main            ; JuMP to Main
    

;===============================================================================
; Vertical Sync
; -------------
; here we generate the signal that tells the TV to move the beam to the top of
; the screen so we can start the next frame of video.
; The Sync Signal must be on for 3 scanlines.
;===============================================================================

VerticalSync:
        lda #2      ; LoaD Accumulator with 2 so D1=1
        ldx #49     ; LoaD X with 49
        sta WSYNC   ; Wait for SYNC (halts CPU until end of scanline)
        sta VSYNC   ; Accumulator D1=1, turns on Vertical Sync signal
        stx TIM64T  ; set timer to go off in 41 scanlines (49 * 64) / 76
        sta WSYNC   ; Wait for Sync - halts CPU until end of 1st scanline of VSYNC
        sta WSYNC   ; wait until end of 2nd scanline of VSYNC
        lda #0      ; LoaD Accumulator with 0 so D1=0
        sta WSYNC   ; wait until end of 3rd scanline of VSYNC
        sta VSYNC   ; Accumulator D1=0, turns off Vertical Sync signal
        rts         ; ReTurn from Subroutine
    
    
;===============================================================================
; Vertical Blank
; --------------
; game logic runs here.
;===============================================================================

VerticalBlank:
        inc Frame

        lda Frame
        and #%00000100 ; mask frame count (delay)
        beq NoMove
        lda #0 ; clear frame
        sta Frame
        inc PosX ; move right
        lda #159 ; right boundary is 159
        cmp PosX
        bcs NoMove
        lda #0 ; left boundary is 0
        sta PosX
NoMove:
        
        rts             ; ReTurn from Subroutine

    
;===============================================================================
; Kernel
; ------
; here we update the registers in TIA (the video chip) in order to generate
; what the player sees.  For now we're just going to output 192 colored
; scanlines lines so we have something to see.
;===============================================================================

Kernel:            
        sta WSYNC       ; Wait for SYNC (halts CPU until end of scanline)
        lda INTIM       ; check the timer
        bne Kernel      ; Branch if its Not Equal to 0
    ; turn on the display
        sta VBLANK      ; Accumulator D1=0, turns off Vertical Blank signal (image output on)
        
        lda #%00000000
        sta GRP0 ; clear sprite
        
        lda #%11111111
        sta PF0 ; make ceiling
        sta PF1
        sta PF2

    ; top 8 scanlines playfield fade
        ldy #0
        lda #%01011110
        sta COLUPF ; playfield color
        
TopLines:
        sta WSYNC
        sbc #1
        sta COLUPF
        iny
        cpy #8
        bne TopLines

      ; walls ; blank walls
        lda #%01011110
        sta COLUPF
        lda #%00010000
        sta PF0
        lda #0
        sta PF1
        sta PF2
        
MiddleLines:
        sta WSYNC
        iny
        cpy #182
        bne MiddleLines

        lda PosX
        ldx #0
        jsr PosObject
        sta WSYNC
        sta HMOVE
        
    ; bottom 8 lines
        lda #%01111110 ; sprite top
        sta GRP0
        lda #%01011100 ; playfield color
        sta COLUPF
        sta WSYNC
        lda #%11111111 ; sprite middle
        sta GRP0
        lda #%01011100
        
        repeat 6
          sta COLUPF ; playfield fade
          sbc #2
          sta WSYNC
        repend
        
        sta COLUPF
        lda #%01111110 ; sprite bottom
        sta GRP0
        dex
        sta WSYNC

        
;===============================================================================
; Overscan
; --------------
; game logic runs here.  Since we don't have any yet, just delay so that the
; entire video frame consists of 262 scanlines
;===============================================================================

OverScan:
        sta WSYNC   ; Wait for SYNC (halts CPU until end of scanline)
        lda #2      ; LoaD Accumulator with 2 so D1=1
        sta VBLANK  ; STore Accumulator to VBLANK, D1=1 turns image output off
        
    ; set the timer for 27 scanlines.  Each scanline lasts 76 cycles,
    ; but the timer counts down once every 64 cycles, so use this
    ; formula to figure out the value to set.  
    ;       (scanlines * 76) / 64    
    ; Also note that it might be slight off due to when on the scanline TIM64T
    ; is updated.  So use Stella to check how many scanlines the code is
    ; generating and adjust accordingly.
        lda #32     ; set timer for 27 scanlines, 32 = ((27 * 76) / 64)
        sta TIM64T  ; set timer to go off in 27 scanlines
        
    ; game logic will go here
    
OSwait:
        sta WSYNC   ; Wait for SYNC (halts CPU until end of scanline)
        lda INTIM   ; Check the timer
        bne OSwait  ; Branch if its Not Equal to 0
        rts         ; ReTurn from Subroutine
        
;===============================================================================
; Object positioning code
;===============================================================================

PosObject:        ; A holds X value
        sec             ; 2  
        sta WSYNC       ; X holds object, 0=P0, 1=P1, 2=M0, 3=M1, 4=Ball
DivideLoop:
        sbc #15         ; 2  
        bcs DivideLoop  ; 2  4
        eor #7          ; 2  6
        asl             ; 2  8
        asl             ; 2 10
        asl             ; 2 12
        asl             ; 2 14
        sta.wx HMP0,X   ; 5 19
        sta RESP0,X     ; 4 23 <- set object position
SLEEP12:
        rts             ; 6 29
    
        
;===============================================================================
; Define End of Cartridge
;===============================================================================
        ORG $FFFA        ; set address to 6507 Interrupt Vectors 
        .WORD InitSystem ; NMI
        .WORD InitSystem ; RESET
        .WORD InitSystem ; IRQ
        
