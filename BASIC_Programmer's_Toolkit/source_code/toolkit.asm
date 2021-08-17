;   toolkit.h65,  ReSource65: v 0.91a,  Jan 23 2008
;ÿ
;   The BASIC Programmer's Toolkit
;   ==============================
;   (C) 1979, Palo Alto ICs
;
;   Written and reconstructed (resuscitated?, resurrected?)
;   by Chuck Bond.
;
;   Thanks to Harry J. Saal and Len Shustek for invaluable
;   technical advice and assistance with the Toolkit project.
;   Special thanks to Dr. Saal for recognizing the potential
;   of the Toolkit.
;
;   Thanks also to Jim Butterfield for providing indispensable
;   memory maps for Commodore computers. His work spawned not
;   only the Toolkit, but also innumerable other PET programs,
;   addons and accessories.
;
;   This source file is written for the cbA65 assembler and
;   Commodore PET BASIC ROM version 2.0 (sometimes called the
;   'upgrade' ROM).
;
;*************************************************************
;
;   The BASIC Programmer's Toolkit is a BASIC extender package
;   for Commodore PET computers. It adds the following commands
;   to the resident BASIC interpreter:
;
;       AUTO        Automatic line numbering
;       STEP        Execute program one line at a time
;       TRACE       Tracks executing lines by line number
;       OFF         Exits from TRACE or STEP mode
;       RENUMBER    Renumbers program lines
;       DELETE      Deletes a range of program lines
;       HELP        Displays line with last error highlighted
;       FIND        Locate lines with given keywords or strings
;       DUMP        Display all program variables with values
;       APPEND      Adds tape program to end of current program
;
;**************************************************************
;
;   The BASIC Programmer's Toolkit is provided in a 2k
;   (2048 byte) ROM with optional card, socket and connector. 
;
;   After installation, power up the PET and invoke the Toolkit
;   with:
;
;       SYS(45056)
;
;   NOTE: 45056 = $B000
;
;   Uninstall with:
;
;       SYS(57622)
;
;**************************************************************
;
;   Miscellaneous BASIC Tidbits
;   ===========================
;
;   The format of BASIC program lines is:
;
;       line link | line number | tokenized text       | NULL
;       ----------|-------------|----------------....--|------
;         LLLL         NNNN       XXXXXXXX...........X    0
;          |
;          v
;         LLLL         NNNN       XXXX...............X    0
;          .
;          .
;          v
;         NULL    (End Of Program)
;
;    where LLLL is a 2-byte pointer to the next line link, and
;    NNNN is a 2-byte integer < 64000. The 1st byte of the 1st
;    link is at $0401.
;
;    The largest line number supported by BASIC is 63999.
;
;-------------------------------------------------------------
;
;   BASIC variable names consist of one or two characters. The
;   first (or only) character is alphabetic and the second
;   (if any) may be alphabetic or numeric. If no second character
;   is specified, a blank is used.
;
;   The variable type is indicated by the high bit of each 8-bit
;   character.  Integers are indicated with both high bits set.
;   Strings are indicated with the high bit of the first
;   character set. Floating point variables have neither high
;   bit set.
;
.title "BASIC Programmer's Toolkit for Commodore Pet BASIC v2.0"
.files xref,h65,rom=2048
.print cols=100,stats
;
;   Constant equates and 'magic' numbers
;
CR          .equ $0D        ; ASCII carriage return
RDY         .equ $AE        ; offset in ROM to "READY." message
VBLNK       .equ $20        ; mask for vertical blank bit in VIA
OFFM        .equ $00        ; turns off AUTO, TRACE or STEP
POSMSK      .equ $7F        ; mask to force positive byte
AUTOM       .equ $01        ; AUTO mode marker
TRACEM      .equ $02        ; TRACE mode marker
STEPM       .equ $03        ; STEP mode marker
SCREEN      .equ $8000      ; SCREEN RAM starting location
TKNTO       .equ $A4        ; BASIC token for TO statement
TKNMIN      .equ $AB        ; BASIC token for minus sign: -
MAXLIN      .equ $F9FF      ; maximum line number for BASIC (63999)
BASPG       .equ $C3        ; BASIC page for direct mode CHRGET call
;
;   Zero page locations
;
COUNT       .equ $0005  ; temporary byte storage for counters, etc.
FLAG1       .equ $0009  ; Misc. flag or index value
TMPINT      .equ $0011  ; integer temp
STRPTR      .equ $001F  ; pointer to string variable
TMPTR0      .equ $0021  ; temporary pointer to BASIC text
BASPTR      .equ $0028  ; pointer to start of BASIC program
VARPTR      .equ $002A  ; pointer to start of BASIC variables
ARRPTR      .equ $002C  ; pointer to start of BASIC arrays
MEMSZ       .equ $0034  ; highest memory location used by BASIC
CURRLN      .equ $0036  ; current BASIC line number
VARNAM      .equ $0042  ; current VARIABLE name
VARADR      .equ $0044  ; address of VARIABLE
TMPTR1      .equ $0055  ; temporary storage for integers or pointers
TMPTR2      .equ $0057  ;    "                     "
TMPTR3      .equ $005C  ;    "                     "
FACCM       .equ $005F  ; f.p. mantissa (int. storage for conversion)
CHRGET      .equ $0070  ; The infamous BASIC character/token fetch
CHRGOT      .equ $0076  ; get last character
TXTPTR      .equ $0077  ; pointer into BASIC program
MODE        .equ $007C  ; OFF, AUTO, STEP or TRACE marker (0,1,2 or 3)
LINNUM1     .equ $0080  ; work area and storage for BASIC line    
LINNUM2     .equ $0082  ;  numbers used by AUTO and RENUMBER
TMPTR4      .equ $0084  ; temporary pointer 
TMPTR5      .equ $0086  ; temporary pointer or byte storage (see below)
TMPERR      .equ $0086  ; temporary offset to char used by HELP routine
SAVEX       .equ $0087  ; temporary storage for BASIC 'x' register
STATUS      .equ $0096  ; BASIC status byte
SHIFTKEY    .equ $0098  ; SHIFT key pressed = 1, not pressed = 0
LDVERF      .equ $009D  ; LOAD/VERIFY flag for cassette
NUMCHR      .equ $009E  ; number of characters in keyboard buffer
REVFLG      .equ $009F  ; screen reverse field flag
TAPTMP1     .equ $00C7  ; temporary pointers for tape handler
TAPTMP2     .equ $00C9  ;    "         "           "
FNLEN       .equ $00D1  ; number of characters in file name
DEVID       .equ $00D4  ; device ID
TAPBUFF     .equ $00D6  ; pointer to start of tape buffer
;
;   stack page
;
STKPAG      .equ $0100
;
;   BASIC input buffer
;
INBUFF      .equ $0200 ; direct mode input buffer and work area
KEYBUFF     .equ $026F ; BASIC keyboard buffer
;
;   cassette buffer
;
SAFTMP1     .equ $03E0 ; safe temporary when cassette #2 not used
SAFTMP2     .equ $03E1 ;  "              "                "
STEPSZ      .equ $03E2 ; line spacing for AUTO and RENUMBER
LNBUFF      .equ $03E4 ; Line buffer for TRACE and STEP scrolling
TRCBUF      .equ $03E6 ; buffer for displayed TRACE lines (integers)
;
;   BASIC entry points
;
KWDLST      .equ $C092 ; list of BASIC keywords
PRTERR      .equ $C355 ; print error message
ERRMSG      .equ $C377 ; print ERROR
RDYMSG      .equ $C389 ; print READY.
FIXLINKS    .equ $C439 ; reset BASIC line links
LOOKUP      .equ $C495 ; get BASIC token for keyword
FINDLINE    .equ $C52C ; get ptr to BASIC line. num: ($11), ptr: ($5C)
RST_PTRS    .equ $C572 ; reset BASIC pointers to default values
RD_INT      .equ $C873 ; convert ASCII string to integer in $11,$12
PUT_CRLF    .equ $C9DE ; print CR/LF to device
PUT_STRING  .equ $CA1C ; prepare and print ASCII string to device
PRTSTR      .equ $CA22 ; print string
PUTCHR      .equ $CA45 ; print character
SYNERR      .equ $CE03 ; display ?SYNTAX ERROR msg
LOCVAR      .equ $D069 ; find an f.p. variable by name
FX2FLT      .equ $D26D ; convert fixed point to floating point
LDFACC      .equ $DAAE ; load f.p. number fo FACC1
INT2FP      .equ $DB55 ; convert integer to f.p.
PRTLIN      .equ $DCD5 ; print line number
PRTINT      .equ $DCD9 ; print integer in a(hi),x(lo)
CNV_PRT     .equ $DCE3 ; convert f.p. in FACC1 to ASCII and print
FP2ASC      .equ $DCE9 ; convert to ASCII string at bottom of stack
;
;   Screen editor
;
NUMCHK      .equ $E10A ; clear carry if 'a' contains ASCII digit
;
;   I/O ports
VIA         .equ $E840 ; misc. operating system flags and VBLANK
;
;   Kernel (kernal) entry points
;
PRTMON      .equ $F156  ; print MONITOR message at index in 'y'
PRTLOAD     .equ $F3E6  ; print LOAD message
PRTRDY      .equ $F3EF  ; print READY. message
PRTSRCH     .equ $F40A  ; print SEARCHING message
GETPARM     .equ $F43E  ; get device parameters
GETHDR      .equ $F494  ; get tape program header
PRTFNF      .equ $F56E  ; print FILE NOT FOUND message
SRCH_HDR    .equ $F5A6  ; search tape for next header
RD_HDR      .equ $F63C  ; get tape program start & end addresses  
SET_BUFF    .equ $F656  ; set buffer start address
WT_PLAY     .equ $F812  ; wait for cassette PLAY switch
RD_TAPE     .equ $F85E  ; read cassette tape
WT_IO       .equ $F8E6  ; wait for I/O completion
STOPKEY     .equ $FFE1  ; check for stop key and restart BASIC 
;
;   Toolkit initialization entry
;
;   Call with SYS(45056)
;
            .org $B000
TOOLKIT     jmp INITROM        ; Toolkit initialization.
;
;  Initialize line numbering values for AUTO and RENUMBER
;
INITVAR     lda #10            ; Default BASIC line spacing.
            sta STEPSZ         ; Reference storage location.
            lda #$00            
            sta STEPSZ+1       
            sta LINNUM2+1       
            sta MODE            
            sta LINNUM1+1       
            lda #100           ; Default BASIC starting line.
            sta LINNUM2        ; Reference location.
            sta LINNUM1        ; Working location.
            rts         
;
;  Get line numbering parameters: STARTING LINE, STEP SIZE
;
LINPARMS    jsr CHRGET         ; Check for numeric parameter.
            beq PARMEX         ; No parameters, use defaults
            bcs PRMERR         ; IF not a number, error.
            jsr RD_INT         ; ELSE get integer to TMPINT
            pha                ; Save number terminator.
            lda TMPINT+1       ; ASCII number is returned
            ldx TMPINT         ;  as binary integer here.
            sta LINNUM2+1      ; Replace default starting
            stx LINNUM2        ;  line number.
            sta LINNUM1+1      ; Initialize working value.
            stx LINNUM1        
            pla                ; Check terminator.
            beq PARMEX         ; No step size, use default
            cmp #','           ; IF separator, get next
            beq @F             ;  number.
PRMERR      jmp SYNERR         ; ELSE print error.
@           jsr CHRGET          
            bcs PRMERR         ; IF not a number, error.
            jsr RD_INT         ; ELSE get line spacing
            pha                ;  (STEPSIZE) to TMPINT.
            lda TMPINT+1       ; Copy to safe place.
            ldx TMPINT          
            sta STEPSZ+1        
            stx STEPSZ          
            pla                 
            bne PRMERR         ; Extra characters on line?
PARMEX      rts                ; No, we're done.
;
; Update BASIC line number for AUTO and RENUMBER
;
NXTLIN      clc                ; Add line step size to 
            lda LINNUM1        ;  current line number.
            adc STEPSZ          
            sta LINNUM1         
            lda LINNUM1+1
            adc STEPSZ+1       
            sta LINNUM1+1       
            bcs @F              
            cmp >(MAXLIN+1)    ; Set carry if invalid #.
@           rts                ; Carry clear if OK.
;
;       RENUMBER
;
;  Accepts:
;            RENUMBER               Uses default values.
;            RENUMBER start         Uses given start, default step.
;            RENUMBER start,step    Uses given start, step
;
;  The strategy employed to renumber a BASIC program is as follows:
;
;       1) Get new starting line number and number step size.
;       2) Check the validity of the numbers by calculating
;           the new (trial) line numbers from program start to
;           end. Do not alter the program in this phase.
;       3) IF the last program line number is too large, exit.
;       4) ELSE keep the existing line numbers and line links
;           intact while scanning program text for references
;           to (old) line numbers.
;       5) Calculate new line number for each reference to an
;           old line number, and replace old number with new one.
;           Adjust text and pointers as needed to fit.
;       6) When the end-of-program is reached, go back to start
;           of program and replace old line numbers with new
;           ones.
;       7) Reset line links and return to BASIC.
;
_RENUM      jsr INITVAR        ; Set default line numbering.
            jsr LINPARMS       ; Get line parms, if any. 
            jsr COPYPTR        ; Set temporary pointer to pgm.
            .byte BASPTR        
            .byte TMPTR3       ; TMPTR3 will be BASIC scanner.
;
;  Do a dry run of new line numbers to see if they fit the program.
;
RNCHKLP     jsr SKPLNK         ; Get start of next line.
            bne @F             ; Test lines until EOP.
            jmp RNMAIN         ; If OK, go to main loop 
@           jsr NXTLIN         ; Update line number.
            bcc RNCHKLP        ; Loop and check another.
            jmp LINERR         ; Line number too big!

GO_BAS      lda #$FF           ; Start BASIC.
            sta CURRLN+1       ; Disable current line number.
            jmp RDYMSG         
;
; After text has been renumbered, fix the BASIC lines, ptrs and exit.
;
RNNUM       lda LINNUM2        ; Initialize starting line 
            sta LINNUM1        ;  number.
            lda LINNUM2+1       
            sta LINNUM1+1       
            jsr COPYPTR        ; Make a temporary pointer
            .byte BASPTR       ;  into BASIC text.
            .byte TMPTR3        
@           ldy #$03           ; Offset to low byte of
            lda LINNUM1+1      ;  line number.
            sta (TMPTR3),y     ; Replace low byte.
            dey                 
            lda LINNUM1         
            sta (TMPTR3),y     ; Replace high byte.
            jsr NXTLIN         ; Update line number.
            jsr SKPLNK          
            bne @B             ; Loop until end of program.
            jsr RST_PTRS       ; Clean up and go
            jmp GO_BAS         ;  back to BASIC.
;
; Main RENUMBER loop. Line numbering parameters have checked OK.
;
RNMAIN      jsr SETBAS         ; Reset temporary BASIC ptr.
RNILOOP     jsr SKPLNK         ; Skip to next link.
            beq RNNUM          ; If EOP, cleanup and exit.
            ldy #$04           ; Initialize 'in-string' flag.
            sty FLAG1           
RNLNLP      lda (TMPTR3),y     ; Get next token from BASIC.
RNCONT      beq RNILOOP        ; IF EOL, go to line handler.
            cmp #'"'           ; Is this START/END of string?
            bne @F             ; Continue, if not.
            lda FLAG1          ; Toggle 'in_string' flag.
            eor #$FF            
            sta FLAG1           
            bne RNXTTKN        ; Get next token (always). 
@           bit FLAG1          ; In string?
            bmi RNXTTKN        ; Get next token, if so.
            cmp #$8F           ; REM token?
            beq RNILOOP        ; Yes, ignore this line.
            ldx #$06           ; Check for any statement
@           cmp TKNLST-1,x     ;  (token) which uses 
            beq CHKNUM         ;  line numbers and test
            dex                ;  number if found.
            bne @B              
RNXTTKN     iny                ; No tokens, do next line.
            bne RNLNLP         ; Branch always.

SKPLNK      ldy #$00             
            lda (TMPTR3),y     ; Skip to next line link.
            tax                 
            iny                 
            lda (TMPTR3),y      
            stx TMPTR3         ; Save it and get hi byte
            sta TMPTR3+1       ;  of next line number.
            lda (TMPTR3),y     ;  (for End-Of-Program test)
            rts                

CHKNUM      clc                ; Check for line number.
            tya                ; Update pointers into BASIC
            adc TMPTR3         ;  text.
            sta TXTPTR          
            sta TMPTR2          
            ldx TMPTR3+1        
            bcc @F              
            inx                 
@           stx TXTPTR+1        
            stx TMPTR2+1       
            jsr CHRGET         ; Now get next token.
            bcc @F             ; Branch if number.
            cmp #TKNMIN        ; Is this a MINUS token?
            beq RNCHKNM        ; If yes, check for number.
            cmp #TKNTO         ; Is this a TO token?
            bne RNXTTKN        ;  (may follow GO token)
            beq RNCHKNM        ; If yes, check for number.
@           jsr RD_INT         ; Get number from text.
            jsr FNDMTCH        ; Scan for match.
            jsr COPYPTR        ; Update BASIC text pointer.
            .byte TMPTR2        
            .byte TXTPTR        
            ldx #$00            
            ldy #$00            
CPYNUM      lda STKPAG+1,x     ; Copy ASCII line number
            beq @F1            ;  into BASIC text, replacing
            pha                ;  old line number reference.
            jsr CHRGET          
            bcc @F              
            jsr MVUP1          ; Open a gap in BASIC text
@           pla                ;  if needed to fit new
            sta (TXTPTR),y     ;  number in old space.
            inx                
            bne CPYNUM          
@           jsr CHRGET          
@           jsr CHRGOT          
            bcs RNCHKNM         
            jsr MVDN1          ; Close gap in BASIC if
            beq @B             ;  needed.
RNCHKNM     tax                ; See if we there is a number
            sec                ;  we need to check.
            lda TXTPTR          
            sbc TMPTR3          
            tay                 
            txa                
            cmp #','           
            beq CHKNUM          
            cmp #TKNMIN        
            beq CHKNUM          
            cmp #TKNTO         ; Is this a TO token?
            beq CHKNUM         ;  (may follow GO token)
            tax                       
            jmp RNCONT         ; Continue with next line.
;
;   Try to match line number reference to its line. Generate new
;   line number on the fly. If no line matches the reference, use
;   MAXLIN (63999).
;
FNDMTCH     lda LINNUM2        ; Set line start number.
            ldx LINNUM2+1      3 
            sta LINNUM1         
            stx LINNUM1+1       
            jsr COPYPTR        ; Make temporary BASIC
            .byte BASPTR       ;  text pointer.
            .byte TMPTR0        
CMPLIN      ldy #$03            
            lda (TMPTR0),y     ; Compare this line number
            cmp TMPINT+1       ;  to the reference.
            bne BMPLIN         ; If no match, skip to next
            dey                ;  line.
            lda (TMPTR0),y      
            cmp TMPINT          
            bne BMPLIN         
; Line number matches. Convert it to ASCII for insertion in text.
;
LN2ASC      lda LINNUM1+1       
            ldx LINNUM1         
RNINT2A     sta FACCM          ; Convert integer to f.p.
            stx FACCM+1
            ldx #$90           ; Exponent for 2-byte integer.
            sec                
            jsr INT2FP          
            jmp FP2ASC         ; Convert f.p. to ASCII.
;
; Calculate new line number and skip to next line link.
;
BMPLIN      jsr NXTLIN         ; Update line number.
            ldy #$01            
            lda (TMPTR0),y     1 
            bne @F             ; If EOP use maximum
            lda >MAXLIN        ;  line number.
            ldx <MAXLIN         
            bne RNINT2A        ; Branch always.
@           tax                 
            dey                 
            lda (TMPTR0),y      
            stx TMPTR0+1        
            sta TMPTR0          
            jmp CMPLIN         ; Check new line for match.
;
;  Move BASIC text up one byte. Adjust pointers.
;
MVUP1       stx SAVEX          ; Start at top of BASIC text.
            ldx VARPTR          
            ldy VARPTR+1       ; Initialize text pointers.
            stx TMPTR1          
            sty TMPTR1+1        
            inx
            bne @F             
            iny                 
@           sec                ; Check available memory
            txa                ;  for room to expand.
            sbc MEMSZ           
            tya                 
            sbc MEMSZ+1        
            bcc @F              
            jmp PRTERR         ; Error if no room to move. 
@           sty VARPTR+1        
            stx VARPTR          
            ldy #$01            
            ldx #$00            
MVUPLP      lda (TMPTR1,x)     ; Move from top of text
            sta (TMPTR1),y     ;  to current point.
            lda TMPTR1          
            bne @F              
            dec TMPTR1+1        
@           dec TMPTR1          
            sec                ; Loop until we reach
            lda TMPTR1         ;  current insertion point.
            sbc TXTPTR          
            lda TMPTR1+1       
            sbc TXTPTR+1       
            bcs MVUPLP         ; Loop termination control.
TSTLNK      php                
            jsr COPYPTR         
            .byte TMPTR3        
            .byte TMPTR2       
            plp                 
            ldy #$01               
CHKEOP      lda (TMPTR2),y     ; Check link for
            bne @F             ;  End-Of-Program.
            ldx SAVEX           
            dey                ; If EOP, prepare for graceful
            rts                ;  exit.
;
;  Increment or decrement a line link. Put new value in program text
;  and in link pointer.
;
@           tax                  
            dey                  
            lda (TMPTR2),y     ; Get link to x,y
            tay                 
            bcs @F             ; Calling routine uses carry
            iny                ;  to invoke increment or 
            bne SAVLNK         ;  decrement code.
            inx                 
            bne SAVLNK         ; Branch always.
@           bne @F              
            dex                 
@           dey                 
SAVLNK      tya                 
            ldy #$00           ; Save in text.
            sta (TMPTR2),y      
            pha                 
            txa
            iny                
            sta (TMPTR2),y      
            sta TMPTR2+1       ; Save in pointer.
            pla                 
            sta TMPTR2          
            jmp CHKEOP         1
;
;  Move BASIC text down one byte. Adjust pointers.
;
MVDN1       lda VARPTR         
            bne @F              
            dec VARPTR+1        
@           dec VARPTR          
            jsr COPYPTR         
            .byte TXTPTR        
            .byte TMPTR1        
            ldy #$01
            ldx #$00           
MVDNLP      lda (TMPTR1),y     ; Loop from current insertion
            sta (TMPTR1,x)     ;  point to end of BASIC text.
            inc TMPTR1          
            bne @F              
            inc TMPTR1+1        
@           sec                 
            lda TMPTR1          
            sbc VARPTR          
            lda TMPTR1+1        
            sbc VARPTR+1        
            bcc MVDNLP         ; Loop termination control. 
            bcs TSTLNK         ; Branch always.
;
; These are the BASIC tokens for keywords which precede line numbers.
; Used by RENUMBER to correct all references to lines.
;
TKNLST      .byte $9B          ; LIST  token
            .byte $8A          ; RUN     "
            .byte $A7          ; THEN    "
            .byte $89          ; GOTO    "
            .byte $8D          ; GOSUB   "
            .byte $CB          ; GO      "   (followed by TO)

LINERR      jsr INITVAR        ; Line number exceeds limit.
            lda <RNGERR        ; Print "?OUT OF RANGE"
            ldy >RNGERR         
            jsr PUT_STRING      
            jmp ERRMSG          
RNGERR      .strz '?OUT OF RANGE'
;
;   Move pointer from one z.p. location to another.
;   Pointer locations follow the calling routines JSR COPYPTR code.
;
COPYPTR     clc                ; Get calling routine return
            pla                ;  address and save in PTR1.
            sta TMPTR4          
            adc #$02           ; Bump return address to skip
            tax                ;  over two bytes and push
            pla                ;  back on stack.
            sta TMPTR4+1        
            adc #$00            
            pha                 
            txa                
            pha                 
            ldy #$01           ; Get address of ptr1 to x...
            lda (TMPTR4),y      
            tax
            iny                
            lda (TMPTR4),y      
            tay                ; ...and ptr2 to y.
            lda $00,x          ; Now move pointer.
            sta $0000,y         
            lda $01,x           
            sta $0001,y         
            rts                

SETBAS      lda <BASPTR        ; Set TMPTR3 to point to
            sta TMPTR3         ;  start of BASIC pointer.
            ldx >BASPTR        ; This causes first link
            stx TMPTR3+1       ;  skip to point to start
            rts                ;  of BASIC.

INITROM     ldx #$07           ; Initialize Toolkit ROM.
@           lda WEDGE-1,x      ; Gopy 'wedge' to CHRGET
            sta TXTPTR+1,x     ;  following TXTPTR.
            dex                 
            bne @B              
            lda <CPYRT         ; Display PAICs copyright
            ldy >CPYRT         ;  notice.
            jsr PUT_STRING      
            jmp INITVAR        ; Initialize Toolkit vars.
;
;   'wedge' code for insertion in CHRGET routine
;
WEDGE       jmp TK_ENTRY       
            brk                ; Location for MODE byte.
            jmp TK_CONT        
;
;  This is the entry point for the Toolkit 'wedge' code. Normal BASIC
;  calls to the CHRGET routine are intercepted to test for Toolkit
;  command processing. If no Toolkit action is performed, BASIC is
;  continued.
;
TK_ENTRY    pha                ; Toolkit entry from CHRGET wedge.
            stx SAVEX          ; Save 'x' register for BASIC.
            lda TXTPTR+1       ; Test for direct mode:
            cmp >INBUFF        ;  - are we pointing into
            bne @F             ;    the BASIC buffer? ($02XX)
            lda TXTPTR          
            cmp <INBUFF        ;  - start of buffer? ($0200)
            beq SCNBUF         ; Branch if in Direct mode.  
            bne MODCHK         ; Branch always.
@           lda TXTPTR          
            sta TMPERR         ; We're in program space
MODCHK      lda MODE           ; Check mode.
            beq RTNBAS         ; Just continue if mode = OFF.
            cmp #AUTOM         ; Test for AUTO mode.
            bne @F              
            jmp CHKAUTO        ; Try AUTO function.
@           jmp STPTRC         ; Must be STEP or TRACE.
RTNBAS      ldx SAVEX          ; Restore 'x' register.
            pla                ; Get saved character
            cmp #'9'+1         ;  and set up for a
            bcs TK_EX          ;  return to BASIC.
TK_CONT     cmp #' '           ; Continue with BASIC.
            bne @F             ; Skip spaces.
            jmp CHRGET          
@           jmp NUMCHK         ; Clear carry if number.
TK_EX       rts                 

SCNBUF      tsx                ; Start scan of BASIC buffer.
            lda STKPAG+3,x     ; Check previous caller loc.
            cmp #BASPG         ; BASIC direct mode?
            bne MODCHK         ; IF not, check mode byte.
            ldy #$00           ; ELSE scan buffer.
            sty COUNT          
;
;   Scan the BASIC input buffer (in direct mode) and test the character
;   string for a match with one of the Toolkit commands. The count of the
;   found command is used to index into a jump table, (TKADDRHI & TKADDRLO) 
;
FNDCMD      ldx #$FF            
@           inx                ; Scan buffer.
            lda INBUFF,x        
            bmi MODCHK          
            cmp #' '            
            beq @B             ; Skip spaces.
CMDLP       lda CMDLIST,y      ; Get next character from 
            beq MODCHK         ;  list of Toolkit cmds.
            eor INBUFF,x        
            bne @F              
            iny                ; Bump indices on every 
            inx                ;    matched character.
            bpl CMDLP           
@           cmp #$80           ; Accept keyword if mismatch
            beq GOTTKN         ;    is caused by terminator.
@           iny                ; Otherwise skip characters
            lda CMDLIST-1,y    ;    to next keyword. 
            bpl @B              
            inc COUNT          ; Update keyword counter.
            bne FNDCMD         ; Branch always (try next cmd).
GOTTKN      inc TXTPTR         ; Bump TXTPTR past this
            dex                ;  keyword. No need to bump
            bne GOTTKN         ;  high byte (always $02).
            ldx COUNT           
            cpx #$02            
            bmi @F              
            pla                 
            pla                 
@           pla                 
            lda TKADDRHI,x     ; Execute Toolkit command.
            pha                 
            lda TKADDRLO,x      
            pha                 
            rts                
;
; This is the Toolkit keyword (command) list. The last character in each
; string has the high bit set to facilitate keyword counting.
;
CMDLIST     .strx 'RUN'          
            .strx 'AUTO'       
            .strx 'STEP'        
            .strx 'TRACE'      
            .strx 'OFF'
            .strx 'RENUMBER'   
            .strx 'DELETE'      
            .strx 'HELP'        
            .strx 'FIND'        
            .strx 'DUMP'        
            .strx 'APPEND'     
            .byte $00          
;
;   Toolkit command dispatch table. The Toolkit commands are
;   preceded with an underscore to facilitate browsing the
;   program source code.
;
;   high bytes of Toolkit routine entry points
;
TKADDRHI    .byte >(RUN-1)     ; RUN 
            .byte >(_AUTO-1)   ; AUTO
            .byte >(_STEP-1)   ; STEP
            .byte >(_TRACE-1)  ; TRACE
            .byte >(_OFF-1)    ; OFF
            .byte >(_RENUM-1)  ; RENUMBER
            .byte >(_DELETE-1) ; DELETE
            .byte >(_HELP-1)   ; HELP
            .byte >(_FIND-1)   ; FIND
            .byte >(_DUMP-1)   ; DUMP
            .byte >(_APPEND-1) ; APPEND
;
;   low bytes of Toolkit routine entry points
;
TKADDRLO    .byte <(RUN-1)     ; RUN
            .byte <(_AUTO-1)   ; AUTO
            .byte <(_STEP-1)   ; STEP
            .byte <(_TRACE-1)  ; TRACE
            .byte <(_OFF-1)    ; OFF
            .byte <(_RENUM-1)  ; RENUMBER
            .byte <(_DELETE-1) ; DELETE
            .byte <(_HELP-1)   ; HELP
            .byte <(_FIND-1)   ; FIND
            .byte <(_DUMP-1)   ; DUMP
            .byte <(_APPEND-1) ; APPEND
;
;       NOTE: RUN is not really a Toolkit command. However, it is
;       necessary to intercept the BASIC RUN command in order to
;       perform some Toolkit housekeeping.
;
RUN         ldx #$0E           ; Intercept RUN command.
            lda #$FF           ; Fill line TRACE buffer
@           sta LNBUFF-1,x     ;  with invalid numbers.
            dex                 
            bne @B              
            ldx #$00            
            stx TXTPTR          
            pha                 
            jmp RTNBAS         ; Continue with BASIC
APPENDMSG   .byte CR            
            .strz 'APPENDING'  
XTTLKT      jmp GO_BAS          
FNDERR      jmp SYNERR         
;
;       FIND
;
;   Find a BASIC variable or keyword
;
_FIND       jsr CHRGET          
            cmp #$01           ; Trap for tokens and EOL.
            bmi FNDERR          
            cmp #','           ; Trap for missing argument.
            beq FNDERR          
            ldx #$00            
            stx SAFTMP1         
            cmp #'"'            
            beq @F              
            jsr LOOKUP         ; Tokenize keyword, if any.
            jsr CHRGET         
            jmp SAVIDX         
@           dec SAFTMP1         
            jsr CHRGET          
            tax                 
            beq FNDERR         ; NULL (end-of-line)?
            cmp #'"'            
            beq FNDERR         ; Empty string?
SAVIDX      lda TXTPTR         ; Save index into text.
            sta FLAG1           
@           jsr CHRGET         ; Scan to EOL or delimiter. 
            tax                 
            beq SRCHPRM         
            cmp #','            
            bne @B              
            jsr CHRGET          
SRCHPRM     jsr GETRNG         ; Get search range.
            cmp #$00            
            bne FNDERR          
            jsr COPYPTR         
            .byte TMPTR1        
            .byte TMPTR3        
            bcc @F              
FNDLP       jsr SKPLNK          
            beq XTTLKT         ; If EOP, exit to BASIC.
@           sec                ; Check to see if we are
            ldy #$02           ;  beyond the search range.
            lda TMPINT          
            sbc (TMPTR3),y     
            lda TMPINT+1
            iny                
            sbc (TMPTR3),y     
            bcc XTTLKT         ; IF beyond range, return.
            ldy #$04           ; ELSE keep going.
            tya                 
            eor SAFTMP1         
            sta SAVEX          
SCNCHR      lda (TMPTR3),y     ; Get next character.
            beq FNDLP          ; Do next line if EOL.
            cmp #'"'           ; Toggle 'in-string' flag.
            bne @F              
            lda SAVEX           
            eor #$FF            
            sta SAVEX           
            bne BMPTR          ; Flag fixed, do next char.
@           bit SAVEX          
            bmi BMPTR           
            ldx FLAG1           
            sty SAFTMP2         
CHKCHR      lda INBUFF,x       ; Main loop to find variable
            beq FNDIT          ;  in text. 'x' indexes
            cmp #'"'           ;  the desired variable,
            beq FNDIT          ;  'y' indexes BASIC text.
            cmp (TMPTR3),y     
            beq @F              
            cmp #','            
            bne GETYIDX         
            lda SAFTMP1         
            beq FNDIT           
            bne GETYIDX        ; Reset to start of var.
@           inx                ; Bump pointers to next
            iny                ;  character to match.
            bne CHKCHR          
FNDIT       jsr STOPKEY        ; Check for STOP key and
            ldy #$02           ;  then list current line.
            sty TMPERR          
            lda (TMPTR3),y      
            tax                 
            iny                 
            lda (TMPTR3),y      
            jsr PRTINT         ; Print integer in a,x.
            jsr LSTLIN          
            jmp FNDLP           
GETYIDX     ldy SAFTMP2         
BMPTR       iny                 
            bne SCNCHR         ; Check another char.
            jmp SYNERR         ; (Should never get here.)
;
;       APPEND
;
;   Append tape program to end of resident BASIC program. Line numbers
;   can be reconciled by calling RENUMBER after successful completion.
;
_APPEND     jsr CHRGET         
            jsr LOOKUP         ; TOKENIZE
            jsr CHRGET          
            lda #$00            
            sta LDVERF          
            jsr GETPARM         
            lda DEVID           
            bne @F             
BADAPP      jmp SYNERR         ; Exit if bad parameters.
@           cmp #$03               
            bcs BADAPP         
            jsr SET_BUFF       ; Initialize tape buffer ptr.
            jsr WT_PLAY        ; Wait for PLAY switch and
            jsr PRTSRCH        ;  show SEARCHING msg.
            lda FNLEN          ; Get length of filename.
            beq SEARCH         ; IF zero, do search for any. 
            jsr GETHDR         ; ELSE search for this file.
            bne GOT_HDR         
NOTFOUND    jmp PRTFNF         ; Print FILE NOT FOUND msg.
SEARCH      jsr SRCH_HDR        
            beq NOTFOUND       
GOT_HDR     jsr RD_HDR          
            sec                ; Header found. Now calculate
            lda TAPTMP2        ;  size of program to append.
            sbc TAPTMP1         
            sta TMPTR5          
            lda TAPTMP2+1       
            sbc TAPTMP1+1       
            sta TMPTR5+1        
            jsr RSTVARP        ; Reset pointer to variables
            ldx TMPTR3+1        
            ldy TMPTR3          
            bne @F              
            dex                 
@           dey                
            tya                 
            ldy #$01            
            clc
            adc TAPTMP1        
            bcc @F              
            inx                 
@           clc                ; Determine whether new program
            sta (TAPBUFF),y    ;  will fit in memory.
            adc TMPTR5          
            sta TAPTMP2         
            txa
            iny                
            sta (TAPBUFF),y     
            adc TMPTR5+1        
            sta TAPTMP2+1       
            sec                ; Use pointers to compare block
            lda TAPTMP2        ;  starts and ends. 
            iny                 
            sta (TAPBUFF),y     
            sbc MEMSZ           
            lda TAPTMP2+1       
            iny                 
            sta (TAPBUFF),y     
            sbc MEMSZ+1         
            bcc @F              
            jmp PRTERR         ; Bad fit. Complain to user.
@           lda <APPENDMSG     ; Size OK.
            ldy >APPENDMSG      
            jsr PUT_STRING     ; show "APPENDING" message
            jsr RD_HDR         ; Read program in.
            jsr RD_TAPE         
            jsr WT_IO           
            lda STATUS         
            beq @F              
            jmp PRTLOAD         
@           jmp PRTRDY          
MODEX       jmp RTNBAS         
;
;       OFF
;
;   Turn off AUTO, TRACE or STEP modes.
;
_OFF        lda #OFFM          ; Entry to turn OFF mode markers.
            .byte $2C          
;
;       TRACE   Turn on TRACE mode
;
_TRACE      lda #TRACEM        ; Entry to turn on TRACE.
            .byte $2C          
;
;       STEP    Turn on STEP mode
;
_STEP       lda #STEPM         ; Set appropriate mode and...
            sta MODE            
            jmp GO_BAS         ; ...go back to BASIC.

STPTRC      ldx TXTPTR+1       ; Are we in direct mode?
            cpx >INBUFF          
            beq MODEX          ; Yes, turn OFF.
            lda CURRLN+1       ; Check line number.
            cmp >(MAXLIN+1)    ; (max. line # is $F9FF)
            bcc @F             ; Branch if valid line number 
            lda #OFFM          ;   otherwise turn OFF.
            sta MODE           
            beq MODEX          
@           ldx CURRLN         ; Check for new line number.
            cmp LNBUFF+1        
            bne NEWLIN          
            cpx LNBUFF          
BRLNK       beq MODEX          ; No change, do nothing.
NEWLIN      sta LNBUFF+1       ; OK, add to list.
            stx LNBUFF          
            ldx #$0B           
@           lda LNBUFF,x       ; Scroll TRACE line buffer
            sta TRCBUF,x       ;    up one line.
            dex                 
            bpl @B              
            bmi TRDISP         ; Branch always.
TSTMOD      lda MODE            
            cmp #STEPM         ; Are we in STEP mode?
            bne CHKSLO           
@           jsr STOPKEY        ; Yes, check for STOP.
            lda SHIFTKEY       ; STEP delay control uses
            beq @B             ;  SHIFT KEY for trigger.
            bne LNGDLY         ; Use long delay.
CHKSLO      lda SHIFTKEY        
            bne @F             ; The delay control accounts 
LNGDLY      lda #$FF           ;  for the possibility that
@           tay                ;  the user simply holds the
@           tax                ;  SHIFT KEY down.
@           inx
            bne @B             ; Also used to slow down
            iny                ;  TRACE mode.
            bne @B1            
            beq BRLNK          ; Exit when delay complete.
TRDISP      ldx #$05           ; Put TRACE buffer on screen.
DSPLP       lda SCROFF,x       ; Screen address in TMPTR1.
            sta TMPTR1          
            lda >SCREEN        
            sta TMPTR1+1        
            txa                
            asl                 
            tay                 
            stx SAVEX           
            ldx TRCBUF,y        
            lda TRCBUF+1,y      
            cmp #$FF           ; Don't display invalid
            beq MKBLNK         ;  line numbers (num > $F9FF)
            jsr RNINT2A        ; Convert to ASCII.
            ldy #$00           
            lda #'#'           
@           jsr HILITE         ; Highlight the pound sign.
            lda STKPAG,y
            bne @B
@           cpy #$06            
            bcs @F              
BLNKIT      lda #' '            
            jsr HILITE         ; Highlight the space.
            bne @B              
@           ldx SAVEX           
            dex
            bpl DSPLP          ; Do next display line.
            bmi TSTMOD         ; Branch always.
MKBLNK      ldy #$00            
            beq BLNKIT          
HILITE      ora #$80           ; Reverse field (highlight).
            tax                 
@           lda VIA            ; Wait for vertical blank
            and #VBLNK         ;  to avoid screen snow.
            bne @B             
            txa                
            sta (TMPTR1),y     ; Put character on screen.
            iny                 
            rts
;
;   Table of screen RAM offsets to upper right TRACE display
;   area. These are coded in reverse order where,
;
;    SCREEN LINE OFFSET
;    ------------------
;       $22 = 34          1ST LINE    (TOP)
;       $4A = 34+40       2ND LINE
;       $72 = 34+80       3RD LINE
;       $9A = 34+120      4TH LINE
;       $C2 = 34+160      5TH LINE
;       $EA = 34+200      6TH LINE    (BOTTOM)
;
;  The decision to provide 6 line numbers was at least
;  partially based on the desire to keep the offset to one
;  byte (OFFSET < 256).
;
SCROFF      .byte $EA,$C2,$9A,$72,$4A,$22 
;
;       DELETE    
;
;   Delete a range of lines in a BASIC program.
;
;
_DELETE     jsr CHRGET         ; Delete range of line numbers
            jsr GETRNG         ; Get parameters.
            beq @F              
            jmp SYNERR          
@           jsr FINDLINE       ; Get BASIC line from number
            bcc DLTLP          ;  in $11,$12.
            ldy #$00            
            lda (TMPTR2),y      
            tax                 
            iny                 
            lda (TMPTR2),y      
            sta TMPTR2+1        
            stx TMPTR2         
DLTLP       ldy #$00           ; Main DELETE loop. Move
            lda (TMPTR2),y     ;  text down to overwrite
            sta (TMPTR1),y     ;  the deleted region.
            inc TMPTR1          
            bne @F              
            inc TMPTR1+1        
@           inc TMPTR2          
            bne @F              
            inc TMPTR2+1        
@           jsr MOVTST          
            bne DLTLP           
            lda TMPTR1         ; Update start of variables
            sta VARPTR         ;  pointer. Variable values
            lda TMPTR1+1       ;  are abandoned.
            sta VARPTR+1        
            ldy #RDY           ; Print READY. message
            jsr PRTMON         ; and return to BASIC.
            jmp FIXLINKS       

MOVTST      ldx VARPTR         ; Move loop termination  
            cpx TMPTR2         ;  test. Have we moved
            bne @F             ;  everybody below the
            ldy VARPTR+1       ;  start of VARS?
            cpy TMPTR2+1        
@           rts                
;
; Get a range of lines. Used by FIND and DELETE to
; parse two hyphen-separated integers.
;
GETRNG      jsr COPYPTR         
            .byte BASPTR        
            .byte TMPTR1        
            jsr RSTVARP        ; Reset pointer to BASIC vars.
            jsr COPYPTR         
            .byte TMPTR3       
            .byte TMPTR2        
            ldx #$FF           ; Initialize line number
            stx TMPINT+1       ;  and get 1st (or only)
            jsr CHRGOT         ;  parameter.
            bcc GETNUM1        
CHKMIN      cmp #'-'           ; Check for hyphen delimiter.
            beq @F              
            cmp #TKNMIN        
            bne GOTRNG              
@           jsr CHRGET         ; Do 2nd parameter.
            bcc GETNUM2         
            bcs CHKMIN          
GETNUM1     jsr RD_INT         ; Get line number and make 
            pha                ;  a pointer to the line.
            jsr FINDLINE        
            jsr COPYPTR         
            .byte TMPTR3       
            .byte TMPTR1
            pla
            beq SETPTR         
            ldx #$FF           
            stx TMPINT+1        
            cmp #'-'           ; Look for 2nd parameter.
            beq @F              
            cmp #TKNMIN        
            bne GOTRNG          
@           jsr CHRGET          
            bcs GOTRNG
GETNUM2     jsr RD_INT         ; Make a pointer to the
            bne GOTRNG         ;  end of the range.
            jsr FINDLINE        
SETPTR      jsr COPYPTR         
            .byte TMPTR3       
            .byte TMPTR2
            sec                ; Check range for validity.
            lda TMPTR2         ; Exit with Z=1 for OK.
            sbc TMPTR1         ; Z=0 otherwise.
            lda TMPTR2+1       
            sbc TMPTR1+1        
            bcc BADPRM            
            lda #$00           ; Force state of carry to
GOTRNG      rts                ;  proper value.
BADPRM      lda #$01            
            rts                
;
;       HELP
;
;  List BASIC line in error and highlight point at which
;  error occurred. (May be off by one character.)
;
_HELP       ldx CURRLN+1       ; Make temporary pointer to
            stx TMPINT+1       ;  error line.
            inx                 
            beq @F              
            lda CURRLN          
            sta TMPINT          
            jsr PRTLIN         ; Print line number.
            ldx #$FF           ; Disable current line with
            stx CURRLN+1       ;  invalid value.
            inx                 
            stx MODE            
            jsr FINDLINE       ; Find BASIC line.
            sec                     
            lda TMPERR          
            sbc TMPTR3
            sta TMPERR         
            dec TMPERR          
            jsr LSTLIN         ; List the line and go to
@           jmp RDYMSG         ;  BASIC READY. msg.

;
; List a BASIC line with highlight at error position.
; Position is location where parser gave up on evaluating
; a formula. It may be one character beyond the actual error.
;
LSTLIN      ldy #$03           ; Use COUNT for 'in-string'
            sty COUNT          ;  flag.
            sty SAVEX          
            lda #' '            
HLSTLP      ldy SAVEX           
            and #POSMSK        ; Scrape off high bit.
HPRTLP      jsr PUTCHR         ; Print character.
            cmp #'"'           ; Are we starting a string?
            bne @F             
            lda COUNT          ; Toggle 'in string' flag.
            eor #$FF            
            sta COUNT          
@           lda #$00            
            sta REVFLG          
            iny
            cpy TMPERR         ; Reverse video at location
            bne @F             ;  of BASIC error.
            sty REVFLG          
@           lda (TMPTR3),y      
            bne @F              
            jmp PUT_CRLF       ; Print CR/LF and exit.
@           bpl HPRTLP          
            cmp #$FF            
            beq HPRTLP          
            bit COUNT           
            bmi HPRTLP         ; Continue through string.
            sec                 
            sbc #POSMSK        ; Convert to count value.
            tax                 
            sty SAVEX           
            ldy #$FF            
KWDLP       dex                ; Find BASIC keyword.
            beq @F1            ; Exit when count complete.
@           iny                
            lda KWDLST,y        
            bpl @B             ; Skip over leading chars.
            bmi KWDLP          ; Branch always.
@           iny                ; Print BASIC keyword.
            lda KWDLST,y        
            bmi HLSTLP         ; Loop back at end of kywd.
            jsr PUTCHR          
            bne @B             
;
;       AUTO
;
; AUTO is an automatic line numbering feature which
; relieves the the programmer from having to enter line
; numbers for each BASIC program line. It accepts line
; numbering parameters with the same syntax as RENUMBER.
;
; Once AUTO mode has been entered, the screen will
; generate and display consecutive line numbers with the
; cursor properly placed for typing in BASIC text.
;
; The strategy for implementing AUTO makes use of the
; PET interrupt driven keyboard handler. Line numbers
; are generated in ASCII format and copied into the PET
; keyboard buffer with the character count set. The
; operating system then handles the task of copying
; the keyboard buffer to its destination.
;
_AUTO       jsr LINPARMS       ; Get line number parms. 
            jsr STUFFNUM       ; Output starting line number.
            lda #AUTOM         ; Set mode marker for AUTO.
            sta MODE            
            lda #$00           ; Trick BASIC into thinking
            rts                ;  nothing happened.

CHKAUTO     pla                
            pha                 
            bne @F              
            lda #OFFM          ; End of line.
            sta MODE           ; Clear mode markers.
            sta NUMCHR
CNTBAS      jmp RTNBAS         
@           cmp #' '           ; Weed out non-numeric
            beq CNTBAS         ;  characters.
            cmp #'9'+1          
            bcs @F              
            cmp #'0'            
            bcs CNTBAS          
@           sec                ; Check number for size
            lda TMPINT         ;  and accept of OK.
            tay                 
            sbc LINNUM1         
            lda TMPINT+1        
            tax                
            sbc LINNUM1+1       
            bcc @F             
            sty LINNUM1         
            stx LINNUM1+1       
            jsr NXTLIN         ; Update line number.
            bcc @F             ; Line number too big!
            jmp LINERR         ; Print error msg and exit.

@           jsr STUFFNUM        
            bpl CNTBAS         ; Branch always.
STUFFNUM    jsr LN2ASC         ; Convert number to ASCII.
            ldy #$00           ; Stuff ASCII line number 
@           iny                ;  into keyboard buffer.
            lda STKPAG-1,y      
            sta KEYBUFF-1,y     
            bne @B              
            lda #' '           ; Add a space...
            sta KEYBUFF-1,y     
            sty NUMCHR         ; ...update number of chars
            rts                ;  and let system print it.
;
;      DUMP
;
; Scan through BASIC variables printing names and values
; to screen. Will print floating point, integer and string
; variables. Does not handle arrays.
;
_DUMP       jsr COPYPTR        ; Make a temporary pointer
            .byte VARPTR       ;  to BASIC variables.
            .byte TMPTR3        
VARLOOP     sec                ; Process variables until
            lda TMPTR3         ;  pointer moves up into 
            sbc ARRPTR         ;  array storage
            lda TMPTR3+1        
            sbc ARRPTR+1        
            bcc @F             
            jmp GO_BAS         ; Done! Return to BASIC
@           ldy #$00            
            sty SAVEX
            iny                
@           lda (TMPTR3),y     ; Scrape off high bits for
            asl                ;  variable type flag.
            rol SAVEX           
            lsr                 
            sta VARNAM,y        
            dey                 
            bpl @B             ; Now check variable type. 
            lda SAVEX           
            beq FPVAR          ; Branch if f.p. type.
            cmp #$01            
            beq NOTVAR         ; Unknown type, ignore.
            cmp #$02            
            beq STRVAR         ; Branch if string variable.
;
;  Print an integer variable to the screen.
;
            jsr PRTNAM         ; Print integer name.
            lda #'%'                 
            jsr PUTCHR          
            lda #'='           ; Format display for 
            jsr PUTCHR         ;  integer.
            ldy #$02            
            lda (TMPTR3),y     ; Adjust pointer to point
            pha                ;  to integer value.
            iny                
            lda (TMPTR3),y      
            tay                
            pla                
            jsr FX2FLT         ; Convert to float
            jsr FP2ASC         ;  then to ASCII
            jsr PUT_STRING     ;  and print it.
            jmp BMPVAR         ; Do next variable.
;
;  Print a floating point variable to screen.
;
FPVAR       jsr PRTNAM         
            lda #'='           
            jsr PUTCHR          
            jsr LOCVAR         ; Get address of f.p. value.
            lda VARADR          
            ldy VARADR+1        
            jsr LDFACC         ; Move value to FACC1.
            jsr CNV_PRT        ; Convert to ASCII and print.
            jmp BMPVAR         ; Do CR, then next var.
FMTSTR      .byte '"'          
            .byte '='           
            .byte '$'
;
;  Print a string variable to the screen.
;
STRVAR      jsr PRTNAM         ; Print variable name. 
            ldx #$02            
@           lda FMTSTR,x       ; Set display for string var.
            jsr PUTCHR          
            dex                 
            bpl @B             
            ldy #$04           ; Move string pointer to
            lda (TMPTR3),y     ;  STRPTR for print routine.
            sta STRPTR+1       
            dey                 
            lda (TMPTR3),y      
            sta STRPTR          
            dey                 
            lda (TMPTR3),y     ; Get number of chars
            jsr PRTSTR         ;  and print string. 
            lda #'"'            
            jsr PUTCHR         ; Put closing quote
BMPVAR      jsr PUT_CRLF       ;  and CR/LF.
NOTVAR      jsr STOPKEY         
@           lda SHIFTKEY       ; Wait for SHIFT key.
            bne @B              
            clc                ; Skip over variable
            lda TMPTR3         ;  descriptor.
            adc #$07           ; BASIC variable descriptors
            sta TMPTR3         ;  occupy seven bytes.
            ldx TMPTR3+1       
            bcc @F              
            inx                 
@           stx TMPTR3+1        
            jmp VARLOOP        ; Do next variable.

PRTNAM      lda VARNAM         ; Print name of current var.
            jsr PUTCHR         
            lda VARNAM+1        
            beq @F              
            jsr PUTCHR          
@           rts                
;
; Skip through BASIC line links to locate end of program.
; Then reset pointer to start of BASIC variables.
;
RSTVARP     jsr SETBAS         ; Set to start of BASIC ptr.
@           jsr SKPLNK         ; Follow links to end of pgm.
            bne @B              
            clc                ; Now correct pointer to
            txa                ;  BASIC variables.
            ldx TMPTR3+1        
            adc #$02            
            sta VARPTR          
            bcc @F              
            inx                 
@           stx VARPTR+1        
            rts                 
;
;   Palo Alto ICs copyright message
;
CPYRT       .text '(C) 1979 PAICS'  ; Palo Alto ICs copyright
            .byte $0D               ;  notice.
            .byte $00                
;
;   (residual garbage in original ROM)
;
            .text 'A'       
            .text 'I'        
            .text 'C'        
            .text 'S'        
            .byte $0D        
            .byte $00        
            .byte $CB        
            .byte $00       
            .byte $00        
            .byte $00        
            .byte $00        
            .byte $00       
            .byte $00        
            .byte $00        
            .byte $00        
            .byte $00        
            .byte $00        
            .byte $00        
            .end
