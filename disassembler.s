;-------------------------------------------------------------------------
; Disassembler by Morten Amundsen (mor10am@gmail.com)
;-------------------------------------------------------------------------

        section        Program,code

;-- Macros ---------------------------------------------------------------

GETWORD:        MACRO
                moveq        #0,d0
                move.w        (a6)+,d0
                ENDM

;-------------------------------------------------------------------------

GETLONG:        MACRO
                move.l        (a6)+,d0
                ENDM

;-------------------------------------------------------------------------

FILL:                MACRO
                lea        \1,a0
LOOP\@1:        move.b        (a0)+,(a5)+
                bne.s        LOOP\@1
                lea        -1(a5),a5
                ENDM

;-------------------------------------------------------------------------

CHAR:                MACRO
                move.b        #\1,(a5)+
                ENDM

;-------------------------------------------------------------------------

SOURCE:                MACRO
                move.w        CODE,d0
                move.w        d0,d1
                and.w        #%0000000000111000,d0        ; Mode
                lsr.w        #3,d0
                and.w        #%0000000000000111,d1        ; Register
                bsr.l        MODEREGISTER
                ENDM

;-------------------------------------------------------------------------

DESTINATION:        MACRO
                move.w        CODE,d0
                move.w        d0,d1
                and.w        #%0000000111000000,d0        ; Mode
                lsr.w        #6,d0
                and.w        #%0000111000000000,d1        ; Register
                rol.w        #7,d1
                bsr.l        MODEREGISTER
                ENDM

;-------------------------------------------------------------------------

DATAREG:        MACRO
                lea        DATAS,a0
                lsl.w        #1,d1
                lea        (a0,d1.w),a0
                move.b        (a0)+,(a5)+
                move.b        (a0),(a5)+
                ENDM

;-------------------------------------------------------------------------

ADDRESSREG:        MACRO
                lea        ADDS,a0
                lsl.w        #1,d1
                lea        (a0,d1.w),a0
                move.b        (a0)+,(a5)+
                move.b        (a0),(a5)+
                ENDM

;-- Library offsets ------------------------------------------------------

;-------------------------------------------------------------------------

NIL:                equ        $00
TAB:                equ        $09
LF:                equ        $0a
SPC:                equ        $20

;-------------------------------------------------------------------------

S:
        movem.l        d0-d7/a0-a6,-(a7)

        move.l        POINTER,a6
        bclr        #0,a6

        clr.w        COUNTER
MAINLOOP:
        bsr.s        CLRLINE
        lea        LINE,a5

        move.l        a6,d0
        bsr.l        LONG2TEXT
        CHAR        " "
        CHAR        ":"
        CHAR        TAB

        GETWORD
        
        move.w        d0,CODE
        and.w        #$f000,d0
        rol.w        #6,d0

        lea        GROUPS,a0
        lea        (a0,d0.w),a0
        move.l        (a0),a0
        jsr        (a0)

        lea        LINE,a0
        move.l        BUFPTR,a1
BUFLOOP:
        move.b        (a0)+,(a1)+
        bne.s        BUFLOOP

        lea        -1(a1),a1
        move.l        a1,BUFPTR        

        addq.w        #1,COUNTER
        cmp.w        #256,COUNTER
        bne.s        MAINLOOP

EXIT:
        movem.l        (a7)+,d0-d7/a0-a6
        move.l        BUFPTR,a0
        moveq        #0,d0
        rts

COUNTER:        dc.w        0

;-------------------------------------------------------------------------

CLRLINE:
        lea        LINE,a0
        move.w        #79,d7
CLRLOOP:
        clr.b        (a0)+
        dbf        d7,CLRLOOP
        rts

;-------------------------------------------------------------------------

NONE:
        cmp.b        #TAB,-(a5)
        bne.s        NONE

        lea        1(a5),a5

        lea        (a5),a0
        move.w        #39,d7
SAFETY:        clr.b        (a0)+
        dbf        d7,SAFETY

        FILL        DECLARE
        CHAR        TAB
        
        move.w        CODE,d0
        bsr.w        WORD2TEXT

        CHAR        LF
        rts

;=========================================================================

BMMI:
        move.w        CODE,d0

        and.w        #%0000111100000000,d0                ; ori
        beq.w        ORII
        cmp.w        #%0000100000000000,d0
        beq.w        BITSS
        cmp.w        #%0000001000000000,d0
        beq.w        ANDII
        cmp.w        #%0000010000000000,d0
        beq.w        SUBII
        cmp.w        #%0000011000000000,d0
        beq.w        ADDII
        cmp.w        #%0000101000000000,d0
        beq.w        EORII
        cmp.w        #%0000110000000000,d0
        beq.w        CMPII
        cmp.w        #%0000111000000000,d0
        beq.w        MOVESI

        btst        #8,d0
        bne.s        BITSD

        bra.w        NONE

;-------------------------------------------------------------------------

BITSD:
        move.w        CODE,d0
        and.w        #%0000000000111000,d0
        cmp.w        #%0000000000001000,d0
        beq.w        MOVEPI

        move.w        CODE,d0
        and.w        #%0000000011000000,d0
        lsr.w        #4,d0

        lea        BTST,a0
        lea        (a0,d0.w),a0

        move.b        (a0)+,(a5)+
        move.b        (a0)+,(a5)+
        move.b        (a0)+,(a5)+
        move.b        (a0),(a5)+

        CHAR        TAB

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG

        CHAR        ","

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts        

;-------------------------------------------------------------------------

BITSS:
        move.w        CODE,d0
        and.w        #%0000000011000000,d0
        lsr.w        #4,d0

        lea        BTST,a0
        lea        (a0,d0.w),a0

        move.b        (a0)+,(a5)+
        move.b        (a0)+,(a5)+
        move.b        (a0)+,(a5)+
        move.b        (a0),(a5)+

        CHAR        TAB

        CHAR        "#"

        GETWORD
        bsr.l        WORD2TEXT

        CHAR        ","

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts


;-------------------------------------------------------------------------

MOVEPI:
        FILL        MOVEP

        move.w        CODE,d0
        bsr.w        GETSIZE

        CHAR        TAB

        move.w        CODE,d0
        btst        #7,d0
        beq.s        MPM2R

MPR2M:
        move.w        d0,d1
        and.w        #%0000111000000000,d1
        rol.w        #7,d1
        DATAREG

        CHAR        ","

        GETWORD
        bsr.w        WORD2TEXT

        CHAR        "("

        move.w        CODE,d0
        move.w        d0,d1
        and.w        #%0000000000000111,d1
        ADDRESSREG

        CHAR        ")"
        CHAR        LF
        rts

MPM2R:
        GETWORD
        bsr.w        WORD2TEXT

        CHAR        "("

        move.w        CODE,d0
        move.w        d0,d1
        and.w        #%0000000000000111,d1
        ADDRESSREG

        CHAR        ")"

        CHAR        ","

        move.w        d0,d1
        and.w        #%0000111000000000,d1
        rol.w        #7,d1
        DATAREG

        CHAR        LF
        rts

;-------------------------------------------------------------------------

ORII:                                                ; ORI
        FILL        OR
        bra.s        IMMCAT0

ANDII:
        FILL        AND
        bra.s        IMMCAT0

SUBII:
        FILL        SUB
        bra.s        IMMCAT0

ADDII:
        FILL        ADD
        bra.s        IMMCAT0

EORII:
        FILL        EOR
        bra.s        IMMCAT0

CMPII:
        FILL        CMP
        bra.s        IMMCAT0

MOVESI:
        FILL        MOVES
        bra.w        IMMCAT0

;-----------------------------------------------------------------------

IMMCAT0:
        move.w        CODE,d0
        move.w        d0,d1
        and.w        #%0000000011000000,d1
        cmp.w        #%0000000011000000,d1                ; Hmmm...
        beq.w        NONE

        bsr.w        GETSIZE

        CHAR        TAB
        
        lsr.w        #1,d1

        CHAR        "#"
        bsr.s        IMMVALUE

        CHAR        ","

        moveq        #0,d0
        move.w        CODE,d0

        SOURCE

        CHAR        LF
        rts

;-------------------------------------------------------------------------
        
IMMVALUE:
        cmp.w        #2,d1
        beq.s        VALLONG

VALWORD:
        GETWORD
        bsr.l        WORD2TEXT
        rts

VALLONG:
        GETLONG
        bsr.l        LONG2TEXT
        rts

;=========================================================================

MOVEI:                                        ; MOVE.B/MOVE.W/MOVE.L
        FILL        MOVE

        lsr.w        #2,d0

        cmp.w        #1,d0
        beq.s        MOVEB
        cmp.w        #2,d0
        beq.s        MOVEL

MOVEW:
        FILL        WORD
        clr.w        SIZE
        bra.s        MOVEMODES

MOVEB:
        FILL        BYTE
        clr.w        SIZE
        bra.s        MOVEMODES

MOVEL:
        FILL        LONG
        move.w        #1,SIZE
        
MOVEMODES:
        CHAR        TAB

        clr.w        ISC
        SOURCE

        CHAR        ","

        move.w        #1,ISC
        DESTINATION

        CHAR        LF
        rts

;=========================================================================

BCCI:                                                ; Bcc
        CHAR        "b"

        move.w        CODE,d0
        and.w        #%0000111100000000,d0
        lsr.w        #7,d0

        lea        BRANCHES,a0
        lea        (a0,d0.w),a0

        move.b        (a0)+,(a5)+
        move.b        (a0),(a5)+

        CHAR        "."

        move.w        CODE,d0
        and.w        #$00ff,d0
        beq.s        DIS16

DIS8:
        CHAR        "s"
        CHAR        TAB
        
        ext.w        d0
        ext.l        d0
        add.l        a6,d0

        bsr.w        LONG2TEXT

        CHAR        LF
        rts

DIS16:
        CHAR        "l"
        CHAR        TAB
        
        GETWORD

        ext.l        d0
        add.l        a6,d0
        subq.l        #$2,d0

        bsr.w        LONG2TEXT        

        CHAR        LF
        rts

;=========================================================================

MOVEQI:                                                ; MOVEQ
        FILL        MOVEQ

        CHAR        TAB

        CHAR        "#"

        move.w        CODE,d0
        and.w        #$00ff,d0
        bsr.w        BYTE2TEXT

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        lsr.w        #8,d0
        move.w        d0,d1

        DATAREG

        CHAR        LF
        rts

;=========================================================================

SHIFTI:                                                ; ROR/ROL/LSL/LSR/etc
        move.w        CODE,d0                                ; (All shifting instr.)
        and.w        #%0000000011000000,d0
        cmp.w        #%0000000011000000,d0                ; Memory-form or
        beq.w        MEMFORM                                ; Register-form shifting...

REGFORM:
        move.w        CODE,d0
        move.w        d0,d1
        and.w        #%0000000000011000,d0
        and.w        #%0000000100000000,d1
        lsr.w        #6,d1
        or.w        d1,d0

        lea        SHIFTS,a0
        move.l        (a0,d0.w),a0

        FILL        (a0)

        move.w        CODE,d0
        bsr.w        GETSIZE

        CHAR        TAB

        btst        #5,d0
        beq.s        IDATA

REGNUM:
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG
        
        bra.s        SHFTD                

IDATA:
        CHAR        "#"

        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        bsr.w        WORD2TEXT        

SHFTD:
        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        DATAREG

        CHAR        LF
        rts

;-------------------------------------------------------------------------

MEMFORM:
        move.w        CODE,d0
        and.w        #%0000011100000000,d0
        lsr.w        #6,d0

        lea        SHIFTS,a0
        move.l        (a0,d0.w),a0

        FILL        (a0)
        CHAR        "."
        CHAR        "w"

        CHAR        TAB

        SOURCE

        CHAR        LF        
        rts

;=========================================================================

LINEAI:                                                ; LINE_A
        FILL        LINEA
        bra.s        LINECOMP

LINEFI:                                                ; LINE_F
        FILL        LINEF

LINECOMP:
        CHAR        TAB
        CHAR        "#"

        move.w        CODE,d0
        and.w        #$0fff,d0

        bsr.w        WORD2TEXT

        CHAR        LF
        rts

;=========================================================================

ADDINS:
        move.w        CODE,d0
        and.w        #%0000000011000000,d0
        cmp.w        #%0000000011000000,d0
        beq.w        ADDAI

        move.w        CODE,d0
        and.w        #%0000000100110000,d0
        cmp.w        #%0000000100000000,d0
        beq.w        ADDXI

ADDING:                                                        ; ADD
        FILL        ADD

        move.w        CODE,d0
        bsr.w        GETSIZE        

        CHAR        TAB

        btst        #8,d0
        beq.s        ADDMEM2D

ADDD2MEM:
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG

        CHAR        ","

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts

ADDMEM2D:
        move.w        CODE,d0
        SOURCE

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG

        CHAR        LF
        rts

;--------------------------------------------------------------------------

ADDXI:                                                ; ADDX
        FILL        ADDX

        move.w        CODE,d0
        bsr.w        GETSIZE

        CHAR        TAB

        btst        #3,d0
        beq.s        DATAXA

ADDRX:
        CHAR        "-"
        CHAR        "("
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        ADDRESSREG
        CHAR        ")"
        
        CHAR        ","

        CHAR        "-"
        CHAR        "("
        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        ADDRESSREG
        CHAR        ")"

        CHAR        LF        
        rts                

;------------------------------------------------------------------------

DATAXA:
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        DATAREG

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG

        CHAR        LF        
        rts

;--------------------------------------------------------------------------

ADDAI:                                                        ; ADDA
        FILL        ADD

        CHAR        "."

        move.w        CODE,d0
        btst        #8,d0
        beq.s        ADDAW

ADDAL:
        CHAR        "l"
        bra.s        ADDAC

ADDAW:
        CHAR        "w"

ADDAC:
        CHAR        TAB

        SOURCE

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        ADDRESSREG

        CHAR        LF        
        rts

;=========================================================================

SUBINS:
        move.w        CODE,d0
        and.w        #%0000000011000000,d0
        cmp.w        #%0000000011000000,d0
        beq.w        SUBAI

        move.w        CODE,d0
        and.w        #%0000000100110000,d0
        cmp.w        #%0000000100000000,d0
        beq.w        SUBXI

SUBING:                                                        ; SUB
        FILL        SUB

        move.w        CODE,d0
        bsr.w        GETSIZE        

        CHAR        TAB

        btst        #8,d0
        beq.s        SUBMEM2D

SUBD2MEM:
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG

        CHAR        ","

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts

SUBMEM2D:
        move.w        CODE,d0
        SOURCE

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG

        CHAR        LF
        rts

;--------------------------------------------------------------------------

SUBXI:                                                        ; SUBX
        FILL        SUBX

        move.w        CODE,d0
        bsr.w        GETSIZE

        CHAR        TAB

        btst        #3,d0
        beq.s        DATAXS

SUBAX:
        CHAR        "-"
        CHAR        "("
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        ADDRESSREG
        CHAR        ")"
        
        CHAR        ","

        CHAR        "-"
        CHAR        "("
        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        ADDRESSREG
        CHAR        ")"

        CHAR        LF        
        rts                

;------------------------------------------------------------------------

DATAXS:
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        DATAREG

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG

        CHAR        LF        
        rts

;--------------------------------------------------------------------------

SUBAI:                                                        ; SUBA
        FILL        SUB

        CHAR        "."

        move.w        CODE,d0
        btst        #8,d0
        beq.s        SUBAW

SUBAL:
        CHAR        "l"
        bra.s        SUBAC

SUBAW:
        CHAR        "w"

SUBAC:
        CHAR        TAB

        SOURCE

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        ADDRESSREG

        CHAR        LF        
        rts

;=========================================================================

ASSDB:                                                ; ADDQ/SUBQ/Scc/DBcc

        move.w        CODE,d0
        and.w        #%0000000011000000,d0
        cmp.w        #%0000000011000000,d0
        beq.s        SDBRANCH

        move.w        CODE,d0
        btst        #8,d0
        beq.s        ADDQI

SUBQI:                                                ; SUBQ
        FILL        SUBQ
        bra.s        ADDSUBQ
        
ADDQI:
        FILL        ADDQ                                ; ADDQ

ADDSUBQ:
        bsr.w        GETSIZE

        CHAR        TAB

        CHAR        "#"

        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        bsr.w        BYTE2TEXT

        CHAR        ","

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts

SDBRANCH:
        move.w        CODE,d0
        and.w        #%0000000000111000,d0
        cmp.w        #%0000000000001000,d0
        beq.s        DBCCI

SCCI:                                                        ; Scc
        CHAR        "s"
        bra.s        CBRA
        
DBCCI:                                                        ; DBcc
        CHAR        "d"
        CHAR        "b"

CBRA:
        move.w        CODE,d0
        and.w        #%0000111100000000,d0
        cmp.w        #%0000000100000000,d0
        bgt.s        JUMPB

        tst.w        d0
        bne.s        BRAF

        CHAR        "t"
        bra.s        DONEB
                
BRAF:
        CHAR        "f"
        bra.s        DONEB
        
JUMPB:
        lsr.w        #7,d0
        lea        BRANCHES,a0
        lea        (a0,d0.w),a0

        move.b        (a0)+,(a5)+
        move.b        (a0),(a5)+

DONEB:
        CHAR        TAB

        move.w        CODE,d0
        and.w        #%0000000000111000,d0
        cmp.w        #%0000000000001000,d0
        beq.s        DBCCI2

SCCI2:
        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts
        
DBCCI2:
        move.w        CODE,d0
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        DATAREG

        CHAR        ","

        GETWORD
        add.l        a6,d0
        subq.l        #$2,d0
        bsr.w        LONG2TEXT
        
        CHAR        LF
        rts

;=========================================================================

CMPEOR:
        move.w        CODE,d0
        and.w        #%0000000011000000,d0
        cmp.w        #%0000000011000000,d0
        beq.w        CMPAI

        move.w        CODE,d0
        btst        #8,d0
        beq.w        CMPINS

        and.w        #%0000000000111000,d0
        cmp.w        #%0000000000001000,d0
        beq.s        CMPMI

EORINS:                                                        ; EOR
        FILL        EOR

        move.w        CODE,d0
        bsr.w        GETSIZE

        CHAR        TAB

        SOURCE

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG

        CHAR        LF
        rts

;----------------------------------------------------------------------

CMPMI:                                                        ; CMPM
        FILL        CMPM

        move.w        CODE,d0
        bsr.w        GETSIZE

        CHAR        TAB

        CHAR        "("

        and.w        #%0000000000000111,d0
        move.w        d0,d1
        ADDRESSREG

        CHAR        ")"
        CHAR        "+"

        CHAR        ","

        CHAR        "("

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        ADDRESSREG

        CHAR        ")"
        CHAR        "+"

        CHAR        LF        
        rts

;-------------------------------------------------------------------------

CMPAI:                                                        ; CMPA
        FILL        CMP

        move.w        CODE,d0

        CHAR        "."
        CHAR        "w"

        clr.w        SIZE

        btst        #8,d0
        beq.s        CMPAOK

        move.w        #1,SIZE

        lea        -1(a5),a5
        CHAR        "l"

CMPAOK:        
        CHAR        TAB

        clr.w        ISC
        SOURCE

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        ADDRESSREG        

        CHAR        LF
        rts

;-------------------------------------------------------------------------

CMPINS:                                                        ; CMP
        FILL        CMP

        move.w        CODE,d0
        bsr.w        GETSIZE
                
        CHAR        TAB

        SOURCE

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG                

        CHAR        LF
        rts        


;=========================================================================

ORDIS:
        move.w        CODE,d0
        and.w        #%0000000011000000,d0
        cmp.w        #%0000000011000000,d0
        beq.w        DIVI

        move.w        CODE,d0
        and.w        #%0000000111110000,d0
        cmp.w        #%0000000100000000,d0
        beq.w        SBCDI

ORI:                                                        ; OR
        FILL        OR

        move.w        CODE,d0
        bsr.w        GETSIZE

        CHAR        TAB

        move.w        CODE,d0
        btst        #8,d0
        beq.s        ORMEM2D

ORD2MEM:
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG

        CHAR        ","

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts

ORMEM2D:
        SOURCE

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG

        CHAR        LF
        rts

;-------------------------------------------------------------------------

SBCDI:                                                        ; SBCD
        FILL        SBCD

        CHAR        TAB

        move.w        CODE,d0
        btst        #3,d0
        beq.s        SBCDDAT

SBCDADD:
        CHAR        "-"
        CHAR        "("

        and.w        #%0000000000000111,d0
        move.w        d0,d1
        ADDRESSREG

        CHAR        ")"
        CHAR        ","
        CHAR        "-"
        CHAR        "("

        move.w        CODE,d1
        and.w        #%0000111000000000,d1
        rol.w        #7,d1
        ADDRESSREG

        CHAR        ")"
        
        CHAR        LF
        rts

SBCDDAT:
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        DATAREG

        CHAR        ","

        move.w        CODE,d1
        and.w        #%0000111000000000,d1
        rol.w        #7,d1
        DATAREG
        
        CHAR        LF
        rts

;------------------------------------------------------------------------

DIVI:                                                ; DIVS/DIVU
        FILL        DIV

        CHAR        "u"

        move.w        CODE,d0
        btst        #8,d0
        beq.s        OKDIV

        lea        -1(a5),a5
        CHAR        "s"

OKDIV:
        CHAR        TAB

        SOURCE
        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG

        CHAR        LF
        rts


;=========================================================================

AMAE:
        move.w        CODE,d0
        and.w        #%0000000011000000,d0
        cmp.w        #%0000000011000000,d0
        beq.w        MULI

        move.w        CODE,d0
        and.w        #%0000000011110000,d0
        beq.w        ABCDI

        move.w        CODE,d0
        and.w        #%0000000111111000,d0
        cmp.w        #%0000000101000000,d0
        beq.w        EXGDD
        cmp.w        #%0000000101001000,d0
        beq.w        EXGAA
        cmp.w        #%0000000110000000,d0
        beq.w        EXGDA

ANDINS:
        FILL        AND

        move.w        CODE,d0
        bsr.w        GETSIZE

        CHAR        TAB

        btst        #8,d0
        beq.s        ANDMEM2D

ANDD2MEM:
        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG

        CHAR        ","

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts

ANDMEM2D:
        move.w        CODE,d0
        SOURCE

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG

        CHAR        LF
        rts

;--------------------------------------------------------------------------

EXGDD:
        FILL        EXG
        CHAR        TAB

        move.w        CODE,d0
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        DATAREG

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG
        
        CHAR        LF
        rts

;--------------------------------------------------------------------------

EXGAA:
        FILL        EXG
        CHAR        TAB

        move.w        CODE,d0
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        ADDRESSREG

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        ADDRESSREG
        
        CHAR        LF
        rts

;--------------------------------------------------------------------------

EXGDA:
        FILL        EXG
        move.w        CODE,d0
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        DATAREG

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        ADDRESSREG
        
        CHAR        LF
        rts

;--------------------------------------------------------------------------

ABCDI:
        FILL        ABCD

        CHAR        TAB

        move.w        CODE,d0
        btst        #3,d0
        beq.s        ABCDDAT

ABCDADD:
        CHAR        "-"
        CHAR        "("

        and.w        #%0000000000000111,d0
        move.w        d0,d1
        ADDRESSREG

        CHAR        ")"
        CHAR        ","
        CHAR        "-"
        CHAR        "("

        move.w        CODE,d1
        and.w        #%0000111000000000,d1
        rol.w        #7,d1
        ADDRESSREG

        CHAR        ")"
        
        CHAR        LF
        rts

ABCDDAT:
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        DATAREG

        CHAR        ","

        move.w        CODE,d1
        and.w        #%0000111000000000,d1
        rol.w        #7,d1
        DATAREG
        
        CHAR        LF
        rts

;--------------------------------------------------------------------------

MULI:
        FILL        MUL

        move.w        CODE,d0

        CHAR        "u"

        btst        #8,d0
        beq.s        OKMUL

        lea        -1(a5),a5
        CHAR        "s"
        
OKMUL:
        CHAR        TAB

        SOURCE

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG

        CHAR        LF        
        rts

;=========================================================================

MISC:
        move.w        CODE,d0

        cmp.w        #$4e70,d0
        beq.w        RESETI
        cmp.w        #$4e71,d0
        beq.w        NOPI
        cmp.w        #$4e72,d0
        beq.w        STOPI
        cmp.w        #$4e73,d0
        beq.w        RTEI
        cmp.w        #$4e74,d0
        beq.w        RTDI
        cmp.w        #$4e75,d0
        beq.w        RTSI
        cmp.w        #$4e76,d0
        beq.w        TRAPVI
        cmp.w        #$4e77,d0
        beq.w        RTRI
        cmp.w        #$4afc,d0
        beq.w        ILLEGALI

        move.w        CODE,d0
        and.w        #%0000111111111110,d1
        cmp.w        #%0000111001111010,d1
        beq.w        MOVECI

;---

        move.w        d0,d1
        and.w        #%0000111111111000,d1
        
        cmp.w        #%0000100001000000,d1
        beq.w        SWAPI
        cmp.w        #%0000100010000000,d1
        beq.w        EXTWI
        cmp.w        #%0000100011000000,d1
        beq.w        EXTLI
        cmp.w        #%0000111001100000,d1
        beq.w        TOUSPI
        cmp.w        #%0000111001101000,d1
        beq.w        FROMUSPI
                
;---

        move.w        d0,d1
        and.w        #%0000111111110000,d1

        cmp.w        #%0000111001010000,d1
        beq.w        LINKI

;---

        move.w        d0,d1
        and.w        #%0000111111000000,d1

        cmp.w        #%0000111010000000,d1
        beq.w        JSRI

        cmp.w        #%0000111011000000,d1
        beq.w        JMPI

        cmp.w        #%0000111001000000,d1
        beq.w        TRAPI

        cmp.w        #%0000100001000000,d1
        beq.w        PEAI

        cmp.w        #%0000100000000000,d1
        beq.w        NBCDI

        cmp.w        #%0000101011000000,d1
        beq.w        TASI
                
;---

        move.w        d0,d1
        and.w        #%0000111110000000,d1

        cmp.w        #%0000100010000000,d1
        beq.s        MOVMEA

        cmp.w        #%0000110010000000,d1
        beq.w        MOVMREG

;---

        move.w        d0,d1
        and.w        #%0000111100000000,d1

        cmp.w        #%0000101000000000,d1
        beq.w        TSTI

        cmp.w        #%0000000000000000,d1
        beq.w        FROMSRI

        cmp.w        #%0000011000000000,d1
        beq.w        TOSRI

        cmp.w        #%0000001000000000,d1
        beq.w        FROMCCRI

        cmp.w        #%0000010000000000,d1
        beq.w        TOCCRI

        move.w        d0,d1
        and.w        #%0000000111000000,d1

        cmp.w        #%0000000111000000,d1
        beq.w        LEAI

        cmp.w        #%0000000110000000,d1
        beq.w        CHKI        

        bra.l        NONE

;-------------------------------------------------------------------------

MOVMEA:
        FILL        MOVEM
        CHAR        "."

        CHAR        "w"

        move.w        CODE,d0
        btst        #6,d0
        beq.s        OKME        

        lea        -1(a5),a5
        CHAR        "l"
        
OKME:
        CHAR        TAB

        GETWORD
        move.w        d0,SET

        move.w        CODE,d0
        and.w        #%0000000000111000,d0
        cmp.w        #%0000000000100000,d0
        bne.s        OKSET

        bsr.w        REVSET
        
OKSET:
        bsr.w        REGISTERS

        CHAR        ","

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts

;-----------------------------------------------------------------------

MOVMREG:
        FILL        MOVEM
        CHAR        "."

        CHAR        "w"

        move.w        CODE,d0
        btst        #6,d0
        beq.s        OKMR

        lea        -1(a5),a5
        CHAR        "l"
        
OKMR:
        CHAR        TAB

        GETWORD
        move.w        d0,SET
        
        move.w        CODE,d0
        SOURCE

        CHAR        ","

        bsr.s        REGISTERS

        CHAR        LF
        rts


;-------------------------------------------------------------------------

REGISTERS:
        move.w        SET,d0
        lea        DATAS,a0
        
        moveq        #0,d7
REGLOOP:
        btst        d7,d0
        beq.s        NXT

        move.b        (a0),(a5)+
        move.b        1(a0),(a5)+
        CHAR        "/"

NXT:        lea        2(a0),a0
        addq.w        #1,d7
        cmp.w        #$10,d7
        bne.s        REGLOOP

        lea        -1(a5),a5
        rts

;-------------------------------------------------------------------------

REVSET:
        move.w        SET,d0
        moveq        #0,d1

        moveq        #0,d6
        move.w        #15,d7
REVLOOP:
        btst        d7,d0
        beq.s        NEXTREG

        bset        d6,d1

NEXTREG:
        addq.w        #1,d6
        dbf        d7,REVLOOP

        move.w        d1,SET
        rts        

;-------------------------------------------------------------------------

MOVECI:
        FILL        MOVEC
        CHAR        TAB

        move.w        CODE,d0
        btst        #0,d0
        beq.s        CSRC

CDST:
        GETWORD
        bsr.s        REGISTER
        CHAR        ","
        bsr.s        CONTROL
        CHAR        LF
        rts        

;----

CSRC:
        GETWORD
        bsr.s        CONTROL
        CHAR        ","
        bsr.s        REGISTER
        CHAR        LF
        rts

;-----------------

CONTROL:
        move.w        d0,d2
        and.w        #%0000111111111111,d2

        tst.w        d2
        beq.s        SFCI
        cmp.w        #$1,d2
        beq.s        DFCI
        cmp.w        #$800,d2
        beq.s        USPI
        cmp.w        #$801,d2
        beq.s        VBRI

        rts

;--------------

SFCI:
        CHAR        "s"
        CHAR        "f"
        CHAR        "c"
        rts

DFCI:
        CHAR        "d"
        CHAR        "f"
        CHAR        "c"
        rts

USPI:
        CHAR        "u"
        CHAR        "s"
        CHAR        "p"
        rts

VBRI:
        CHAR        "v"
        CHAR        "b"
        CHAR        "r"
        rts

;-----------------

REGISTER:
        move.w        d0,d2
        and.w        #%0111000000000000,d0
        rol.w        #4,d0
        move.w        d0,d1

        btst        #15,d2
        bne.s        AREG

DREG:
        DATAREG
        move.w        d2,d0
        rts
        
AREG:
        ADDRESSREG
        move.w        d2,d0
        rts

;-------------------------------------------------------------------------

TASI:
        FILL        TAS
        CHAR        TAB

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts
        
;-------------------------------------------------------------------------

TSTI:
        FILL        TST

        move.w        CODE,d0
        bsr.w        GETSIZE

        CHAR        TAB

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts

;-------------------------------------------------------------------------

NBCDI:
        FILL        NBCD
        CHAR        TAB

        move.w        CODE,d0
        SOURCE

        CHAR        LF        
        rts

;-------------------------------------------------------------------------

PEAI:
        FILL        PEA
        CHAR        TAB

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts

;-------------------------------------------------------------------------

TOUSPI:
        FILL        MOVE
        CHAR        "."
        CHAR        "l"

        CHAR        TAB

        move.w        CODE,d0
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        ADDRESSREG

        CHAR        ","

        CHAR        "u"
        CHAR        "s"
        CHAR        "p"

        CHAR        LF
        rts

;-------------------------------------------------------------------------

FROMUSPI:
        FILL        MOVE
        CHAR        "."
        CHAR        "l"

        CHAR        TAB

        CHAR        "u"
        CHAR        "s"
        CHAR        "p"

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        ADDRESSREG

        CHAR        LF
        rts

;-------------------------------------------------------------------------

SWAPI:
        FILL        SWAP
        bra.s        ESREG
        
EXTWI:
        FILL        EXTW
        bra.s        ESREG

EXTLI:
        FILL        EXTL

ESREG:
        CHAR        TAB

        move.w        CODE,d0
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        DATAREG

        CHAR        LF
        rts

;-------------------------------------------------------------------------

LEAI:
        FILL        LEA

        CHAR        TAB

        move.w        CODE,d0
        SOURCE

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        ADDRESSREG

        CHAR        LF        
        rts

;-------------------------------------------------------------------------

CHKI:
        FILL        CHK

        CHAR        TAB

        move.w        CODE,d0
        SOURCE

        CHAR        ","

        move.w        CODE,d0
        and.w        #%0000111000000000,d0
        rol.w        #7,d0
        move.w        d0,d1
        DATAREG

        CHAR        LF        
        rts

;-------------------------------------------------------------------------

LINKI:
        move.w        CODE,d0
        btst        #3,d0
        bne.s        UNLKI

        FILL        LINK
        CHAR        TAB

        move.w        CODE,d0
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        ADDRESSREG        

        CHAR        ","

        CHAR        "#"

        GETWORD
        bsr.w        WORD2TEXT

        CHAR        LF
        rts

UNLKI:
        FILL        UNLK

        CHAR        TAB

        move.w        CODE,d0
        and.w        #%0000000000000111,d0
        move.w        d0,d1
        ADDRESSREG        

        CHAR        LF        
        rts
;-------------------------------------------------------------------------

FROMCCRI:
        move.w        d0,d1
        and.w        #%0000000011000000,d1
        cmp.w        #%0000000011000000,d1
        bne.s        CLRI

        FILL        MOVE
        CHAR        "."
        CHAR        "w"

        CHAR        TAB

        CHAR        "c"
        CHAR        "c"
        CHAR        "r"

        CHAR        ","

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts

;-------------------------------------------------------------------------

CLRI:
        FILL        CLR

        move.w        CODE,d0
        bsr.w        GETSIZE

        CHAR        TAB

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts

;-------------------------------------------------------------------------

TOCCRI:
        move.w        d0,d1
        and.w        #%0000000011000000,d1
        cmp.w        #%0000000011000000,d1
        bne.s        NEGINS

        FILL        MOVE
        CHAR        "."
        CHAR        "w"

        CHAR        TAB

        move.w        CODE,d0
        SOURCE

        CHAR        ","

        CHAR        "c"
        CHAR        "c"
        CHAR        "r"

        CHAR        LF
        rts

;---------------------------------------------------------------------------

NEGINS:
        FILL        NEG

        move.w        CODE,d0
        bsr.w        GETSIZE

        CHAR        TAB

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts

;---------------------------------------------------------------------------

FROMSRI:
        move.w        d0,d1
        and.w        #%0000000011000000,d1
        cmp.w        #%0000000011000000,d1
        bne.s        NEGXI

        FILL        MOVE
        CHAR        "."
        CHAR        "w"

        CHAR        TAB

        CHAR        "s"
        CHAR        "r"

        CHAR        ","

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts

;-------------------------------------------------------------------------

NEGXI:
        FILL        NEGX

        move.w        CODE,d0
        bsr.w        GETSIZE

        CHAR        TAB

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts

;-------------------------------------------------------------------------

TOSRI:
        move.w        d0,d1
        and.w        #%0000000011000000,d1
        cmp.w        #%0000000011000000,d1
        bne.s        NOTI

        FILL        MOVE
        CHAR        "."
        CHAR        "w"

        CHAR        TAB

        move.w        CODE,d0
        SOURCE

        CHAR        ","

        CHAR        "s"
        CHAR        "r"

        CHAR        LF
        rts

;-------------------------------------------------------------------------

NOTI:
        FILL        NOT

        move.w        CODE,d0
        bsr.w        GETSIZE

        CHAR        TAB

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts

;-------------------------------------------------------------------------

TRAPI:
        FILL        TRAP
        CHAR        TAB

        CHAR        "#"
        
        move.w        CODE,d0
        and.w        #%0000000000000111,d0
        bsr.w        BYTE2TEXT

        CHAR        LF
        rts

;-------------------------------------------------------------------------

JSRI:
        FILL        JSR
        bra.s        JUMPCOMP

JMPI:
        FILL        JMP

JUMPCOMP:
        CHAR        TAB

        move.w        CODE,d0
        SOURCE

        CHAR        LF
        rts

;-------------------------------------------------------------------------

STOPI:
        FILL        STOP
        CHAR        TAB

        GETWORD
        CHAR        "#"
        bsr.w        WORD2TEXT
        
        CHAR        LF
        rts        

;-------------------------------------------------------------------------

TRAPVI:
        FILL        TRAPV
        CHAR        LF
        rts
        
;-------------------------------------------------------------------------

RTSI:
        FILL        RTS
        CHAR        LF
        rts

;-------------------------------------------------------------------------

RTDI:
        FILL        RTD
        CHAR        TAB

        GETWORD
        CHAR        "#"
        bsr.w        WORD2TEXT
        
        CHAR        LF
        rts

;-------------------------------------------------------------------------

ILLEGALI:
        FILL        ILLEGAL
        CHAR        LF
        rts

;-------------------------------------------------------------------------

RTRI:
        FILL        RTR
        CHAR        LF
        rts

;-------------------------------------------------------------------------

RTEI:
        FILL        RTE
        CHAR        LF
        rts

;-------------------------------------------------------------------------

NOPI:
        FILL        NOP
        CHAR        LF
        rts

;-------------------------------------------------------------------------

RESETI:
        FILL        RESET
        CHAR        LF
        rts

;=========================================================================

GETSIZE:
        move.w        d0,d1
        and.w        #%0000000011000000,d1
        lsr.w        #5,d1

        clr.w        SIZE

        cmp.w        #4,d1
        blt.s        OKSIZE

        move.w        #1,SIZE
OKSIZE:
        lea        SIZES,a1
        lea        (a1,d1.w),a1

        move.b        (a1)+,(a5)+
        move.b        (a1),(a5)+

        rts                

;=========================================================================

MODEREGISTER:                        ; Mode (d0) ... Register (d1)
        lsl.w        #2,d0
        lea        MODES,a0
        lea        (a0,d0.w),a0
        move.l        (a0),a0
        jmp        (a0)                

;-------------------------------------------------------------------------

DATADIRECT:
        DATAREG
        rts

;-------------------------------------------------------------------------

ADDRESSDIRECT:
        ADDRESSREG
        rts

;-------------------------------------------------------------------------

ADDRESSINDIRECT:
        CHAR        "("
        ADDRESSREG
        CHAR        ")"
        rts

;-------------------------------------------------------------------------

ADDRESSINPOST:
        CHAR        "("
        ADDRESSREG
        CHAR        ")"
        CHAR        "+"        
        rts

;-------------------------------------------------------------------------

ADDRESSINPRE:
        CHAR        "-"        
        CHAR        "("
        ADDRESSREG
        CHAR        ")"
        rts

;-------------------------------------------------------------------------

ADDRESSINDIS:
        GETWORD
        move.l        d1,-(a7)
        bsr.w        WORD2TEXT
        move.l        (a7)+,d1

        bsr.s        ADDRESSINDIRECT
        rts

;-------------------------------------------------------------------------

ADDRESSINIDX:
        GETWORD
        movem.l        d0/d1,-(a7)
        bsr.w        BYTE2TEXT
        movem.l        (a7)+,d0/d1

        CHAR        "("

        ADDRESSREG        
        
        CHAR        ","

        move.w        d0,d1
        and.w        #$f000,d1
        and.w        #$0f00,d0

        rol.w        #4,d1
        lsr.w        #8,d0
        
        btst        #3,d1
        beq.s        IDXDATA

        bclr        #3,d1
        ADDRESSREG
        bra.s        SIZEIDX        

IDXDATA:
        DATAREG

SIZEIDX:
        CHAR        "."

        CHAR        "w"

        btst        #3,d0
        beq.s        DOIDX

        lea        -1(a5),a5
        CHAR        "l"

DOIDX:
        CHAR        ")"
        rts

;-------------------------------------------------------------------------

OTHERMODES:
        lsl.w        #2,d1
        lea        SUBMODES,a0
        lea        (a0,d1.w),a0
        move.l        (a0),a0
        jmp        (a0)                

;-------------------------------------------------------------------------

ABSSHORT:
        GETWORD
        bsr.w        WORD2TEXT
        CHAR        "."
        CHAR        "w"
        rts

;-------------------------------------------------------------------------

ABSLONG:
        GETLONG
        bsr.w        LONG2TEXT
        rts

;-------------------------------------------------------------------------

PCDIS:
        GETWORD
        ext.l        d0
        add.l        a6,d0
        subq.l        #$2,d0
        bsr.l        LONG2TEXT

        CHAR        "("
        FILL        PCT
        CHAR        ")"
        rts

;-------------------------------------------------------------------------

PCIDX:
        GETWORD
        move.w        d0,d1

        and.w        #$00ff,d0
        ext.w        d0
        ext.l        d0
        add.l        a6,d0
        subq.l        #$2,d0

        move.l        d1,-(a7)
        bsr.l        LONG2TEXT
        move.l        (a7)+,d1

        CHAR        "("
        FILL        PCT
        CHAR        ","

        moveq        #0,d0
        move.w        d1,d0
        
        and.w        #$f000,d1
        and.w        #$0f00,d0

        rol.w        #4,d1
        lsr.w        #8,d0
        
        btst        #3,d1
        beq.s        PCDATA

        bclr        #3,d1
        ADDRESSREG
        bra.s        SIZEPC

PCDATA:
        DATAREG

SIZEPC:
        CHAR        "."

        CHAR        "w"

        btst        #3,d0
        beq.s        DOPC

        lea        -1(a5),a5
        CHAR        "l"

DOPC:
        CHAR        ")"
        rts

;-------------------------------------------------------------------------

IMM_SR_CCR:
        tst.w        ISC
        beq.s        IMMIDIATE

        cmp.w        #1,ISC
        beq.s        SMSR

SMCCR:
        FILL        CCRT
        rts

SMSR:
        FILL        SRT
        rts

;-------------------------------------------------------------------------

IMMIDIATE:
        CHAR        "#"

        tst.w        SIZE
        bne.s        IMMLONG

IMMWORD:
        GETWORD
        bsr.l        WORD2TEXT
        rts

IMMLONG:
        GETLONG
        bsr.l        LONG2TEXT
        rts

;-------------------------------------------------------------------------

BYTE2TEXT:
        btst        #7,d0
        beq.s        PLUSSB

        CHAR        "-"

        neg.w        d0
        and.w        #$00ff,d0

PLUSSB:
        CHAR        "$"

        moveq        #1,d4
        move.w        #$00f0,d6

        move.w        #1,d7
B2LOOP:
        move.w        d0,d1
        and.w        d6,d1
                        
        moveq        #0,d5
B2SHIFT:
        cmp.w        d5,d7
        beq.s        B2DONE

        lsr.w        #4,d1
        addq.w        #1,d5
        bra.s        B2SHIFT

B2DONE:
        tst.w        d4
        beq.s        B2IGNORE

        tst.b        d1
        beq.s        B2CONT

        moveq        #0,d4        

B2IGNORE:
        bsr.w        INSTEXT

B2CONT:
        lsr.w        #4,d6
        dbf        d7,B2LOOP

        cmp.b        #'$',-1(a5)
        bne.s        OKBT

        move.b        #'0',(a5)+

OKBT:
        rts

;-------------------------------------------------------------------------

WORD2TEXT:
        CHAR        "$"

        moveq        #1,d4
        move.w        #$f000,d6

        move.w        #3,d7
W2LOOP:
        move.w        d0,d1
        and.w        d6,d1
                        
        moveq        #0,d5
W2SHIFT:
        cmp.w        d5,d7
        beq.s        W2DONE

        lsr.w        #4,d1
        addq.w        #1,d5
        bra.s        W2SHIFT

W2DONE:
        tst.w        d4
        beq.s        W2IGNORE

        tst.b        d1
        beq.s        W2CONT

        moveq        #0,d4        

W2IGNORE:
        bsr.s        INSTEXT

W2CONT:
        lsr.w        #4,d6
        dbf        d7,W2LOOP

        cmp.b        #'$',-1(a5)
        bne.s        OKWT

        move.b        #'0',(a5)+
        
OKWT:
        rts

;-------------------------------------------------------------------------

LONG2TEXT:
        CHAR        "$"

        moveq        #1,d4
        move.l        #$f0000000,d6

        move.w        #7,d7
L2LOOP:
        move.l        d0,d1
        and.l        d6,d1
                        
        moveq        #0,d5
L2SHIFT:
        cmp.w        d5,d7
        beq.s        L2DONE

        lsr.l        #4,d1
        addq.w        #1,d5
        bra.s        L2SHIFT

L2DONE:
        tst.w        d4
        beq.s        L2IGNORE

        tst.b        d1
        beq.s        L2CONT

        moveq        #0,d4        

L2IGNORE:
        bsr.s        INSTEXT

L2CONT:
        lsr.l        #4,d6
        dbf        d7,L2LOOP

        cmp.b        #'$',-1(a5)
        bne.s        OKLT

        move.b        #'0',(a5)+

OKLT:
        rts

;-------------------------------------------------------------------------

INSTEXT:
        cmp.b        #$9,d1
        bgt.s        HEX

        add.b        #'0',d1
        move.b        d1,(a5)+
        rts

HEX:
        add.b        #87,d1
        move.b        d1,(a5)+
        rts        

;-------------------------------------------------------------------------

        section        Datafield,data

POINTER:        dc.l        $fc00d2
LINE:                blk.b        80,0

;--

SET:                dc.w        0
CODE:                dc.w        0

GROUPS:
                dc.l        BMMI        ; 0         Bit Manip./MOVEP/Immidiate
                dc.l        MOVEI        ; 1         Move Byte
                dc.l        MOVEI        ; 2         Move Long
                dc.l        MOVEI        ; 3         Move Word
                dc.l        MISC        ; 4         Miscellanious
                dc.l        ASSDB        ; 5         ADDQ/SUBQ/Scc/DBcc
                dc.l        BCCI        ; 6         Branching (Bcc)
                dc.l        MOVEQI        ; 7         MoveQ
                dc.l        ORDIS        ; 8         OR/DIV/SBCD
                dc.l        SUBINS        ; 9         SUB/SUBX
                dc.l        LINEAI        ; a         LINE_A
                dc.l        CMPEOR        ; b         CMP/EOR
                dc.l        AMAE        ; c         AND/MULU/MULS/ABCD/EXG
                dc.l        ADDINS        ; d         ADD/ADDX
                dc.l        SHIFTI        ; e         Shifting
                dc.l        LINEFI        ; f         LINE_F

;------------------------------------------------------------------------

MODES:
                dc.l        DATADIRECT
                dc.l        ADDRESSDIRECT
                dc.l        ADDRESSINDIRECT
                dc.l        ADDRESSINPOST
                dc.l        ADDRESSINPRE
                dc.l        ADDRESSINDIS
                dc.l        ADDRESSINIDX
                dc.l        OTHERMODES

SUBMODES:
                dc.l        ABSSHORT
                dc.l        ABSLONG
                dc.l        PCDIS
                dc.l        PCIDX
                dc.l        IMM_SR_CCR

ISC:                dc.w        0                        ; 0 = Immidiate
                                                ; 1 = SR
                                                ; 2 = CCR

SIZE:                dc.w        0                        ; 0 = Word
                                                ; 1 = Longword

;------------------------------------------------------------------------

PCT:                dc.b        "pc",0
SRT:                dc.b        "sr",0
CCRT:                dc.b        "ccr",0

BYTE:                dc.b        ".b",0
WORD:                dc.b        ".w",0
LONG:                dc.b        ".l",0

SIZES:
                dc.b        ".b"
                dc.b        ".w"
                dc.b        ".l"
                dc.b        ".w"
                
DATAS:
                dc.b        "d0"
                dc.b        "d1"
                dc.b        "d2"
                dc.b        "d3"
                dc.b        "d4"
                dc.b        "d5"
                dc.b        "d6"
                dc.b        "d7"
ADDS:
                dc.b        "a0"
                dc.b        "a1"
                dc.b        "a2"
                dc.b        "a3"
                dc.b        "a4"
                dc.b        "a5"
                dc.b        "a6"
                dc.b        "a7"

;--

BRANCHES:
        dc.b        "ra"
        dc.b        "sr"
        dc.b        "hi"
        dc.b        "ls"
        dc.b        "cc"
        dc.b        "cs"
        dc.b        "ne"
        dc.b        "eq"
        dc.b        "vc"
        dc.b        "vs"
        dc.b        "pl"
        dc.b        "mi"
        dc.b        "ge"
        dc.b        "lt"
        dc.b        "gt"
        dc.b        "le"
        even
                
SHIFTS:
        dc.l        ASR
        dc.l        ASL
        dc.l        LSR
        dc.l        LSL
        dc.l        ROXR
        dc.l        ROXL
        dc.l        ROR
        dc.l        ROL

DECLARE:        dc.b        "dc.w",0
        
ASR:                dc.b        "asr",0
ASL:                dc.b        "asl",0
LSR:                dc.b        "lsr",0
LSL:                dc.b        "lsl",0
ROXR:                dc.b        "roxr",0
ROXL:                dc.b        "roxl",0
ROR:                dc.b        "ror",0
ROL:                dc.b        "rol",0

BTST:                dc.b        "btst"
BCHG:                dc.b        "bchg"
BCLR:                dc.b        "bclr"
BSET:                dc.b        "bset"

LINEA:                dc.b        "line_a",0
LINEF:                dc.b        "line_f",0

ABCD:                dc.b        "abcd",0
ADD:                dc.b        "add",0
ADDQ:                dc.b        "addq",0
ADDX:                dc.b        "addx",0
AND:                dc.b        "and",0
CLR:                dc.b        "clr",0
CMP:                dc.b        "cmp",0
CMPM:                dc.b        "cmpm",0
DIV:                dc.b        "div",0
EOR:                dc.b        "eor",0
EXG:                dc.b        "exg",0
ILLEGAL:        dc.b        "illegal",0
MOVE:                dc.b        "move",0
MOVEC:                dc.b        "movec",0
MOVEQ:                dc.b        "moveq",0
MOVES:                dc.b        "moves",0
MUL:                dc.b        "mul",0
NEG:                dc.b        "neg",0
NEGX:                dc.b        "negx",0
NOP:                dc.b        "nop",0
NOT:                dc.b        "not",0
OR:                dc.b        "or",0
RESET:                dc.b        "reset",0
RTD:                dc.b        "rtd",0
RTE:                dc.b        "rte",0
RTR:                dc.b        "rtr",0
RTS:                dc.b        "rts",0
SBCD:                dc.b        "sbcd",0
SUB:                dc.b        "sub",0
SUBX:                dc.b        "subx",0
SUBQ:                dc.b        "subq",0
TRAP:                dc.b        "trap",0
TRAPV:                dc.b        "trapv",0
JSR:                dc.b        "jsr",0
JMP:                dc.b        "jmp",0
CHK:                dc.b        "chk.w",0
LEA:                dc.b        "lea",0
PEA:                dc.b        "pea",0
STOP:                dc.b        "stop",0
LINK:                dc.b        "link",0
UNLK:                dc.b        "unlk",0
EXTW:                dc.b        "ext.w",0
EXTL:                dc.b        "ext.l",0
SWAP:                dc.b        "swap",0
TAS:                dc.b        "tas",0
TST:                dc.b        "tst",0
NBCD:                dc.b        "nbcd",0
MOVEM:                dc.b        "movem",0
MOVEP:                dc.b        "movep",0
                even
                
BUFPTR:                dc.l        BUFFER
BUFFER:                blk.b        [80*256],0
E: