; ----------------------------------------------------------------------
; CamelForth for the Texas Instruments MSP430 
; (c) 2009 Bradford J. Rodriguez.
; 
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; Commercial inquiries should be directed to the author at 
; 115 First St., #105, Collingwood, Ontario L9Y 4W3 Canada
; or via email to bj@camelforth.com
; ----------------------------------------------------------------------
; deps430.s43: CPU and Model Dependencies - MSP430F1611
; B. Rodriguez  4 Jan 09

;   Forth words are documented as follows:
;x   NAME     stack -- stack    description
;   where x=C for ANS Forth Core words, X for ANS
;   Extensions, Z for internal or private words.
;
; Indirect-Threaded Forth model for T.I. MSP430
;   cell size is   16 bits (2 bytes)
;   char size is    8 bits (1 byte)
;   address unit is 8 bits (1 byte), i.e., addresses are byte-aligned.
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; ALIGNMENT AND PORTABILITY OPERATORS 
; Many of these are synonyms for other words,
; and so are defined as CODE words.

;C ALIGN    --                         align HERE
;   IHERE 1 AND IALLOT ;
        HEADER(ALIGNN,5,"ALIGN",DOCOLON)
        DW  IHERE,lit,1,ANDD,IALLOT,EXIT

;C ALIGNED  addr -- a-addr       align given addr
;   DUP 1 AND + ;
        HEADER(ALIGNED,7,"ALIGNED",DOCOLON)
        DW  DUP,lit,1,ANDD,PLUS,EXIT

;Z CELL     -- n                 size of one cell
        HEADER(CELL,4,"CELL",DOCON)
        dw 2

;C CELL+    a-addr1 -- a-addr2      add cell size
;   2 + ;
        CODEHEADER(CELLPLUS,5,"CELL+")
        ADD     #2,TOS
        JMP     donext

;C CELLS    n1 -- n2            cells->adrs units
        HEADER(CELLS,5,"CELLS",TWOSTAR+2)

;C CHAR+    c-addr1 -- c-addr2   add char size
        HEADER(CHARPLUS,5,"CHAR+",ONEPLUS+2)

;C CHARS    n1 -- n2            chars->adrs units
        HEADER(CHARS,5,"CHARS",donoop)

;C >BODY    xt -- a-addr      adrs of CREATE data
;   2+ ;                   8086 (3 byte CALL)
        HEADER(TOBODY,5,">BODY",CELLPLUS+2)

;X COMPILE,  xt --         append execution token
; I called this word ,XT before I discovered that it is defined in the 
; ANSI standard as COMPILE,. On a DTC Forth this simply appends xt 
; (like , ) but on an STC Forth this must append 'CALL xt'.
        ; HEADER(COMMAXT,8,"COMPILE,",DOALIAS)
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      8,"COMPILE,"
        .align 16
COMMAXT: DW      DOALIAS        
        DW  ICOMMA

;Z !CF    adrs cfa --   set code action of a word
;   I! ;  
; Indirect threaded model just stores adrs in cfa field.
        HEADER(STORECF,3,"!CF",DOALIAS)
        DW  ISTORE

;Z ,CF    adrs --       append a code field
;   IHERE !CF 2 IALLOT ;  MSP430 VERSION (2 bytes)
        ; HEADER(COMMACF,3,",CF",DOCOLON)
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      3,",CF"
        .align 16
COMMACF: DW      DOCOLON        
        DW IHERE,STORECF,lit,2,IALLOT,EXIT

;Z ,CALL  adrs --       append a subroutine CALL
; MSP430:  128x is call, Ad=11, Dreg=0000 (PC)  thus append 12B0,adrs.
        ; HEADER(COMMACALL,5,",CALL",DOCOLON)
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      5,",CALL"
        .align 16
COMMACALL: DW      DOCOLON        
        DW lit,12B0h,ICOMMA,ICOMMA,EXIT

;Z ,JMP   adrs --       append an absolute 16-bit JMP  (MOV #xx,PC)
; MSP430:  opcode 4, Sreg=0000, Ad=0, As=11 (immed), Dreg=0000 (PC)  
; thus append 4030,adrs.
        ; HEADER(COMMAJMP,4,",JMP",DOCOLON)
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      4,",JMP"
        .align 16
COMMAJMP: DW      DOCOLON        
        DW lit,4030h,ICOMMA,ICOMMA,EXIT

;Z !COLON   --      change code field to DOCOLON
;   -2 IALLOT DOCOLON-adrs ,CF ;
; This should be used immediately after CREATE.
; This is made a distinct word, because on an STC
; Forth, colon definitions have no code field.
        HEADER(STORCOLON,6,"!COLON",DOCOLON)
        DW lit,-2,IALLOT
        DW lit,DOCOLON,COMMACF,EXIT

;Z ,EXIT    --      append hi-level EXIT action
;   ['] EXIT ,XT ;
; This is made a distinct word, because on an STC
; Forth, it appends a RET instruction, not an xt.
        ; HEADER(CEXIT,5,",EXIT",DOCOLON)
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      5,",EXIT"
        .align 16
CEXIT: DW      DOCOLON        
        DW lit,EXIT,COMMAXT,EXIT

; ----------------------------------------------------------------------
; CONTROL STRUCTURES 
; These words allow Forth control structure words
; to be defined portably.

;Z ,BRANCH   xt --    append a branch instruction
; xt is the branch operator to use, e.g. qbranch or (loop).  
; It does NOT append the destination address.  
; On the MSP430 this is equivalent to ,XT (above).
        ; HEADER(COMMABRANCH,7,",BRANCH",DOALIAS)
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      7,",BRANCH"
        .align 16
COMMABRANCH: DW      DOALIAS
        DW  ICOMMA

;Z ,DEST   dest --        append a branch address
;   IHERE - , ;
; This appends the given destination address to the branch instruction. 
; The MSP430 uses relative addressing from the location of the offset cell,
; i.e., to branch to FOO the offset cell at $ contains FOO-$.
        ; HEADER(COMMADEST,5,",DEST",DOCOLON)
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      5,",DEST"
        .align 16
COMMADEST: DW      DOCOLON       
        DW  IHERE,MINUS,ICOMMA,EXIT

;Z !DEST   dest adrs --    change a branch dest'n
;   TUCK - SWAP I! ;
; Changes the destination address found at 'adrs' to the given 'dest'.  
; The MSP430 uses relative addressing from the location of the offset cell,
; i.e., to branch to FOO the offset cell at $ contains FOO-$.
        HEADER(STOREDEST,5,"!DEST",DOCOLON)
        DW  TUCK,MINUS,SWAP,ISTORE,EXIT

;Z ,NONE   --              append a null destination (Flashable)
;   CELL IALLOT ;
; When compiling in Flash ROM a branch to be resolved later, we must
; skip the cell so that it can be programmed at a later time.
; In general Flash memory can only be written once!
; ,NONE should be used wherever !DEST will resolve the branch.
        ; HEADER(COMMANONE,5,",NONE",DOCOLON)
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      5,",NONE"
        .align 16
COMMANONE: DW      DOCOLON        
        DW  CELL,IALLOT,EXIT

; ----------------------------------------------------------------------
; HEADER STRUCTURE 
; The structure of the Forth dictionary headers (name, link, immediate 
; flag, and "smudge" bit) does not necessarily differ across CPUs.  This
; structure is not easily factored into distinct "portable" words; 
; instead, it is implicit in the definitions of FIND and CREATE, and 
; also in NFA>LFA, NFA>CFA, IMMED?, IMMEDIATE, HIDE, and REVEAL.
; These words must be (substantially) rewritten if either the header 
; structure or its inherent assumptions are changed.

