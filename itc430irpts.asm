; ----------------------------------------------------------------------
; CamelForth for the Texas Instruments MSP430 
; (c) 2013,2014 Bradford J. Rodriguez.
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
; itc430irpts.asm - 
; High-Level Interrupt Handlers for MSP430 ITC (indirect-threaded)
; B. Rodriguez 22 dec 2013
; ----------------------------------------------------------------------
; REVISION HISTORY
;  2 mar 2014 bjr - adapted from irpts430.s43 for naken_asm.
; 22 dec 2013 bjr - Created.

; A high-level Forth interrupt handler word takes the form
; cfa: adrs of DOIRPT
; pfa: CALL #HLIRPT
;      ...Forth thread...
;      EXIT
;
; In other words, it looks just like a normal Forth colon
; definition, except that the first two cells are the machine
; instruction CALL #HLIRPT  ($12B0, HLIRPT), and the code field
; contains the address DOIRPT instead of DOCOLON (see below).

; HLIRPT must do the following:
; Save all registers used by high-level code.
; Push the IP of the high-level return thread, HLRETI.
; Set the IP to the following Forth thread.
; Do NEXT.
; When the Forth thread does EXIT, it will pop HLRETI
; into IP and execute that thread.  That will do a
; headerless code word to restore registers and RETI.

HLIRPT: PUSH IP     ; save whatever's in the IP register
    MOV 2(SP),IP    ; then get the Forth thread address into IP
    MOV W,2(SP)     ; save the W register "under" saved IP
    PUSH X          ; save remaining registers
    PUSH Y
    PUSH Q
    PUSH T
;    PUSH TOS       ; see note below regarding these 5 regs.
;    PUSH INDEX
;    PUSH LIMIT
;    PUSH R14
;    PUSH R15
    PUSH #HLRETI    ; push the address of the return IP
    NEXT            ; do the high-level code
    
; At the end of the interrupt word, EXIT will pop IP and do NEXT.
; For indirect threaded code: IP points to a one-word thread,
; which holds the address of a headerless CODE word XHLRETI.

HLRETI: DW XHLRETI  ; a one-word Forth thread
XHLRETI: DW $+2     ; cfa of a code word
;    POP R15        ; body of a code word
;    POP R14        ; see the note below regarding these 5 regs
;    POP LIMIT
;    POP INDEX
;    POP TOS
    POP T
    POP Q
    POP Y
    POP X
    POP IP
    POP W
    RETI

; NOTE: Here's why we don't save all the registers:

; PC and SR are saved by MSP430 interrupt/RETI.
; RSP (the MSP430 Stack Pointer SP) is preserved across 
;   an interrupt, since it's used to save registers.

; PSP: CamelForth primitives are defined such that
;   PSP is *always* a valid stack pointer (e.g., always
;   decrement-before-store and increment-after-fetch, 
;   never index from PSP with a negative offset).  So,
;   the parameter stack may be used by the (stack-neutral!)
;   interrupt word, and PSP will be left unchanged by it. 

; TOS, INDEX, LIMIT: well-behaved Forth code will always 
;   preserve TOS, INDEX, and LIMIT registers before 
;   using them.

; R14 and R15 are not used by ITC CamelForth.  If you
;   have defined CODE words that use them, and those
;   CODE words are part of your interrupt handler, then
;   you should modify this to save them properly.

; Begin an INTERRUPT colon definition
; Usage:   INTERRUPT: name  ...high level code...  ;
; Executing 'name' will return the address of the 
; interrupt handler (the machine code CALL #HLIRPT).

;Z INTERRUPT:   --      begin a high-level interrupt word
;   <BUILDS HIDE ] 
;   -2 IALLOT DOIRPT ,CF      like !COLON except DOIRPT
;   HLIRPT ,CALL ;

    HEADER(INTERRUPT,10,"INTERRUPT:",DOCOLON)
        DW BUILDS,HIDE,RIGHTBRACKET
        DW lit,-2,IALLOT,lit,DOIRPT,COMMACF
        DW lit,HLIRPT,COMMACALL,EXIT

; DOIRPT enters an interrupt handler, i.e., a machine
; code routine that ends with IRET.  This works with both
; pure machine code and high-level Forth interrupt code.
; This allows an INTERRUPT: word to be tested from the 
; command line.  Note that this is not recommended for
; normal execution, since it will push nine cells on the
; return stack!
; DOIRPT sets up the IRET return frame and then jumps to
; the handler at the pfa (address supplied in W).

DOIRPT: 
        PUSH #DONEIRPT  ; push return address on stack
        PUSH SR         ; push SR
        MOV W,PC        ; jump to the word's parameter field
        ; when the handler does IRET, it will go here:
DONEIRPT: NEXT       
