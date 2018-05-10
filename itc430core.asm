; ----------------------------------------------------------------------
; CamelForth for the Texas Instruments MSP430 
; (c) 2009,2014 Bradford J. Rodriguez.
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
; itc430core.asm - Machine Language Primitives - MSP430F1611
; B. Rodriguez  3 Jan 09
; ----------------------------------------------------------------------
; Revision History
; 26 feb 14 bjr - adapted from core430.s43 for naken_asm.
; 26 oct 12 bjr - removed target-specific code and includes to
;		forth430xxx.s43.  This file is now included from that file.
;  1 mar 09 bjr - changed Flash write and erase primitives to correctly
;       write RAM outside Info Flash and Main Flash address limits.


; ----------------------------------------------------------------------
; INTERPRETER LOGIC
; ITC NEXT is defined as
;        MOV @IP+,W      ; 2 fetch word address into W
;        MOV @W+,PC      ; 2 fetch code address into PC, W=PFA

;C EXECUTE   i*x xt -- j*x   execute Forth word
;C                           at 'xt'
        CODEHEADER(EXECUTE,7,"EXECUTE")
        MOV TOS,W       ; 1 put word address into W
        MOV @PSP+,TOS   ; 2 fetch new TOS
        MOV @W+,PC      ; 2 fetch code address into PC, W=PFA

;Z lit      -- x    fetch inline literal to stack
; This is the primtive compiled by LITERAL.
        CODEHEADER(lit,3,"lit")
        SUB #2,PSP      ; 1  push old TOS..
        MOV TOS,0(PSP)  ; 4  ..onto stack
        MOV @IP+,TOS    ; 2  fetch new TOS value
        NEXT            ; 4

;C EXIT     --            exit a colon definition
        CODEHEADER(EXIT,4,"EXIT")
        MOV @RSP+,IP    ; 2 pop old IP from return stack
        NEXT            ; 4

; ----------------------------------------------------------------------
; DEFINING WORDS - ROMable ITC model 

; DOCOLON enters a new high-level thread (colon definition.)
; (internal code fragment, not a Forth word)
DOCOLON: 
        PUSH IP         ; 3 save old IP on return stack
        MOV W,IP        ; 1 set new IP to PFA
        NEXT            ; 4

;C VARIABLE --            define a Forth VARIABLE
;   CREATE CELL ALLOT ;
; Action of ROMable variable is the same as CREATE; it builds a
; constant holding the RAM address.  See CREATE in hilvl430.s43.
        HEADER(VARIABLE,8,"VARIABLE",DOCOLON)
        DW CREATE,CELL,ALLOT,EXIT

;C CONSTANT --            define a Forth constant
;   <BUILDS  I,   Flashable Harvard model
;   DOES> (machine code fragment)
; Note that the constant is stored in Code space.
        HEADER(CONSTANT,8,"CONSTANT",DOCOLON)
        DW BUILDS,ICOMMA,XDOES
; DOCON, code action of CONSTANT,
; entered with W=Parameter Field Adrs
; This is also the action of VARIABLE (Harvard model)
; This is also the action of CREATE (Harvard model)
docreate: ; -- a-addr   ; ROMable CREATE fetches address from PFA
DOVAR:  ; -- a-addr     ; ROMable VARIABLE fetches address from PFA
DOCON:  ; -- x          ; CONSTANT fetches cell from PFA to TOS
        SUB #2,PSP      ; make room on stack
        MOV TOS,0(PSP)
        MOV @W,TOS      ; fetch from parameter field to TOS
        NEXT

;C 2CONSTANT   --            define a Forth double constant  ;mk
;   <BUILDS  I, I,           Flashable Harvard model
;   DOES> (machine code fragment)
; Note that the constant is stored in Code space.
        HEADER(TWOCONSTANT,9,"2CONSTANT",DOCOLON)
        DW BUILDS,ICOMMA,ICOMMA,XDOES
DOTWOCON:  ; ( -- w1 w2 )
      SUB #4,PSP       ; make room on stack
      MOV TOS,2(PSP)
      MOV @W+,TOS      ; fetch from parameter field to TOS
      MOV @W,0(PSP)    ; fetch secon word from parameter field to NOS
      NEXT



; DOCREATE's action is for a table in RAM.
; DOROM is the code action for a table in ROM;
; it returns the address of the parameter field.
DOROM:  ; -- a-addr     ; Table in ROM: get PFA into TOS
        SUB #2,PSP 
        MOV TOS,0(PSP)
        MOV W,TOS
        NEXT
;Z USER     n --        define user variable 'n'
;   <BUILDS I, DOES> (machine code fragment)    Flashable model
        HEADER(USER,4,"USER",DOCOLON)
        DW BUILDS,ICOMMA,XDOES
DOUSER: ; -- a-addr     ; add constant to User Pointer, result in TOS
        SUB #2,PSP
        MOV TOS,0(PSP)
        MOV @W,TOS
        ADD &UP,TOS
        NEXT

; DOALIAS  used to build a word which performs the action of
; another word.  Its action is to fetch the "alias" CFA from
; the parameter field, and execute that, e.g. DOES> I@ EXECUTE ;
; This is currently used only within the Forth kernel.
DOALIAS:  ; --          ; fetch CFA of word to execute
        MOV @W,W        ; 2 fetch from parameter field to W
        MOV @W+,PC      ; 2 fetch code address into PC, W=PFA

; DODOES is the code action of a DOES> clause.  For ITC Forth:
; defined word:  CFA: doescode
;                PFA: parameter field
;
; doescode: MOV #DODOES,PC      ; 16-bit direct jump, in two cells
;           high-level thread
;
; Note that we use JMP DODOES instead of CALL #DODOES because we can 
; efficiently obtain the thread address.  DODOES is entered with W=PFA.
; It enters the high-level thread with the address of the parameter
; field on top of stack.

dodoes: ; -- a-addr     ; 3 for MOV #DODOES,PC
        SUB #2,PSP      ; 1 make room on stack
        MOV TOS,0(PSP)  ; 4
        MOV W,TOS       ; 1 put defined word's PFA in TOS
        PUSH IP         ; 3 save old IP on return stack
        MOV -2(W),IP    ; 3 fetch adrs of doescode from defined word
        ADD #4,IP       ; 1 skip MOV instruction to get thread adrs
        NEXT            ; 4

; OPTION 1              ; OPTION 2
;  MOV #DODOES,PC   3   ;  CALL #DODOES      5
;   ...                 ;   ...
;  PUSH IP          3   ;  POP W            2
;  MOVE -2(W),IP    3   ;  PUSH IP          3
;  ADD #4,IP        1   ;  MOV W,IP         1


; ----------------------------------------------------------------------
; STACK OPERATIONS

;C DUP      x -- x x      duplicate top of stack
        CODEHEADER(DUP,3,"DUP") 
PUSHTOS: SUB    #2,PSP          ; 1  push old TOS..
        MOV     TOS,0(PSP)      ; 4  ..onto stack
        NEXT                    ; 4

;C ?DUP     x -- 0 | x x    DUP if nonzero
        CODEHEADER(QDUP,4,"?DUP")
        CMP     #0,TOS          ; 1  test for TOS nonzero
        JNZ     PUSHTOS         ; 2
NODUP:  NEXT                    ; 4

;C DROP     x --          drop top of stack
        CODEHEADER(DROP,4,"DROP")
        MOV     @PSP+,TOS       ; 2
        NEXT                    ; 4

;C SWAP     x1 x2 -- x2 x1    swap top two items
        CODEHEADER(SWAP,4,"SWAP")
        MOV     @PSP,W          ; 2
        MOV     TOS,0(PSP)      ; 4
        MOV     W,TOS           ; 1
        NEXT                    ; 4

;C OVER    x1 x2 -- x1 x2 x1   per stack diagram
        CODEHEADER(OVER,4,"OVER")
        MOV     @PSP,W          ; 2
        SUB     #2,PSP          ; 2
        MOV     TOS,0(PSP)      ; 4
        MOV     W,TOS           ; 1
        NEXT                    ; 4

;C ROT    x1 x2 x3 -- x2 x3 x1  per stack diagram
        CODEHEADER(ROT,3,"ROT")
        MOV     @PSP,W          ; 2 fetch x2
        MOV     TOS,0(PSP)      ; 4 store x3
        MOV     2(PSP),TOS      ; 3 fetch x1
        MOV     W,2(PSP)        ; 4 store x2
        NEXT                    ; 4

;X NIP    x1 x2 -- x2           per stack diagram
        CODEHEADER(NIP,3,"NIP")
        ADD     #2,PSP          ; 1
        NEXT                    ; 4

;C >R    x --   R: -- x   push to return stack
        CODEHEADER(TOR,2,">R")
        PUSH TOS
        MOV @PSP+,TOS
        NEXT


;C R>    -- x    R: x --   pop from return stack
        CODEHEADER(RFROM,2,"R>")
        SUB #2,PSP      ; 2
        MOV TOS,0(PSP)    ; 4
        MOV @RSP+,TOS
        NEXT

;C R@    -- x     R: x -- x   fetch from rtn stk
        CODEHEADER(RFETCH,2,"R@")
        SUB #2,PSP
        MOV TOS,0(PSP)
        MOV @RSP,TOS
        NEXT

;Z SP@  -- a-addr       get data stack pointer
        CODEHEADER(SPFETCH,3,"SP@")
        SUB #2,PSP
        MOV TOS,0(PSP)
        MOV PSP,TOS
        NEXT

;Z SP!  a-addr --       set data stack pointer
        CODEHEADER(SPSTORE,3,"SP!")
        MOV     TOS,PSP
        MOV     @PSP+,TOS       ; 2
        NEXT

;Z RP@  -- a-addr       get return stack pointer
        CODEHEADER(RPFETCH,3,"RP@")
        SUB #2,PSP
        MOV TOS,0(PSP)
        MOV RSP,TOS
        NEXT

;Z RP!  a-addr --       set return stack pointer
        CODEHEADER(RPSTORE,3,"RP!")
        MOV     TOS,RSP
        MOV     @PSP+,TOS       ; 2
        NEXT

;X TUCK   x1 x2 -- x2 x1 x2     per stack diagram
        HEADER(TUCK,4,"TUCK",DOCOLON)
        DW      SWAP,OVER,EXIT

; ----------------------------------------------------------------------
; MEMORY OPERATIONS

;C @       a-addr -- x   fetch cell from memory
        CODEHEADER(FETCH,1,"@")
        MOV     @TOS,TOS
        NEXT

;C !        x a-addr --   store cell in memory
        CODEHEADER(STORE,1,"!")
        MOV     @PSP+,0(TOS)
        MOV     @PSP+,TOS
        NEXT

;C C@     c-addr -- char   fetch char from memory
        CODEHEADER(CFETCH,2,"C@")
        MOV.B   @TOS,TOS
        NEXT

;C C!      char c-addr --    store char in memory
        CODEHEADER(CSTORE,2,"C!")
        MOV     @PSP+,W
        MOV.B   W,0(TOS)
        MOV     @PSP+,TOS
        NEXT

; FLASH MEMORY OPERATIONS
; Note that an I! or IC! to a RAM address >FLASHSTART will work -- it 
; will enable the flash, write the RAM, and then disable the flash.
; An FLERASE to a RAM address will merely clear that one RAM cell.

;Z FLERASE  a-addr n -- 
        CODEHEADER(FLERASE,7,"FLERASE")
        MOV     @PSP+,W         ; get address in W
        ADD     W,TOS           ; TOS=end adrs (first unerased adrs)
FLE_1:
        CMP     TOS,W           ; adr-end
        JC      FLE_X           ; if no borrow, adr>=end, do not erase
        ; is it within Main flash?
        CMP     #FLASHSTART,W
        JNC     FLE_INFO        ; if borrow, adr<start, check if Info
        CMP     #FLASHEND+1,W
        JNC     FLE_OK          ; if no borrow, adr>end, check if Info
FLE_INFO: ; is it within Info flash?
        CMP     #INFOSTART,W
        JNC     FLE_X           ; if borrow, adr<start, do not erase
        CMP     #INFOEND+1,W
        JC      FLE_X           ; if no borrow, adr>end, do not erase
FLE_OK: ; Address is either in Main flash, or in Info flash.
        ; Segment Erase from flash. 
        ; Assumes ACCVIE = NMIIE = OFIE = 0, watchdog disabled.
        ; Per section 5.3.2 of MSP430 Family User's Guide
        DINT                    ; Disable interrupts
        MOV #FWKEY,&FCTL3       ; Clear LOCK
        MOV #FWKEY+ERASE,&FCTL1 ; Enable segment erase
        MOV     #-1,0(W)        ; Dummy write in segment to erase
        MOV #FWKEY,&FCTL1       ; Done. Clear erase command.
        MOV #FWKEY+LOCK,&FCTL3  ; Done, set LOCK
        EINT                    ; Enable interrupts
        ; Advance flash pointer by 512 bytes or 128 bytes
        ; is it within Main flash?
        CMP     #FLASHSTART,W
        JNC     FL_INFO         ; if borrow, adr<start, must be Info
        CMP     #FLASHEND+1,W
        JC      FL_INFO         ; if no borrow, adr>end, must be Info
        ADD     #(MAINSEG-INFOSEG),W
FL_INFO: ADD    #INFOSEG,W
        JMP     FLE_1           ; continue till past end or outside limits
FLE_X:  MOV     @PSP+,TOS
        NEXT

; Program Space (Flash) operators 

;Z I!        x a-addr --   store cell in Instruction memory
        CODEHEADER(ISTORE,2,"I!")
        MOV     @PSP+,W         ; get data to write
        BIT     #1,TOS
        JNZ     IST_X           ; if not even address, do not write
        CMP     @TOS,W
        JZ      IST_X           ; if memory is desired value, do not write
        ; is it within Main flash?
        CMP     #FLASHSTART,TOS
        JNC     IST_INFO        ; if borrow, adr<start, check if Info
        CMP     #FLASHEND+1,TOS
        JNC     IST_OK          ; if no borrow, adr>end, check if Info
IST_INFO: ; is it within Info flash?
        CMP     #INFOSTART,TOS
        JNC     IST_RAM         ; if borrow, adr<start, assume it's RAM
        CMP     #INFOEND+1,TOS
        JC      IST_RAM         ; if no borrow, adr>end, assume it's RAM
IST_OK: ; Address is either in Main flash, or in Info flash.
        ; Byte/word write from flash. 
        ; Assumes location to write is already erased
        ; Assumes ACCVIE = NMIIE = OFIE = 0, watchdog disabled.
        ; Per section 5.3.3 of MSP430 Family User's Guide
        DINT                    ; Disable interrupts
        MOV #FWKEY,&FCTL3       ; Clear LOCK
        MOV #FWKEY+WRT,&FCTL1   ; Enable write
IST_RAM: ; If RAM, jump here to write.  FCTL1,FCTL3,EINT are superfluous
        MOV     W,0(TOS)        ; Write word to flash location
        MOV #FWKEY,&FCTL1       ; Done. Clear WRT.
        MOV #FWKEY+LOCK,&FCTL3  ; Set LOCK
        EINT                    ; Enable interrupts
IST_X:  MOV     @PSP+,TOS       ; pop new TOS
        NEXT

;Z IC!        x a-addr --   store char in Instruction memory
        CODEHEADER(ICSTORE,3,"IC!")
        MOV     @PSP+,W         ; get data to write
        CMP.B   @TOS,W
        JZ      IST_X           ; if memory is desired value, do not write
        ; is it within Main flash?
        CMP     #FLASHSTART,TOS
        JNC     ICST_INFO       ; if borrow, adr<start, check if Info
        CMP     #FLASHEND+1,TOS
        JNC     ICST_OK         ; if no borrow, adr>end, check if Info
ICST_INFO: ; is it within Info flash?
        CMP     #INFOSTART,TOS
        JNC     ICST_RAM        ; if borrow, adr<start, assume it's RAM
        CMP     #INFOEND+1,TOS
        JC      ICST_RAM        ; if no borrow, adr>end, assume it's RAM
ICST_OK: ; Address is either in Main flash, or in Info flash.
        ; Byte/word write from flash. 
        ; Assumes location to write is already erased
        ; Assumes ACCVIE = NMIIE = OFIE = 0, watchdog disabled.
        ; Per section 5.3.3 of MSP430 Family User's Guide
        DINT                    ; Disable interrupts
        MOV #FWKEY,&FCTL3       ; Clear LOCK
        MOV #FWKEY+WRT,&FCTL1   ; Enable write
ICST_RAM: ; If RAM, jump here to write.  FCTL1,FCTL3,EINT are superfluous
        MOV.B   W,0(TOS)        ; Write byte to flash location
        MOV #FWKEY,&FCTL1       ; Done. Clear WRT.
        MOV #FWKEY+LOCK,&FCTL3  ; Set LOCK
        EINT                    ; Enable interrupts
        JMP     IST_X

;Z I@       a-addr -- x   fetch cell from Instruction memory
        HEADER(IFETCH,2,"I@",FETCH+2)

;Z IC@       a-addr -- x   fetch char from Instruction memory
        HEADER(ICFETCH,3,"IC@",CFETCH+2)

;Z D->I     c-addr1 c-addr2 u --  move Data->Code
; Block move from Data space to Code space.  Flashable.
; For the MSP430, this uses a "smart" algorithm that uses word writes,
; rather than byte writes, whenever possible.  Note that byte reads
; are used for the source, so it need not be aligned.
        CODEHEADER(DTOI,4,"D->I")
        MOV     @PSP+,W     ; dest adrs
        MOV     @PSP+,X     ; src adrs
        CMP     #0,TOS
        JZ      DTOI_X
DTOI_LOOP: ; Begin flash write sequence
        DINT                    ; Disable interrupts
        MOV #FWKEY,&FCTL3       ; Clear LOCK
        MOV #FWKEY+WRT,&FCTL1   ; Enable write
        ; If length is 1, or dest. address is odd, do a byte write.
        ; Else, do a word write.
        CMP     #1,TOS
        JZ      DTOI_BYTE
        BIT     #1,W
        JNZ     DTOI_BYTE
DTOI_WORD: MOV.B @X+,Y          ; get low byte of word
        MOV.B   @X+,Q           ; get high byte of word
        SWPB    Q
        BIS     Q,Y             ; merge bytes
        MOV     Y,0(W)          ; write byte to dest 
        ADD     #2,W
        SUB     #1,TOS          ; another 1 will be subtracted below
        JMP     DTOI_END
DTOI_BYTE: MOV.B  @X+,0(W)      ; copy byte from src to dest
        ADD     #1,W
DTOI_END: ; End flash write sequence
        MOV #FWKEY,&FCTL1       ; Done. Clear WRT.
        MOV #FWKEY+LOCK,&FCTL3  ; Set LOCK
        EINT                    ; Enable interrupts
        SUB     #1,TOS
        JNZ     DTOI_LOOP
DTOI_X: MOV     @PSP+,TOS       ; pop new TOS
        NEXT

; ----------------------------------------------------------------------
; ARITHMETIC OPERATIONS

;C +       n1/u1 n2/u2 -- n3/u3     add n1+n2
        CODEHEADER(PLUS,1,"+")
        ADD     @PSP+,TOS
        NEXT

;C +!     n/u a-addr --       add cell to memory
        CODEHEADER(PLUSSTORE,2,"+!")
        ADD     @PSP+,0(TOS)
        MOV     @PSP+,TOS
        NEXT

;X M+       d n -- d         add single to double
        CODEHEADER(MPLUS,2,"M+")
        ADD     TOS,2(PSP)
        ADDC    #0,0(PSP)
        MOV     @PSP+,TOS
        NEXT

;C -      n1/u1 n2/u2 -- n3/u3    subtract n1-n2
        CODEHEADER(MINUS,1,"-")
        MOV     @PSP+,W
        SUB     TOS,W
        MOV     W,TOS
        NEXT

;C AND    x1 x2 -- x3            logical AND
        CODEHEADER(ANDD,3,"AND")
        AND     @PSP+,TOS
        NEXT

;C OR     x1 x2 -- x3           logical OR
        CODEHEADER(ORR,2,"OR")
        BIS     @PSP+,TOS
        NEXT

;C XOR    x1 x2 -- x3            logical XOR
        CODEHEADER(XORR,3,"XOR")
        XOR     @PSP+,TOS
        NEXT

;C INVERT   x1 -- x2            bitwise inversion
        CODEHEADER(INVERT,6,"INVERT")
        XOR     #-1,TOS
        NEXT

;C NEGATE   x1 -- x2            two's complement
        CODEHEADER(NEGATE,6,"NEGATE")
        XOR     #-1,TOS
        ADD     #1,TOS
        NEXT

;C 1+      n1/u1 -- n2/u2       add 1 to TOS
        CODEHEADER(ONEPLUS,2,"1+")
        ADD     #1,TOS
        NEXT

;C 1-      n1/u1 -- n2/u2     subtract 1 from TOS
        CODEHEADER(ONEMINUS,2,"1-")
        SUB     #1,TOS
        NEXT

;Z ><      x1 -- x2         swap bytes (not ANSI)
        CODEHEADER(SWAPBYTES,2,"><")
        SWPB    TOS
        NEXT

;C 2*      x1 -- x2         arithmetic left shift
        CODEHEADER(TWOSTAR,2,"2*")
        ADD     TOS,TOS
        NEXT

;C 2/      x1 -- x2        arithmetic right shift
        CODEHEADER(TWOSLASH,2,"2/")
        RRA     TOS
        NEXT

;C LSHIFT  x1 u -- x2    logical L shift u places
        CODEHEADER(LSHIFT,6,"LSHIFT")
        MOV     @PSP+,W
        AND     #1Fh,TOS        ; no need to shift more than 16
        JZ      LSH_X
LSH_1:  ADD     W,W
        SUB     #1,TOS
        JNZ     LSH_1
LSH_X:  MOV     W,TOS
        NEXT

;C RSHIFT  x1 u -- x2    logical R shift u places
        CODEHEADER(RSHIFT,6,"RSHIFT")
        MOV     @PSP+,W
        AND     #1Fh,TOS        ; no need to shift more than 16
        JZ      RSH_X
RSH_1:  CLRC
        RRC     W
        SUB     #1,TOS
        JNZ     RSH_1
RSH_X:  MOV     W,TOS
        NEXT

; ----------------------------------------------------------------------
; COMPARISON OPERATIONS 

;C 0=     n/u -- flag    return true if TOS=0
        CODEHEADER(ZEROEQUAL,2,"0=")
        SUB     #1,TOS      ; borrow (clear cy) if TOS was 0
        SUBC    TOS,TOS     ; TOS=-1 if borrow was set
        NEXT

;C 0<     n -- flag      true if TOS negative
        CODEHEADER(ZEROLESS,2,"0<")
        ADD     TOS,TOS     ; set cy if TOS negative
        SUBC    TOS,TOS     ; TOS=-1 if carry was clear
        XOR     #-1,TOS     ; TOS=-1 if carry was set
        NEXT

;C =      x1 x2 -- flag         test x1=x2
        CODEHEADER(EQUAL,1,"=")
        MOV     @PSP+,W
        SUB     TOS,W       ; x1-x2 in W, flags set
        JZ      TOSTRUE
TOSFALSE: MOV   #0,TOS
        NEXT

;X <>     x1 x2 -- flag    test not eq (not ANSI)
        HEADER(NOTEQUAL,2,"<>",DOCOLON)
        DW EQUAL,ZEROEQUAL,EXIT

;C <      n1 n2 -- flag        test n1<n2, signed
        CODEHEADER(LESS,1,"<")
        MOV     @PSP+,W
        SUB     TOS,W       ; x1-x2 in W, flags set
        JGE     TOSFALSE
TOSTRUE: MOV    #-1,TOS
        NEXT

;C >     n1 n2 -- flag         test n1>n2, signed
        HEADER(GREATER,1,">",DOCOLON)
        DW SWAP,LESS,EXIT

;C U<    u1 u2 -- flag       test u1<u2, unsigned
        CODEHEADER(ULESS,2,"U<")
        MOV     @PSP+,W
        SUB     TOS,W       ; u1-u2 in W, cy clear if borrow
        JNC     TOSTRUE
        JMP     TOSFALSE

;X U>    u1 u2 -- flag     u1>u2 unsgd (not ANSI)
        HEADER(UGREATER,2,"U>",DOCOLON)
        DW SWAP,ULESS,EXIT

; ----------------------------------------------------------------------
; LOOP AND BRANCH OPERATIONS 
; These use relative branch addresses: a branch is ADD @IP,IP

;Z branch   --                  branch always
        CODEHEADER(bran,6,"branch")
dobran:  ADD @IP,IP   ; 2
        NEXT            ; 4

;Z ?branch   x --              branch if TOS zero
        CODEHEADER(qbran,7,"?branch")
        ADD #0,TOS      ; 1  test TOS value
        MOV @PSP+,TOS   ; 2  pop new TOS value (doesn't change flags)
        JZ  dobran    ; 2  if TOS was zero, take the branch
        ADD #2,IP       ; 1  else skip the branch destination
        NEXT            ; 4

;Z (do)    n1|u1 n2|u2 --  R: -- sys1 sys2
;Z                          run-time code for DO
; '83 and ANSI standard loops terminate when the boundary of 
; limit-1 and limit is crossed, in either direction.  This can 
; be conveniently implemented by making the limit 8000h, so that
; arithmetic overflow logic can detect crossing.  I learned this 
; trick from Laxen & Perry F83.
; fudge factor = 8000h-limit, to be added to the start value.
        ; CODEHEADER(xdo,4,"(do)")
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      4,"(do)"
        .align 16
xdo: DW     $+2       
        SUB     #4,RSP          ; push old loop values on return stack
        MOV     LIMIT,2(RSP)
        MOV     INDEX,0(RSP)
        MOV     #8000h,LIMIT    ; compute 8000h-limit "fudge factor"
        SUB     @PSP+,LIMIT
        MOV     TOS,INDEX       ; loop ctr = index+fudge
        ADD     LIMIT,INDEX
        MOV     @PSP+,TOS       ; pop new TOS
        NEXT

;Z (loop)   R: sys1 sys2 --  | sys1 sys2
;Z                        run-time code for LOOP
; Add 1 to the loop index.  If loop terminates, clean up the 
; return stack and skip the branch.  Else take the inline branch.  
; Note that LOOP terminates when index=8000h.
        ; CODEHEADER(xloop,6,"(loop)")
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      6,"(loop)"
        .align 16
xloop: DW     $+2
        ADD     #1,INDEX
        BIT     #100h,SR    ; is overflow bit set?
        JZ      dobran    ; no overflow = loop
        ADD     #2,IP       ; overflow = loop done, skip branch ofs
        MOV     @RSP+,INDEX ; restore old loop values
        MOV     @RSP+,LIMIT
        NEXT

;Z (+loop)   n --   R: sys1 sys2 --  | sys1 sys2
;Z                        run-time code for +LOOP
; Add n to the loop index.  If loop terminates, clean up the 
; return stack and skip the branch. Else take the inline branch.
        ; CODEHEADER(xplusloop,7,"(+loop)")
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      7,"(+loop)"
        .align 16
xplusloop: DW     $+2
        ADD     TOS,INDEX
        MOV     @PSP+,TOS   ; get new TOS, doesn't change flags
        BIT     #100h,SR    ; is overflow bit set?
        JZ      dobran    ; no overflow = loop
        ADD     #2,IP       ; overflow = loop done, skip branch ofs
        MOV     @RSP+,INDEX ; restore old loop values
        MOV     @RSP+,LIMIT
        NEXT

;C I        -- n   R: sys1 sys2 -- sys1 sys2
;C                  get the innermost loop index
        CODEHEADER(II,1,"I")
        SUB     #2,PSP          ; make room in TOS
        MOV     TOS,0(PSP)
        MOV     INDEX,TOS       ; index = loopctr - fudge
        SUB     LIMIT,TOS
        NEXT

;C J        -- n   R: 4*sys -- 4*sys
;C                  get the second loop index
        CODEHEADER(JJ,1,"J")
        SUB     #2,PSP          ; make room in TOS
        MOV     TOS,0(PSP)
        MOV     @RSP,TOS        ; index = loopctr - fudge
        SUB     2(RSP),TOS
        NEXT

;C UNLOOP   --   R: sys1 sys2 --  drop loop parms
        CODEHEADER(UNLOOP,6,"UNLOOP")
        MOV     @RSP+,INDEX     ; restore old loop values
        MOV     @RSP+,LIMIT
        NEXT

; ----------------------------------------------------------------------
; MULTIPLY AND DIVIDE

;C UM*     u1 u2 -- ud   unsigned 16x16->32 mult.
        CODEHEADER(UMSTAR,3,"UM*")
        ; IROP1 = TOS register
        MOV     @PSP,IROP2L     ; get u1, leave room on stack
;
; T.I. SIGNED MULTIPLY SUBROUTINE: IROP1 x IROP2L -> IRACM|IRACL
MPYU:   CLR IRACL ; 0 -> LSBs RESULT
        CLR IRACM ; 0 -> MSBs RESULT
; UNSIGNED MULTIPLY AND ACCUMULATE SUBROUTINE:
; (IROP1 x IROP2L) + IRACM|IRACL -> IRACM|IRACL
MACU:   CLR IROP2M  ; MSBs MULTIPLIER
        MOV #1,IRBT ; BIT TEST REGISTER
L_002:  BIT IRBT,IROP1 ; TEST ACTUAL BIT
        JZ L_01     ; IF 0: DO NOTHING
        ADD IROP2L,IRACL ; IF 1: ADD MULTIPLIER TO RESULT
        ADDC IROP2M,IRACM
L_01:   RLA IROP2L  ; MULTIPLIER x 2
        RLC IROP2M
;
        RLA IRBT    ; NEXT BIT TO TEST
        JNC L_002   ; IF BIT IN CARRY: FINISHED
; END T.I. ROUTINE  section 5.1.1 of MSP430 Family Application Reports
        MOV     IRACL,0(PSP)    ; low result on stack
        MOV     IRACM,TOS       ; high result in TOS
        NEXT

;C UM/MOD   ud u1 -- u2 u3   unsigned 32/16->16
        CODEHEADER(UMSLASHMOD,6,"UM/MOD")
        ; IROP1 = TOS register
        MOV     @PSP+,IROP2M    ; get ud hi
        MOV     @PSP,IROP2L     ; get ud lo, leave room on stack
;
; T.I. UNSIGNED DIVISION SUBROUTINE 32-BIT BY 16-BIT
; IROP2M|IROP2L : IROP1 -> IRACL REMAINDER IN IROP2M
; RETURN: CARRY = 0: OK CARRY = 1: QUOTIENT > 16 BITS
DIVIDE: CLR IRACL   ; CLEAR RESULT
        MOV #17,IRBT ; INITIALIZE LOOP COUNTER
DIV1:   CMP IROP1,IROP2M ;
        JLO DIV2
        SUB IROP1,IROP2M
DIV2:   RLC IRACL
        JC DIV4     ; Error: result > 16 bits
        DEC IRBT    ; Decrement loop counter
        JZ DIV3     ; Is 0: terminate w/o error
        RLA IROP2L
        RLC IROP2M
        JNC DIV1
        SUB IROP1,IROP2M
        SETC
        JMP DIV2
DIV3:   CLRC        ; No error, C = 0
DIV4:   ; Error indication in C
; END T.I. ROUTINE  Section 5.1.5 of MSP430 Family Application Reports
        MOV     IROP2M,0(PSP)   ; remainder on stack
        MOV     IRACL,TOS       ; quotient in TOS
        NEXT

; ----------------------------------------------------------------------
; BLOCK AND STRING OPERATIONS

;C FILL   c-addr u char --  fill memory with char
        CODEHEADER(FILL,4,"FILL")
        MOV     @PSP+,X     ; count
        MOV     @PSP+,W     ; address
        CMP     #0,X
        JZ      FILL_X
FILL_1: MOV.B   TOS,0(W)    ; store char in memory
        ADD     #1,W
        SUB     #1,X
        JNZ     FILL_1
FILL_X: MOV     @PSP+,TOS   ; pop new TOS
        NEXT

;X CMOVE   c-addr1 c-addr2 u --  move from bottom
; as defined in the ANSI optional String word set
; On byte machines, CMOVE and CMOVE> are logical
; factors of MOVE.  They are easy to implement on
; CPUs which have a block-move instruction.
        CODEHEADER(CMOVE,5,"CMOVE")
        MOV     @PSP+,W     ; dest adrs
        MOV     @PSP+,X     ; src adrs
        CMP     #0,TOS
        JZ      CMOVE_X
CMOVE_1: MOV.B  @X+,0(W)    ; copy byte
        ADD     #1,W
        SUB     #1,TOS
        JNZ     CMOVE_1
CMOVE_X: MOV    @PSP+,TOS   ; pop new TOS
        NEXT

;X CMOVE>  c-addr1 c-addr2 u --  move from top
; as defined in the ANSI optional String word set
        CODEHEADER(CMOVEUP,6,"CMOVE>")
        MOV     @PSP+,W     ; dest adrs
        MOV     @PSP+,X     ; src adrs
        CMP     #0,TOS
        JZ      CMOVU_X
        ADD     TOS,W       ; start at end
        ADD     TOS,X
CMOVU_1: SUB    #1,X
        SUB     #1,W
        MOV.B   @X,0(W)     ; copy byte
        SUB     #1,TOS
        JNZ     CMOVU_1
CMOVU_X: MOV    @PSP+,TOS   ; pop new TOS
        NEXT

;Z I->D     c-addr1 c-addr2 u --  move Code->Data
; Block move from Code space to Data space.
; On the MSP430, this is the same as CMOVE.
       HEADER(ITOD,4,"I->D",CMOVE+2)

;Z SKIP   c-addr u c -- c-addr' u'
;Z                          skip matching chars
; Although SKIP, SCAN, and S= are perhaps not the ideal factors 
; of WORD and FIND, they closely follow the string operations 
; available on many CPUs, and so are easy to implement and fast.
        CODEHEADER(SKIP,4,"SKIP")
        MOV     @PSP+,X     ; get count
        MOV     @PSP,W      ; get address, leave space on stack
        CMP     #0,X
        JZ      SKIP_X
SKIP_1: CMP.B   @W,TOS      ; does character match?
        JNZ     SKIP_X      ; no, we are done
        ADD     #1,W
        SUB     #1,X
        JNZ     SKIP_1
SKIP_X: MOV     W,0(PSP)    ; store updated address on stack
        MOV     X,TOS       ; updated count to TOS
        NEXT

;Z SCAN    c-addr u c -- c-addr' u'
;Z                      find matching char
        CODEHEADER(SCAN,4,"SCAN")
        MOV     @PSP+,X     ; get count
        MOV     @PSP,W      ; get address, leave space on stack
        CMP     #0,X
        JZ      SCAN_X
SCAN_1: CMP.B   @W,TOS      ; does character match?
        JZ      SCAN_X      ; yes, we are done
        ADD     #1,W
        SUB     #1,X
        JNZ     SCAN_1
SCAN_X: MOV     W,0(PSP)    ; store updated address on stack
        MOV     X,TOS       ; updated count to TOS
        NEXT

;Z S=    c-addr1 c-addr2 u -- n   string compare
;Z             n<0: s1<s2, n=0: s1=s2, n>0: s1>s2
        CODEHEADER(SEQUAL,2,"S=")
        MOV     @PSP+,W     ; adrs2
        MOV     @PSP+,X     ; adrs1
        CMP     #0,TOS
        JZ      SEQU_X
SEQU_1: CMP.B   @W+,0(X)    ; compare char1-char2
        JNZ     SMISMATCH
        ADD     #1,X
        SUB     #1,TOS
        JNZ     SEQU_1
        ; no mismatch found, strings are equal, TOS=0
        JMP     SEQU_X
        ; mismatch found, CY clear if borrow set (s1<s2)
SMISMATCH: SUBC TOS,TOS     ; TOS=-1 if borrow was set
        ADD     TOS,TOS     ; TOS=-2 or 0
        ADD     #1,TOS      ; TOS=-1 or +1
SEQU_X: NEXT                ; return result in TOS

;Z N=    c-addr1 c-addr2 u -- n   name compare
;Z             n<0: s1<s2, n=0: s1=s2, n>0: s1>s2
; For Harvard model, c-addr1 is Data, c-addr2 is Header.
; On MSP430, both use the same fetch instruction, so N= is the same as S=.
        HEADER(NEQUAL,2,"N=",SEQUAL+2)

