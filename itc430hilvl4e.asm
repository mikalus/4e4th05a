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
; itc430hilvl.asm - High Level Words - MSP430
; B. Rodriguez  4 Jan 09

;   Forth words are documented as follows:
;x   NAME     stack -- stack    description
;   where x=C for ANS Forth Core words, X for ANS
;   Extensions, Z for internal or private words.
; ----------------------------------------------------------------------
; REVISION HISTORY
;  10 may 18 mk  - added:
;    - added 1MS to wait about 1 millisecond.
;    - added MS to wait about n milliseconds.
;    - added \ to have comments.
;    - added BELL to send $07 (bell) to Terminal.
;    - added ESC[ to start esc-sequence.
;    - added PN to send parameter of esc-sequence.
;    - added PN; to send delimiter ; followed by parameter.
;    - added AT-XY to set cursor position in terminal.
;    - added PAGE to "page" command to terminal to clear screen.
;    - added .VER to print version
;    - added .VER to print version.
;    - Modified WORDS to stop&go.
;    - CAPITALIZE in '  (tick)
;    - modified QUIT to become 4e4th prompt.
;    - CAPITALIZE in INTERPRET.
;    - Checking stack underflow in INTERPRET.
;    - added CAPITALIZE to capitalize string.
;    - added UPC to capitalize character.
;    - added ?STACK for checking stack underflow.
;    - CAPITALIZE in IWORD for case INsensitiv interpretation.
;    - Modified ACCEPT : backspace writing over characters in terminal.
;  1 mar 14 bjr - adapted from hilvl430.s43 for naken_asm.
; 22 dec 13 bjr - added XON/OFF logic to interpreter loop
; 27 nov 12 bjr - fixed S" IS" to use PARSE
; 13 nov 12 bjr - added PARSE, fixed ( to use PARSE.
; 12 nov 12 bjr - fixed FM/MOD.
; 17 jan 09 bjr - changed label _DP to DDP for compatibility with token
;   naming convention.  Now uses DEST macro to compute branch offsets.
; 11 jan 09 - modified QUIT for Xon/Xoff flow control
; 4 jan 09 - created from Camel86h.asm.

; SYSTEM VARIABLES & CONSTANTS ==================

;Z u0      -- a-addr       current user area adrs
;  0 USER U0
    HEADER(U0,2,"U0",DOUSER)
        DW 0

;C >IN     -- a-addr        holds offset into TIB
;  2 USER >IN
    HEADER(TOIN,3,">IN",DOUSER)
        DW 2

;C BASE    -- a-addr       holds conversion radix
;  4 USER BASE
    HEADER(BASE,4,"BASE",DOUSER)
        DW 4

;C STATE   -- a-addr       holds compiler state
;  6 USER STATE
    HEADER(STATE,5,"STATE",DOUSER)
        DW 6

;Z dp      -- a-addr       holds dictionary ptr
;  8 USER DP
    HEADER(DDP,2,"DP",DOUSER)
        DW 8

;Z 'source  -- a-addr      two cells: len, adrs
; 10 USER 'SOURCE
    HEADER(TICKSOURCE,7,"\'SOURCE",DOUSER)
        DW 10

;Z latest    -- a-addr     last word in dict.
;   14 USER LATEST
    HEADER(LATEST,6,"LATEST",DOUSER)
        DW 14

;Z hp       -- a-addr     HOLD pointer
;   16 USER HP
    HEADER(HP,2,"HP",DOUSER)
        DW 16

;Z LP       -- a-addr     Leave-stack pointer
;   18 USER LP
    HEADER(LP,2,"LP",DOUSER)
        DW 18

;Z IDP    -- a-addr        ROM dictionary pointer
;   20 USER IDP
    HEADER(IDP,3,"IDP",DOUSER)
        DW 20

;Z NEWEST   -- a-addr       temporary LATEST storage
;   22 USER NEWEST
    HEADER(NEWEST,6,"NEWEST",DOUSER)
        DW 22

;Z XON/OFF  -- a-addr       XON and XOFF characters
;   24 USER XON/OFF
    HEADER(XONOFF,7,"XON/OFF",DOUSER)
        DW 24

; user variables 26,28,30 tbd

;X PAD       -- a-addr    user PAD buffer
;                         = end of hold area!
    HEADER(PAD,3,"PAD",DOUSER)
        DW PADAREA-UAREA

;Z l0       -- a-addr     bottom of Leave stack
    HEADER(L0,2,"L0",DOUSER)
        DW LSTACK-UAREA

;Z r0       -- a-addr     end of return stack
    HEADER(RZERO,2,"R0",DOUSER)
        DW RSTACK-UAREA

;Z s0       -- a-addr     end of parameter stack
    HEADER(S0,2,"S0",DOUSER)
        DW PSTACK-UAREA

;X tib     -- a-addr     Terminal Input Buffer
;  HEX 80 USER TIB       8086: above user area
    HEADER(TIB,3,"TIB",DOUSER)
        DW TIBAREA-UAREA

;Z tibsize  -- n         size of TIB
    HEADER(TIBSIZE,7,"TIBSIZE",DOCON)
        DW TIB_SIZE-2    ; 2 chars safety zone 

;C BL      -- char            an ASCII space
    HEADER(BLANK,2,"BL",DOCON)
        DW 20h

;Z uinit    -- addr  initial values for user area
; MSP430: we also use this to initialize the RAM interrupt
; vectors, which immediately follow the user area.
; Per init430f1611.s43, allocate 16 cells for user
; variables, followed by 30 cells for interrupt vectors.
    HEADER(UINIT,5,"UINIT",DOROM)
        DW 0,0,10,0     ; reserved,>IN,BASE,STATE
        DW RAMDICT      ; DP
        DW 0,0          ; SOURCE init'd elsewhere
        DW lastword     ; LATEST
        DW 0,0          ; HP,LP init'd elsewhere
        DW ROMDICT      ; IDP
        DW 0            ; NEWEST not init'd
        DW 0            ; XON/OFF initially disabled
        DW 0,0,0        ; user variables TBD
    ; RAM interrupt vectors, 15 vectors of 2 cells each
        MOV #nullirq,PC
        MOV #nullirq,PC
        MOV #nullirq,PC
        MOV #nullirq,PC
        MOV #nullirq,PC
        MOV #nullirq,PC
        MOV #nullirq,PC
        MOV #nullirq,PC
        MOV #nullirq,PC
        MOV #nullirq,PC
        MOV #nullirq,PC
        MOV #nullirq,PC
        MOV #nullirq,PC
        MOV #nullirq,PC
        MOV #nullirq,PC

;Z #init    -- n    #bytes of user area init data
    HEADER(NINIT,5,"#INIT",DOCON)
        DW (UAREA_SIZE+VECS_SIZE)*2     ; SIZEs given in cells

; ARITHMETIC OPERATORS ==========================

;C S>D    n -- d          single -> double prec.
;   DUP 0< ;
    HEADER(STOD,3,"S>D",DOCOLON)
        DW DUP,ZEROLESS,EXIT

;Z ?NEGATE  n1 n2 -- n3  negate n1 if n2 negative
;   0< IF NEGATE THEN ;        ...a common factor
    HEADER(QNEGATE,7,"?NEGATE",DOCOLON)
        DW ZEROLESS,qbran
        DEST(QNEG1)
        DW NEGATE
QNEG1:  DW EXIT

;C ABS     n1 -- +n2     absolute value
;   DUP ?NEGATE ;
    HEADER(ABBS,3,"ABS",DOCOLON)
        DW DUP,QNEGATE,EXIT

;X DNEGATE   d1 -- d2     negate double precision
;   SWAP INVERT SWAP INVERT 1 M+ ;
    HEADER(DNEGATE,7,"DNEGATE",DOCOLON)
        DW SWAP,INVERT,SWAP,INVERT,lit,1,MPLUS
        DW EXIT

;Z ?DNEGATE  d1 n -- d2   negate d1 if n negative
;   0< IF DNEGATE THEN ;       ...a common factor
    HEADER(QDNEGATE,8,"?DNEGATE",DOCOLON)
        DW ZEROLESS,qbran
        DEST(DNEG1)
        DW DNEGATE
DNEG1:  DW EXIT

;X DABS     d1 -- +d2    absolute value dbl.prec.
;   DUP ?DNEGATE ;
    HEADER(DABS,4,"DABS",DOCOLON)
        DW DUP,QDNEGATE,EXIT

;C M*     n1 n2 -- d    signed 16*16->32 multiply
;   2DUP XOR >R        carries sign of the result
;   SWAP ABS SWAP ABS UM*
;   R> ?DNEGATE ;
    HEADER(MSTAR,2,"M*",DOCOLON)
        DW TWODUP,XORR,TOR
        DW SWAP,ABBS,SWAP,ABBS,UMSTAR
        DW RFROM,QDNEGATE,EXIT

;C SM/REM   d1 n1 -- n2 n3   symmetric signed div
;   2DUP XOR >R              sign of quotient
;   OVER >R                  sign of remainder
;   ABS >R DABS R> UM/MOD
;   SWAP R> ?NEGATE
;   SWAP R> ?NEGATE ;
; Ref. dpANS-6 section 3.2.2.1.
    HEADER(SMSLASHREM,6,"SM/REM",DOCOLON)
        DW TWODUP,XORR,TOR,OVER,TOR
        DW ABBS,TOR,DABS,RFROM,UMSLASHMOD
        DW SWAP,RFROM,QNEGATE,SWAP,RFROM,QNEGATE
        DW EXIT

;C    d1 n1 -- n2 n3   floored signed div'n
;                            courtesy of Ed Smeda
;   DUP >R  SM/REM  2DUP  1 <  AND  IF
;     SWAP R@ +  SWAP 1-  THEN
;   R> DROP ;
; Ref. dpANS-6 section 3.2.2.1.
;    HEADER  FMSLASHMOD,6,'FM/MOD',DOCOLON
;	DW DUP,TOR,SMSLASHREM
;        DW TWODUP,lit,1,LESS,ANDD,qbran
;        DEST  FMMOD1
;        DW SWAP,RFETCH,PLUS,SWAP,ONEMINUS
;FMMOD1: DW RFROM,DROP,EXIT

; Fixed FM/MOD, added 12 nov 2012
;C FM/MOD   d1 n1 -- n2 n3   floored signed div'n
;   DUP >R              divisor
;   2DUP XOR >R         sign of quotient
;   >R                  divisor
;   DABS R@ ABS UM/MOD
;   SWAP R> ?NEGATE SWAP  apply sign to remainder
;   R> 0< IF              if quotient negative,
;       NEGATE
;       OVER IF             if remainder nonzero,
;         R@ ROT -  SWAP 1-     adjust rem,quot
;       THEN
;   THEN  R> DROP ;
; Ref. dpANS-6 section 3.2.2.1.
    HEADER(FMSLASHMOD,6,"FM/MOD",DOCOLON)
        DW DUP,TOR,TWODUP,XORR,TOR,TOR
        DW DABS,RFETCH,ABBS,UMSLASHMOD
        DW SWAP,RFROM,QNEGATE,SWAP,RFROM,ZEROLESS,qbran
        DEST(FMMOD1)
        DW NEGATE,OVER,qbran
        DEST(FMMOD2)
        DW RFETCH,ROT,MINUS,SWAP,ONEMINUS
FMMOD2: 
FMMOD1: DW RFROM,DROP,EXIT


;C *      n1 n2 -- n3       signed multiply
;   M* DROP ;
    HEADER(STAR,1,"*",DOCOLON)
        DW MSTAR,DROP,EXIT

;C /MOD   n1 n2 -- n3 n4    signed divide/rem'dr
;   >R S>D R> FM/MOD ;
    HEADER(SLASHMOD,4,"/MOD",DOCOLON)
        DW TOR,STOD,RFROM,FMSLASHMOD,EXIT

;C /      n1 n2 -- n3       signed divide
;   /MOD nip ;
    HEADER(SLASH,1,"/",DOCOLON)
        DW SLASHMOD,NIP,EXIT

;C MOD    n1 n2 -- n3       signed remainder
;   /MOD DROP ;
    HEADER(MODD,3,"MOD",DOCOLON)
        DW SLASHMOD,DROP,EXIT

;C */MOD  n1 n2 n3 -- n4 n5    n1*n2/n3, rem&quot
;   >R M* R> FM/MOD ;
    HEADER(SSMOD,5,"*/MOD",DOCOLON)
        DW TOR,MSTAR,RFROM,FMSLASHMOD,EXIT

;C */     n1 n2 n3 -- n4        n1*n2/n3
;   */MOD nip ;
    HEADER(STARSLASH,2,"*/",DOCOLON)
        DW SSMOD,NIP,EXIT

;C MAX    n1 n2 -- n3       signed maximum
;   2DUP < IF SWAP THEN DROP ;
    HEADER(MAX,3,"MAX",DOCOLON)
        DW TWODUP,LESS,qbran
        DEST(MAX1)
        DW  SWAP
MAX1:   DW DROP,EXIT

;C MIN    n1 n2 -- n3       signed minimum
;   2DUP > IF SWAP THEN DROP ;
    HEADER(MIN,3,"MIN",DOCOLON)
        DW TWODUP,GREATER,qbran
        DEST(MIN1)
        DW SWAP
MIN1:   DW DROP,EXIT

; DOUBLE OPERATORS ==============================

;C 2@    a-addr -- x1 x2    fetch 2 cells
;   DUP CELL+ @ SWAP @ ;
;   the lower address will appear on top of stack
    HEADER(TWOFETCH,2,"2@",DOCOLON)
        DW DUP,CELLPLUS,FETCH,SWAP,FETCH,EXIT

;C 2!    x1 x2 a-addr --    store 2 cells
;   SWAP OVER ! CELL+ ! ;
;   the top of stack is stored at the lower adrs
    HEADER(TWOSTORE,2,"2!",DOCOLON)
        DW SWAP,OVER,STORE,CELLPLUS,STORE,EXIT

;C 2DROP  x1 x2 --          drop 2 cells
;   DROP DROP ;
    HEADER(TWODROP,5,"2DROP",DOCOLON)
        DW DROP,DROP,EXIT

;C 2DUP   x1 x2 -- x1 x2 x1 x2   dup top 2 cells
;   OVER OVER ;
    HEADER(TWODUP,4,"2DUP",DOCOLON)
        DW OVER,OVER,EXIT

;C 2SWAP  x1 x2 x3 x4 -- x3 x4 x1 x2  per diagram
;   ROT >R ROT R> ;
    HEADER(TWOSWAP,5,"2SWAP",DOCOLON)
        DW ROT,TOR,ROT,RFROM,EXIT

;C 2OVER  x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2
;   >R >R 2DUP R> R> 2SWAP ;
    HEADER(TWOOVER,5,"2OVER",DOCOLON)
        DW TOR,TOR,TWODUP,RFROM,RFROM
        DW TWOSWAP,EXIT

; INPUT/OUTPUT ==================================

;C COUNT   c-addr1 -- c-addr2 u  counted->adr/len
;   DUP CHAR+ SWAP C@ ;
    HEADER(COUNT,5,"COUNT",DOCOLON)
        DW DUP,CHARPLUS,SWAP,CFETCH,EXIT

;C CR      --               output newline
;   0D EMIT 0A EMIT ;
    HEADER(CR,2,"CR",DOCOLON)
        DW lit,0dh,EMIT,lit,0ah,EMIT,EXIT

;C SPACE   --               output a space
;   BL EMIT ;
    HEADER(SPACE,5,"SPACE",DOCOLON)
        DW BLANK,EMIT,EXIT

;C SPACES   n --            output n spaces
;   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
    HEADER(SPACES,6,"SPACES",DOCOLON)
SPCS1:  DW DUP,qbran
        DEST(SPCS2)
        DW SPACE,ONEMINUS,bran
        DEST(SPCS1)
SPCS2:  DW DROP,EXIT

;Z umin     u1 u2 -- u      unsigned minimum
;   2DUP U> IF SWAP THEN DROP ;
    HEADER(UMIN,4,"UMIN",DOCOLON)
        DW TWODUP,UGREATER,qbran
        DEST(UMIN1)
        DW SWAP
UMIN1:  DW DROP,EXIT

;Z umax    u1 u2 -- u       unsigned maximum
;   2DUP U< IF SWAP THEN DROP ;
    HEADER(UMAX,4,"UMAX",DOCOLON)
        DW TWODUP,ULESS,qbran
        DEST(UMAX1)
        DW SWAP
UMAX1:  DW DROP,EXIT

;C ACCEPT  c-addr +n -- +n'  get line from term'l
;   OVER + 1- OVER      -- sa ea a
;   BEGIN KEY           -- sa ea a c
;   DUP 0D <> WHILE
;       DUP EMIT        -- sa ea a c
;       DUP 8 = IF  DROP 1-    >R OVER R> UMAX
;             ELSE  OVER C! 1+ OVER UMIN
;       THEN            -- sa ea a
;   REPEAT              -- sa ea a c
;   DROP NIP SWAP - ;
    HEADER(ACCEPT,6,"ACCEPT",DOCOLON)
        DW OVER,PLUS,ONEMINUS,OVER
ACC1:   DW KEY,DUP,lit,0Dh,NOTEQUAL,qbran
        DEST(ACC5)
        DW DUP,EMIT,DUP,lit,8,EQUAL,qbran
        DEST(ACC3)
        DW DROP,ONEMINUS,TOR,OVER,RFROM,UMAX
        DW SPACE,lit,8,EMIT ;mk clear on BS
        DW bran
        DEST(ACC4)
ACC3:   DW OVER,CSTORE,ONEPLUS,OVER,UMIN
ACC4:   DW bran
        DEST(ACC1)
ACC5:   DW DROP,NIP,SWAP,MINUS,EXIT

;C TYPE    c-addr +n --     type line to term'l
;   ?DUP IF
;     OVER + SWAP DO I C@ EMIT LOOP
;   ELSE DROP THEN ;
    HEADER(TYP,4,"TYPE",DOCOLON)
        DW QDUP,qbran
        DEST(TYP4)
        DW OVER,PLUS,SWAP,xdo
TYP3:   DW II,CFETCH,EMIT,xloop
        DEST(TYP3)
        DW bran
        DEST(TYP5)
TYP4:   DW DROP
TYP5:   DW EXIT


; HARVARD MODEL EXTENSIONS (split Code & Data)

;Z ICOUNT  c-addr1 -- c-addr2 u  counted->adr/len
;   DUP CHAR+ SWAP IC@ ;          from Code space
    HEADER(ICOUNT,6,"ICOUNT",DOCOLON)
        DW DUP,CHARPLUS,SWAP,ICFETCH,EXIT

;Z ITYPE   c-addr +n --       type line to term'l
;   ?DUP IF                       from Code space
;     OVER + SWAP DO I IC@ EMIT LOOP
;   ELSE DROP THEN ;
    HEADER(ITYPE,5,"ITYPE",DOCOLON)
        DW QDUP,qbran
        DEST(ITYP4)
        DW OVER,PLUS,SWAP,xdo
ITYP3:  DW II,ICFETCH,EMIT,xloop
        DEST(ITYP3)
        DW bran
        DEST(ITYP5)
ITYP4:  DW DROP
ITYP5:  DW EXIT

;Z (IS")     -- c-addr u   run-time code for S"
;   R> ICOUNT 2DUP + ALIGNED >R  ;
; Harvard model, for string stored in Code space
; e.g. as used by ."
    ; HEADER(XISQUOTE,5,"(IS\")",DOCOLON)
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      5,"(IS",'"',')'
        .align 16
XISQUOTE: DW      DOCOLON
        DW RFROM,ICOUNT,TWODUP,PLUS,ALIGNED,TOR
        DW EXIT

;Z (S")     -- c-addr u   run-time code for S"
;   R@ I@                     get Data address
;   R> CELL+ DUP IC@ CHAR+    -- Dadr Radr+2 n+1
;   2DUP + ALIGNED >R         -- Dadr Iadr n+1
;   >R OVER R> I->D           -- Dadr
;   COUNT ;
; Harvard model, for string stored in Code space
; which is copied to Data space.
    ; HEADER(XSQUOTE,4,"(S")",DOCOLON)
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      4,"(S",'"',')'
        .align 16
XSQUOTE: DW      DOCOLON
        DW RFETCH,IFETCH
        DW RFROM,CELLPLUS,DUP,ICFETCH,CHARPLUS
        DW TWODUP,PLUS,ALIGNED,TOR
        DW TOR,OVER,RFROM,ITOD,COUNT,EXIT

;Z IS"      --            compile in-line string
;   COMPILE (IS")  [ HEX ]
;   22 PARSE     ( -- c-addr n ) 
;   DUP >R IC,  IHERE R@ D->I
;   R> IALLOT ALIGN ; IMMEDIATE
; Harvard model: string is stored in Code space
    ; IMMED(ISQUOTE,3,"IS"",DOCOLON)
        DW      link
        DB      0FEh       ; immediate
.set link = $
        DB      3,"IS",'"'
        .align 16
ISQUOTE: DW      DOCOLON
        DW lit,XISQUOTE,COMMAXT
        DW lit,22h,PARSE
        DW DUP,TOR,ICCOMMA,IHERE,RFETCH,DTOI
        DW RFROM,IALLOT,ALIGNN,EXIT

;Z IS"      --            compile in-line string  OLD DEF'N
;   COMPILE (IS")  [ HEX ]
;   22 IWORD
;   IC@ 1+ ALIGNED IALLOT ; IMMEDIATE
; Harvard model: string is stored in Code space
;   IMMED  ISQUOTE,3,"IS"',DOCOLON
;       DW lit,XISQUOTE,COMMAXT
;       DW lit,22H,IWORD
;       DW ICFETCH,ONEPLUS,ALIGNED,IALLOT,EXIT

;C S"       --             compile in-line string
;   COMPILE (S")  [ HEX ]
;   HERE I,                     data address
;   22 PARSE     ( -- c-addr n ) 
;   DUP >R IC,  IHERE R@ D->I
;   R@ 1+ ALLOT                 reserve RAM space
;   R> IALLOT ALIGN ; IMMEDIATE
; Harvard model: string is stored in Code space
    ; IMMED(SQUOTE,2,"S"",DOCOLON)
        DW      link
        DB      0FEh       ; immediate
.set link = $
        DB      2,'S','"'
        .align 16
SQUOTE: DW      DOCOLON
        DW lit,XSQUOTE,COMMAXT
        DW HERE,ICOMMA,lit,22h,PARSE
        DW DUP,TOR,ICCOMMA,IHERE,RFETCH,DTOI
        DW RFETCH,ONEPLUS,ALLOT
        DW RFROM,IALLOT,ALIGNN,EXIT

;C S"       --             compile in-line string  OLD DEF'N
;   COMPILE (S")  [ HEX ]
;   HERE I,                     data address
;   22 IWORD
;   IC@ 1+ ALIGNED
;   DUP ALLOT IALLOT ; IMMEDIATE
; Harvard model: string is stored in Code space
;    IMMED  SQUOTE,2,'S"',DOCOLON
;        DW lit,XSQUOTE,COMMAXT
;        DW HERE,ICOMMA,lit,22H,IWORD
;        DW ICFETCH,ONEPLUS,ALIGNED
;        DW DUP,ALLOT,IALLOT,EXIT

;C ."       --         compile string to print
;   POSTPONE IS"  POSTPONE ITYPE ; IMMEDIATE
    ; IMMED(DOTQUOTE,2,"."",DOCOLON)
        DW      link
        DB      0FEh       ; immediate
.set link = $
        DB      2,'.','"'
        .align 16
DOTQUOTE: DW      DOCOLON
        DW ISQUOTE
        DW lit,ITYPE,COMMAXT
        DW EXIT

;Z IWORD     c -- c-addr       WORD to Code space
;   WORD
;   IHERE TUCK OVER C@ CHAR+ D->I ;
    HEADER(IWORD,5,"IWORD",DOCOLON)
;       DW WORDD,IHERE,TUCK,OVER,CFETCH            ;mk
        DW WORDD,CAPITALIZE,IHERE,TUCK,OVER,CFETCH ;mk
        DW CHARPLUS,DTOI,EXIT

; SEPARATE HEADER EXTENSIONS ARE NOT USED
#define HCOUNT ICOUNT
#define HTYPE  ITYPE
#define HWORD  IWORD

; NUMERIC OUTPUT ================================
; Numeric conversion is done l.s.digit first, so
; the output buffer is built backwards in memory.

; Some double-precision arithmetic operators are
; needed to implement ANSI numeric conversion.

;Z UD/MOD   ud1 u2 -- u3 ud4   32/16->32 divide
;   >R 0 R@ UM/MOD  ROT ROT R> UM/MOD ROT ;
    HEADER(UDSLASHMOD,6,"UD/MOD",DOCOLON)
        DW TOR,lit,0,RFETCH,UMSLASHMOD,ROT,ROT
        DW RFROM,UMSLASHMOD,ROT,EXIT

;Z UD*      ud1 d2 -- ud3      32*16->32 multiply
;   DUP >R UM* DROP  SWAP R> UM* ROT + ;
    HEADER(UDSTAR,3,"UD*",DOCOLON)
        DW DUP,TOR,UMSTAR,DROP
        DW SWAP,RFROM,UMSTAR,ROT,PLUS,EXIT

;C HOLD  char --        add char to output string
;   -1 HP +!  HP @ C! ;
    HEADER(HOLD,4,"HOLD",DOCOLON)
        DW lit,-1,HP,PLUSSTORE
        DW HP,FETCH,CSTORE,EXIT

;C <#    --             begin numeric conversion
;   PAD HP ! ;          (initialize Hold Pointer)
    HEADER(LESSNUM,2,"<#",DOCOLON)
        DW PAD,HP,STORE,EXIT

;Z >digit   n -- c      convert to 0..9A..Z
;   [ HEX ] DUP 9 > 7 AND + 30 + ;
    HEADER(TODIGIT,6,">DIGIT",DOCOLON)
        DW DUP,lit,9,GREATER,lit,7,ANDD,PLUS
        DW lit,30h,PLUS,EXIT

;C #     ud1 -- ud2     convert 1 digit of output
;   BASE @ UD/MOD ROT >digit HOLD ;
    HEADER(NUM,1,"#",DOCOLON)
        DW BASE,FETCH,UDSLASHMOD,ROT,TODIGIT
        DW HOLD,EXIT

;C #S    ud1 -- ud2     convert remaining digits
;   BEGIN # 2DUP OR 0= UNTIL ;
    HEADER(NUMS,2,"#S",DOCOLON)
NUMS1:  DW NUM,TWODUP,ORR,ZEROEQUAL,qbran
        DEST(NUMS1)
        DW EXIT

;C #>    ud1 -- c-addr u    end conv., get string
;   2DROP HP @ PAD OVER - ;
    HEADER(NUMGREATER,2,"#>",DOCOLON)
        DW TWODROP,HP,FETCH,PAD,OVER,MINUS,EXIT

;C SIGN  n --           add minus sign if n<0
;   0< IF 2D HOLD THEN ;
    HEADER(SIGN,4,"SIGN",DOCOLON)
        DW ZEROLESS,qbran
        DEST(SIGN1)
        DW lit,2Dh,HOLD
SIGN1:  DW EXIT

;C U.    u --           display u unsigned
;   <# 0 #S #> TYPE SPACE ;
    HEADER(UDOT,2,"U.",DOCOLON)
        DW LESSNUM,lit,0,NUMS,NUMGREATER,TYP
        DW SPACE,EXIT

;C .     n --           display n signed
;   <# DUP ABS 0 #S ROT SIGN #> TYPE SPACE ;
    HEADER(DOT,1,".",DOCOLON)
        DW LESSNUM,DUP,ABBS,lit,0,NUMS
        DW ROT,SIGN,NUMGREATER,TYP,SPACE,EXIT

;C DECIMAL  --      set number base to decimal
;   10 BASE ! ;
    HEADER(DECIMAL,7,"DECIMAL",DOCOLON)
        DW lit,10,BASE,STORE,EXIT

;X HEX     --       set number base to hex
;   16 BASE ! ;
    HEADER(HEX,3,"HEX",DOCOLON)
        DW lit,16,BASE,STORE,EXIT

; DICTIONARY MANAGEMENT =========================

;C HERE    -- addr      returns dictionary ptr
;   DP @ ;
    HEADER(HERE,4,"HERE",DOCOLON)
        DW DDP,FETCH,EXIT

;C ALLOT   n --         allocate n bytes in dict
;   DP +! ;
    HEADER(ALLOT,5,"ALLOT",DOCOLON)
        DW DDP,PLUSSTORE,EXIT

;C ,    x --           append cell to dict
;   HERE ! 1 CELLS ALLOT ;
    HEADER(COMMA,1,02Ch,DOCOLON)
        DW HERE,STORE,lit,1,CELLS,ALLOT,EXIT

;C C,   char --        append char to dict
;   HERE C! 1 CHARS ALLOT ;
    ; HEADER(CCOMMA,2,"C,",DOCOLON)
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      2,"C,"
        .align 16
CCOMMA: DW      DOCOLON
        DW HERE,CSTORE,lit,1,CHARS,ALLOT,EXIT

; The following additional words support the
; "Harvard" model, with separate address spaces
; for Instructions (Code) and Data.  ANSI
; requires DP to manage the Data space, so a
; separate Instruction Dictionary Pointer, IDP,
; is added to manage the Code space.  Also added:
;   I@ IC@ I! IC! I->D D->I   (in the primitives)
;   ITYPE ICOUNT IWORD        (above)
;   IHERE IALLOT I, IC,       (below)
; It should be possible to convert the Harvard
; implementation to a combined-code-and-data
; system, by equating these words to their
; Data-space counterparts.

;C IHERE    -- addr   returns Code dictionary ptr
;   IDP @ ;
    HEADER(IHERE,5,"IHERE",DOCOLON)
        DW IDP,FETCH,EXIT

;C IALLOT   n --    allocate n bytes in Code dict
;   IDP +! ;
    HEADER(IALLOT,6,"IALLOT",DOCOLON)
        DW IDP,PLUSSTORE,EXIT

;C I,    x --           append cell to Code dict
;   IHERE I! 1 CELLS IALLOT ;
    ; HEADER(ICOMMA,2,"I,",DOCOLON)
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      2,"I,"
        .align 16
ICOMMA: DW      DOCOLON
        DW IHERE,ISTORE,lit,1,CELLS,IALLOT,EXIT

;C IC,   char --        append char to Code dict
;   IHERE IC! 1 CHARS IALLOT ;
    ; HEADER(ICCOMMA,3,"IC,",DOCOLON)
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      3,"IC,"
        .align 16
ICCOMMA: DW      DOCOLON
        DW IHERE,ICSTORE,lit,1,CHARS,IALLOT,EXIT

; SEPARATE HEADER EXTENSIONS ARE NOT USED
#define HHERE IHERE
#define HALLOT IALLOT
#define HCOMMA ICOMMA
#define HCCOMMA ICCOMMA
#define HCFETCH ICFETCH
#define HFETCH IFETCH
#define HCSTORE ICSTORE
#define HSTORE ISTORE

; INTERPRETER ===================================
; Note that NFA>LFA, NFA>CFA, IMMED?, and FIND
; are dependent on the structure of the Forth
; header.  This may be common across many CPUs,
; or it may be different.

;mk -----------------------------------------------------------------
; ?STACK   --               check stack underflow 
; depth 0< abort" SUF" ;  
   HEADER(QSTACK,6,"?STACK",DOCOLON)
        DW DEPTH,ZEROLESS,XISQUOTE
        DB 3,"SUF"
        DW QABORT,EXIT
;mk \----------------------------------------------------------------

;C SOURCE   -- adr n    current input buffer
;   'SOURCE 2@ ;        length is at lower adrs
    HEADER(SOURCE,6,"SOURCE",DOCOLON)
        DW TICKSOURCE,TWOFETCH,EXIT

;X /STRING  a u n -- a+n u-n   trim string
;   ROT OVER + ROT ROT - ;
    HEADER(SLASHSTRING,7,"/STRING",DOCOLON)
        DW ROT,OVER,PLUS,ROT,ROT,MINUS,EXIT

;Z >counted  src n dst --     copy to counted str
;   2DUP C! CHAR+ SWAP CMOVE ;
    HEADER(TOCOUNTED,8,">COUNTED",DOCOLON)
        DW TWODUP,CSTORE,CHARPLUS,SWAP,CMOVE,EXIT

;Z ADR>IN   c-addr' --    set >IN to offset to given adr
;   SOURCE         -- adr' adr n
;   ROT ROT -      -- n adr'-adr
;   MIN  0 MAX     -- n'
;   >IN ! ;
    HEADER(ADRTOIN,6,"ADR>IN",DOCOLON)
        DW SOURCE,ROT,ROT,MINUS,MIN,lit,0,MAX
        DW TOIN,STORE,EXIT

;X PARSE   char -- c-addr n    word delim'd by char
;   SOURCE >IN @ /STRING        -- c adr n
;   OVER >R                     save adr of string start
;   ROT SCAN                    -- adr" n"
;   OVER SWAP IF CHAR+ THEN    skip trailing delim. if any
;   ADR>IN                     advance >IN   -- adr"
;   R> TUCK - ;                 -- adr n'
    HEADER(PARSE,5,"PARSE",DOCOLON)
        DW SOURCE,TOIN,FETCH,SLASHSTRING
        DW OVER,TOR,ROT,SCAN
        DW OVER,SWAP,qbran
        DEST(PARSE1)
        DW ONEPLUS  ; char+
PARSE1: DW ADRTOIN
        DW RFROM,TUCK,MINUS,EXIT

;C WORD   char -- c-addr    word delim'd by char
;   DUP  SOURCE >IN @ /STRING   -- c c adr n
;   ROT SKIP                    -- c adr' n'
;   DROP ADR>IN PARSE           -- adr" n"
;   HERE >counted               --
;   HERE                        -- a
;   BL OVER COUNT + C! ;    append trailing blank
    HEADER(WORDD,4,"WORD",DOCOLON)
        DW DUP,SOURCE,TOIN,FETCH,SLASHSTRING
        DW ROT,SKIP
        DW DROP,ADRTOIN,PARSE
        DW HERE,TOCOUNTED,HERE
        DW BLANK,OVER,COUNT,PLUS,CSTORE,EXIT
    
;C WORD   char -- c-addr    word delim'd by char  OLD DEF'N
;   DUP  SOURCE >IN @ /STRING   -- c c adr n
;   DUP >R   ROT SKIP           -- c adr' n'
;   OVER >R  ROT SCAN           -- adr" n"
;   DUP IF CHAR- THEN        skip trailing delim.
;   R> R> ROT -   >IN +!        update >IN offset
;   TUCK -                      -- adr' N
;   HERE >counted               --
;   HERE                        -- a
;   BL OVER COUNT + C! ;    append trailing blank
;    HEADER  WORDD,4,'WORD',DOCOLON
;        DW DUP,SOURCE,TOIN,FETCH,SLASHSTRING
;        DW DUP,TOR,ROT,SKIP
;        DW OVER,TOR,ROT,SCAN
;        DW DUP,qbran
;        DEST  WORD1
;        DW ONEMINUS  ; char-
;WORD1:  DW RFROM,RFROM,ROT,MINUS,TOIN,PLUSSTORE
;        DW TUCK,MINUS
;        DW HERE,TOCOUNTED,HERE
;        DW BLANK,OVER,COUNT,PLUS,CSTORE,EXIT

;Z NFA>LFA   nfa -- lfa    name adr -> link field
;   3 - ;
    HEADER(NFATOLFA,7,"NFA>LFA",DOCOLON)
        DW lit,3,MINUS,EXIT

;Z NFA>CFA   nfa -- cfa    name adr -> code field
;   HCOUNT 7F AND + ALIGNED ;   mask off 'smudge' bit
    HEADER(NFATOCFA,7,"NFA>CFA",DOCOLON)
        DW HCOUNT
        DW lit,07Fh,ANDD,PLUS,ALIGNED,EXIT

;Z IMMED?    nfa -- f      fetch immediate flag
;   1- HC@ 1 AND 0= ;   Flashable model, LSB=0 if immed
    HEADER(IMMEDQ,6,"IMMED?",DOCOLON)
        DW ONEMINUS,HCFETCH,lit,1,ANDD,ZEROEQUAL,EXIT

;C FIND   c-addr -- c-addr 0   if not found
;C                  xt  1      if immediate
;C                  xt -1      if "normal"
;   LATEST @ BEGIN             -- a nfa
;       2DUP OVER C@ CHAR+     -- a nfa a nfa n+1
;       N=                     -- a nfa f
;       DUP IF
;           DROP
;           NFA>LFA H@ DUP     -- a link link
;       THEN
;   0= UNTIL                   -- a nfa  OR  a 0
;   DUP IF
;       NIP DUP NFA>CFA        -- nfa xt
;       SWAP IMMED?            -- xt iflag
;       0= 1 OR                -- xt 1/-1
;   THEN ;
    HEADER(FIND,4,"FIND",DOCOLON)
        DW LATEST,FETCH
FIND1:  DW TWODUP,OVER,CFETCH,CHARPLUS
        DW NEQUAL,DUP,qbran
        DEST(FIND2)
        DW DROP,NFATOLFA,HFETCH,DUP
FIND2:  DW ZEROEQUAL,qbran
        DEST(FIND1)
        DW DUP,qbran
        DEST(FIND3)
        DW NIP,DUP,NFATOCFA
        DW SWAP,IMMEDQ,ZEROEQUAL,lit,1,ORR
FIND3:  DW EXIT

;mk -----------------------------------------------------------------
;C  UPC   char -- char        capitalize character
;   DUP [CHAR] a < OVER [CHAR] z > OR IF EXIT THEN  
;   [ CHAR A CHAR a - ] LITERAL + ; 
;   HEADER  UPC,3,'UPC',DOCOLON
    HEADLESS(UPC,DOCOLON)
        DW DUP,lit,"a",LESS,OVER,lit,"z",GREATER
        DW ORR,qbran
        DEST(UPC1)
        DW EXIT
UPC1:   DW lit,"A"-"a",PLUS
        DW EXIT

;C  CAPITALIZE     c-addr -- c-addr     capitalize string
;   DUP COUNT OVER + SWAP ?DO  I c@ upc I c! LOOP  ; 
;   HEADER CAPITALIZE, 10, 'CAPITALIZE', DOCOLON
    HEADLESS(CAPITALIZE,DOCOLON)
;        DW CAPS,FETCH,qbran
;        DEST(CAPS2)
        DW DUP,COUNT,OVER,PLUS,SWAP,xdo
CAPS1:  DW II,CFETCH,UPC,II,CSTORE
        DW xloop
        DEST(CAPS1)
CAPS2:  DW EXIT
;mk \ ---------------------------------------------------------------



;C LITERAL  x --        append numeric literal
;   STATE @ IF ['] LIT ,XT I, THEN ; IMMEDIATE
; This tests STATE so that it can also be used
; interpretively.  (ANSI doesn't require this.)
    IMMED(LITERAL,7,"LITERAL",DOCOLON)
        DW STATE,FETCH,qbran
        DEST(LITER1)
        DW lit,lit,COMMAXT,ICOMMA
LITER1: DW EXIT

;Z DIGIT?   c -- n -1   if c is a valid digit
;Z            -- x  0   otherwise
;   [ HEX ] DUP 39 > 100 AND +     silly looking
;   DUP 140 > 107 AND -   30 -     but it works!
;   DUP BASE @ U< ;
    HEADER(DIGITQ,6,"DIGIT?",DOCOLON)
        DW DUP,lit,39h,GREATER,lit,100h,ANDD,PLUS
        DW DUP,lit,140h,GREATER,lit,107h,ANDD
        DW MINUS,lit,30h,MINUS
        DW DUP,BASE,FETCH,ULESS,EXIT

;Z ?SIGN   adr n -- adr' n' f  get optional sign
;Z  advance adr/n if sign; return NZ if negative
;   OVER C@                 -- adr n c
;   2C - DUP ABS 1 = AND    -- +=-1, -=+1, else 0
;   DUP IF 1+               -- +=0, -=+2
;       >R 1 /STRING R>     -- adr' n' f
;   THEN ;
    HEADER(QSIGN,5,"?SIGN",DOCOLON)
        DW OVER,CFETCH,lit,2Ch,MINUS,DUP,ABBS
        DW lit,1,EQUAL,ANDD,DUP,qbran
        DEST(QSIGN1)
        DW ONEPLUS,TOR,lit,1,SLASHSTRING,RFROM
QSIGN1: DW EXIT

;C >NUMBER  ud adr u -- ud' adr' u'
;C                      convert string to number
;   BEGIN
;   DUP WHILE
;       OVER C@ DIGIT?
;       0= IF DROP EXIT THEN
;       >R 2SWAP BASE @ UD*
;       R> M+ 2SWAP
;       1 /STRING
;   REPEAT ;
    HEADER(TONUMBER,7,">NUMBER",DOCOLON)
TONUM1: DW DUP,qbran
        DEST(TONUM3)
        DW OVER,CFETCH,DIGITQ
        DW ZEROEQUAL,qbran
        DEST(TONUM2)
        DW DROP,EXIT
TONUM2: DW TOR,TWOSWAP,BASE,FETCH,UDSTAR
        DW RFROM,MPLUS,TWOSWAP
        DW lit,1,SLASHSTRING,bran
        DEST(TONUM1)
TONUM3: DW EXIT

;Z ?NUMBER  c-addr -- n -1      string->number
;Z                 -- c-addr 0  if convert error
;   DUP  0 0 ROT COUNT      -- ca ud adr n
;   ?SIGN >R  >NUMBER       -- ca ud adr' n'
;   IF   R> 2DROP 2DROP 0   -- ca 0   (error)
;   ELSE 2DROP NIP R>
;       IF NEGATE THEN  -1  -- n -1   (ok)
;   THEN ;
    HEADER(QNUMBER,7,"?NUMBER",DOCOLON)
        DW DUP,lit,0,DUP,ROT,COUNT
        DW QSIGN,TOR,TONUMBER,qbran
        DEST(QNUM1)
        DW RFROM,TWODROP,TWODROP,lit,0
        DW bran
        DEST(QNUM3)
QNUM1:  DW TWODROP,NIP,RFROM,qbran
        DEST(QNUM2)
        DW NEGATE
QNUM2:  DW lit,-1
QNUM3:  DW EXIT

;Z INTERPRET    i*x c-addr u -- j*x
;Z                      interpret given buffer
; This is a common factor of EVALUATE and QUIT.
; ref. dpANS-6, 3.4 The Forth Text Interpreter
;   'SOURCE 2!  0 >IN !
;   BEGIN
;   BL WORD DUP C@ WHILE        -- textadr
;       FIND                    -- a 0/1/-1
;       ?DUP IF                 -- xt 1/-1
;           1+ STATE @ 0= OR    IMMED  or interp?
;           IF EXECUTE ELSE ,XT THEN
;       ELSE                    -- textadr
;           ?NUMBER
;           IF POSTPONE LITERAL     converted ok
;           ELSE COUNT TYPE 3F EMIT CR ABORT  err
;           THEN
;       THEN
;   REPEAT DROP ;
    HEADER(INTERPRET,9,"INTERPRET",DOCOLON)
        DW TICKSOURCE,TWOSTORE,lit,0,TOIN,STORE
INTER1: DW QSTACK   ;mk   check stack underflow.
        DW BLANK,WORDD,DUP,CFETCH,qbran
        DEST(INTER9)
        DW CAPITALIZE  ;mk
        DW FIND,QDUP,qbran
        DEST(INTER4)
        DW ONEPLUS,STATE,FETCH,ZEROEQUAL,ORR
        DW qbran
        DEST(INTER2)
        DW EXECUTE,bran
        DEST(INTER3)
INTER2: DW COMMAXT
INTER3: DW bran
        DEST(INTER8)
INTER4: DW QNUMBER,qbran
        DEST(INTER5)
        DW LITERAL,bran
        DEST(INTER6)
INTER5: DW COUNT,TYP,lit,3Fh,EMIT,CR,ABORT
INTER6:
INTER8: DW bran
        DEST(INTER1)
INTER9: DW DROP,EXIT

;C EVALUATE  i*x c-addr u -- j*x  interprt string
;   'SOURCE 2@ >R >R  >IN @ >R
;   INTERPRET
;   R> >IN !  R> R> 'SOURCE 2! ;
    HEADER(EVALUATE,8,"EVALUATE",DOCOLON)
        DW TICKSOURCE,TWOFETCH,TOR,TOR
        DW TOIN,FETCH,TOR,INTERPRET
        DW RFROM,TOIN,STORE,RFROM,RFROM
        DW TICKSOURCE,TWOSTORE,EXIT

;Z ?EMIT   c --     send character if not null
;   ?DUP IF EMIT THEN ;
    HEADLESS(QEMIT,DOCOLON)
        DW QDUP,qbran
        DEST(QEMIT1)
        DW EMIT
QEMIT1: DW EXIT        

;C QUIT     --    R: i*x --    interpret from kbd
;   L0 LP !  R0 RP!   0 STATE !
;   BEGIN
;       XON/OFF C@ ?EMIT        ; send XON if any
;       TIB DUP TIBSIZE ACCEPT
;       XON/OFF CHAR+ C@ ?EMIT  ; send XOFF if any
;       SPACE
;       INTERPRET
;       CR STATE @ 0= IF ." OK" THEN
;   AGAIN ;
    HEADER(QUIT,4,"QUIT",DOCOLON)
        DW L0,LP,STORE
        DW RZERO,RPSTORE,lit,0,STATE,STORE
QUIT1:  DW XONOFF,CFETCH,QEMIT          ; send XON
        DW CR
;        DW XISQUOTE  ; uncomment for prefix prompting
;        DB 3,"OK "
;        DW ITYPE
        DW TIB,DUP,TIBSIZE,ACCEPT
        DW XONOFF,CHARPLUS,CFETCH,QEMIT ; send XOFF
        DW SPACE
        DW INTERPRET
        DW STATE,FETCH,ZEROEQUAL,qbran ;mk (no CR here)
        DEST(QUIT2)
        DW lit,06H,EMIT         ;mk  ACK
        DW BASE,FETCH,HEX       ;mk  save BASE, set HEX  
        DW lit,'$',EMIT         ;mk  print $
        ; DUP <# 0 # # #> TYPE  ;mk  type BASE in hex
        DW DUP,LESSNUM,lit,0,NUM,NUM,NUMGREATER,TYP   ;mk 
        DW BASE,STORE           ;mk  restore BASE
        DW XISQUOTE
        DB 2,"ok"
        .align 16
        DW ITYPE                ; print "ok"
        ;mk depth 0 > if depth 0 do [char] . emit loop then \ noForth style :-)
        DW DEPTH,ZERO,GREATER,qbran ;mk
        DEST(QUIT2)                 ;mk
        DW DEPTH,ZERO,xdo           ;mk
QUIT11: DW lit,".",EMIT             ;mk pint a dot for each item on stack.
        DW xloop                    ;mk
        DEST(QUIT11)                ;mk
QUIT2:  DW bran
        DEST(QUIT1)

;C ABORT    i*x --   R: j*x --   clear stk & QUIT
;   S0 SP!  QUIT ;
    HEADER(ABORT,5,"ABORT",DOCOLON)
        DW S0,SPSTORE,QUIT   ; QUIT never returns

;Z ?ABORT   f c-addr u --      abort & print msg
;   ROT IF ITYPE ABORT THEN 2DROP ;
    HEADER(QABORT,6,"?ABORT",DOCOLON)
        DW ROT,qbran
        DEST(QABO1)
        DW ITYPE,ABORT
QABO1:  DW TWODROP,EXIT

;C ABORT"  i*x 0  -- i*x   R: j*x -- j*x  x1=0
;C         i*x x1 --       R: j*x --      x1<>0
;   POSTPONE IS" POSTPONE ?ABORT ; IMMEDIATE
    ; IMMED(ABORTQUOTE,6,"ABORT"",DOCOLON)
        DW      link
        DB      0FEh       ; immediate
.set link = $
        DB      6,"ABORT",'"'
        .align 16
ABORTQUOTE: DW      DOCOLON
        DW ISQUOTE
        DW lit,QABORT,COMMAXT
        DW EXIT

;C '    -- xt           find word in dictionary
;   BL WORD FIND
;   0= ABORT" ?" ;
    HEADER(TICK,1,27h,DOCOLON)
;       DW BLANK,WORDD,FIND,ZEROEQUAL,XISQUOTE            ;mk
        DW BLANK,WORDD,CAPITALIZE,FIND,ZEROEQUAL,XISQUOTE ;mk
        DB 1,'?'
        DW QABORT,EXIT

;C CHAR   -- char           parse ASCII character
;   BL WORD 1+ C@ ;
    HEADER(CHARR,4,"CHAR",DOCOLON)
        DW BLANK,WORDD,ONEPLUS,CFETCH,EXIT

;C [CHAR]   --          compile character literal
;   CHAR  ['] LIT ,XT  I, ; IMMEDIATE
    IMMED(BRACCHAR,6,"[CHAR]",DOCOLON)
        DW CHARR
        DW lit,lit,COMMAXT
        DW ICOMMA,EXIT

;C (    --                     skip input until )
;   [ HEX ] 29 PARSE 2DROP ; IMMEDIATE
    IMMED(PAREN,1,"(",DOCOLON)
        DW lit,29h,PARSE,TWODROP,EXIT

; COMPILER ======================================

;Z HEADER   --      create a Forth word header
;   LATEST @ H, 0FF HC,         link & IMMED  field
;   HHERE LATEST !            new "latest" link
;   BL HWORD HC@ 1+ HALLOT    name field
;   ALIGN ;
; Separate headers model.
    HEADER(HEADR,6,"HEADER",DOCOLON)
        DW LATEST,FETCH,HCOMMA      ; link
        DW lit,0FFh,HCCOMMA         ; immediate flag - see note below
        DW HHERE,LATEST,STORE
        DW BLANK,HWORD,HCFETCH,ONEPLUS,HALLOT
        DW ALIGNN,EXIT   ; MSP430: headers in I space must be aligned
; Note for Flashable MSP430: when compiling to RAM, we need to set
; the immediate byte to 0FFH.  When compiling to Flash, the word IC!
; will not write 0FFH to erased Flash (because the byte is already 0FFH).
; Thus we can write this byte at a later time (with IMMEDIATE).

;Z <BUILDS  --      define a word with t.b.d. action & no data
;   HEADER 2 IALLOT ;       Flashable: do not store Code Field
    HEADER(BUILDS,7,"<BUILDS",DOCOLON)
        DW HEADR,lit,2,IALLOT,EXIT

;C CREATE   --      create an empty definition
;   HEADER
;   docreate ,CF              code field
;   HERE I, ;           store data adr (Harvard)
; Harvard model, separate Code and Data spaces.
; Separate headers model.
    HEADER(CREATE,6,"CREATE",DOCOLON)
        DW HEADR
        DW lit,docreate,COMMACF
        DW HERE,ICOMMA,EXIT

;Z (DOES>)  --      run-time action of DOES>
;   R>              adrs of headless DOES> def'n
;   LATEST @ NFA>CFA    code field to fix up
;   !CF ;
    ; HEADER(XDOES,7,"(DOES>)",DOCOLON)
        DW      link
        DB      0FFh       ; not immediate
.set link = $
        DB      7,"(DOES>)"
        .align 16
XDOES: DW      DOCOLON
        DW RFROM,LATEST,FETCH,NFATOCFA,STORECF
        DW EXIT

;C DOES>    --      change action of latest def'n
;   COMPILE (DOES>)
;   dodoes ,JMP ; IMMEDIATE
; Note that MSP430 uses a JMP, not a CALL, to DODOES.
    IMMED(DOES,5,"DOES>",DOCOLON)
        DW lit,XDOES,COMMAXT
        DW lit,dodoes,COMMAJMP,EXIT

;C RECURSE  --      recurse current definition
;   LATEST @ NFA>CFA ,XT ; IMMEDIATE
;   NEWEST @ NFA>CFA ,XT ; IMMEDIATE   Flashable
    IMMED(RECURSE,7,"RECURSE",DOCOLON)
        DW NEWEST,FETCH,NFATOCFA,COMMAXT,EXIT

;C [        --      enter interpretive state
;   0 STATE ! ; IMMEDIATE
    IMMED(LEFTBRACKET,1,"[",DOCOLON)
        DW lit,0,STATE,STORE,EXIT

;C ]        --      enter compiling state
;   -1 STATE ! ;
    HEADER(RIGHTBRACKET,1,"]",DOCOLON)
        DW lit,-1,STATE,STORE,EXIT

;Z HIDE     --      "hide" latest definition    Flashable
;   LATEST @ DUP NEWEST !  NFA>LFA H@ LATEST ! ;
    HEADER(HIDE,4,"HIDE",DOCOLON)
        DW LATEST,FETCH,DUP,NEWEST,STORE
        DW NFATOLFA,HFETCH,LATEST,STORE,EXIT

;Z REVEAL   --      "reveal" latest definition  Flashable
;   NEWEST @ LATEST ! ;
    HEADER(REVEAL,6,"REVEAL",DOCOLON)
        DW NEWEST,FETCH,LATEST,STORE,EXIT

;C IMMEDIATE   --   make last def'n immediate
;   0FE LATEST @ 1- HC! ;   set Flashable immediate flag
    HEADER(IMMEDIATE,9,"IMMEDIATE",DOCOLON)
        DW lit,0FEh,LATEST,FETCH,ONEMINUS,HCSTORE
        DW EXIT

;C :        --      begin a colon definition  
;   <BUILDS HIDE ] !COLON ;      Flashable version
    HEADER(COLON,1,":",DOCOLON)
        DW BUILDS,HIDE,RIGHTBRACKET,STORCOLON
        DW EXIT

;C ;
;   REVEAL  ,EXIT
;   POSTPONE [  ; IMMEDIATE
    IMMED(SEMICOLON,1,";",DOCOLON)
        DW REVEAL,CEXIT
        DW LEFTBRACKET,EXIT

;C [']  --         find word & compile as literal
;   '  ['] LIT ,XT  I, ; IMMEDIATE
; When encountered in a colon definition, the
; phrase  ['] xxx  will cause   LIT,xxt  to be
; compiled into the colon definition (where
; (where xxt is the execution token of word xxx).
; When the colon definition executes, xxt will
; be put on the stack.  (All xt's are one cell.)
    IMMED(BRACTICK,3,"[']",DOCOLON)
        DW TICK               ; get xt of 'xxx'
        DW lit,lit,COMMAXT    ; append LIT action
        DW ICOMMA,EXIT        ; append xt literal

;C POSTPONE  --   postpone compile action of word
;   BL WORD FIND
;   DUP 0= ABORT" ?"
;   0< IF   -- xt  non immed: add code to current
;                  def'n to compile xt later.
;       ['] LIT ,XT  I,     add "LIT,xt,COMMAXT"
;       ['] ,XT ,XT         to current definition
;   ELSE  ,XT      immed: compile into cur. def'n
;   THEN ; IMMEDIATE
    IMMED(POSTPONE,8,"POSTPONE",DOCOLON)
        DW BLANK,WORDD,FIND,DUP,ZEROEQUAL,XISQUOTE
        DB 1,'?'
        DW QABORT,ZEROLESS,qbran
        DEST(POST1)
        DW lit,lit,COMMAXT,ICOMMA
        DW lit,COMMAXT,COMMAXT,bran
        DEST(POST2)
POST1:  DW COMMAXT
POST2:  DW EXIT

;Z COMPILE   --   append inline execution token
;   R> DUP CELL+ >R @ ,XT ;
; The phrase ['] xxx ,XT appears so often that
; this word was created to combine the actions
; of LIT and ,XT.  It takes an inline literal
; execution token and appends it to the dict.
;    HEADER  COMPILE,7,'COMPILE',DOCOLON
;        DW RFROM,DUP,CELLPLUS,TOR
;        DW FETCH,COMMAXT,EXIT
; N.B.: not used in the current implementation

; CONTROL STRUCTURES ============================

;C IF       -- adrs    conditional forward branch
;   ['] qbran ,BRANCH  IHERE ,NONE ;      Flashable
;   IMMEDIATE
    IMMED(IFF,2,"IF",DOCOLON)
        DW lit,qbran,COMMABRANCH
        DW IHERE,COMMANONE,EXIT

;C THEN     adrs --        resolve forward branch
;   IHERE SWAP !DEST ; IMMEDIATE
    IMMED(THEN,4,"THEN",DOCOLON)
        DW IHERE,SWAP,STOREDEST,EXIT

;C ELSE     adrs1 -- adrs2    branch for IF..ELSE
;   ['] branch ,BRANCH  IHERE ,NONE       Flashable
;   SWAP  POSTPONE THEN ; IMMEDIATE
    IMMED(ELSS,4,"ELSE",DOCOLON)
        DW lit,bran,COMMABRANCH
        DW IHERE,COMMANONE
        DW SWAP,THEN,EXIT

;C BEGIN    -- adrs        target for bwd. branch
;   IHERE ; IMMEDIATE
    IMMED(BEGIN,5,"BEGIN",DOCOLON)
        DW IHERE,EXIT

;C UNTIL    adrs --   conditional backward branch
;   ['] qbran ,BRANCH  ,DEST ; IMMEDIATE
;   conditional backward branch
    IMMED(UNTIL,5,"UNTIL",DOCOLON)
        DW lit,qbran,COMMABRANCH
        DW COMMADEST,EXIT

;X AGAIN    adrs --      uncond'l backward branch
;   ['] branch ,BRANCH  ,DEST ; IMMEDIATE
;   unconditional backward branch
    IMMED(AGAIN,5,"AGAIN",DOCOLON)
        DW lit,bran,COMMABRANCH
        DW COMMADEST,EXIT

;C WHILE    adrs1 -- adrs2 adrs1
;                           branch for WHILE loop
;   POSTPONE IF SWAP ; IMMEDIATE
    IMMED(WHILE,5,"WHILE",DOCOLON)
        DW IFF,SWAP,EXIT

;C REPEAT   adrs2 adrs1 --     resolve WHILE loop
;   POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE
    IMMED(REPEAT,6,"REPEAT",DOCOLON)
        DW AGAIN,THEN,EXIT

;Z >L   x --   L: -- x        move to leave stack
;   CELL LP +!  LP @ ! ;      (L stack grows up)
    HEADER(TOL,2,">L",DOCOLON)
        DW CELL,LP,PLUSSTORE,LP,FETCH,STORE,EXIT

;Z L>   -- x   L: x --      move from leave stack
;   LP @ @  CELL NEGATE LP +! ;
    HEADER(LFROM,2,"L>",DOCOLON)
        DW LP,FETCH,FETCH
        DW CELL,NEGATE,LP,PLUSSTORE,EXIT

;C DO       -- adrs   L: -- 0
;   ['] xdo ,XT   IHERE     target for bwd branch
;   0 >L ; IMMEDIATE           marker for LEAVEs
    IMMED(DO,2,"DO",DOCOLON)
        DW lit,xdo,COMMAXT,IHERE
        DW lit,0,TOL,EXIT

;Z ENDLOOP   adrs xt --   L: 0 a1 a2 .. aN --
;   ,BRANCH  ,DEST                backward loop
;   BEGIN L> ?DUP WHILE POSTPONE THEN REPEAT ;
;                                 resolve LEAVEs
; This is a common factor of LOOP and +LOOP.
    HEADER(ENDLOOP,7,"ENDLOOP",DOCOLON)
        DW COMMABRANCH,COMMADEST
LOOP1:  DW LFROM,QDUP,qbran
        DEST(LOOP2)
        DW THEN,bran
        DEST(LOOP1)
LOOP2:  DW EXIT

;C LOOP    adrs --   L: 0 a1 a2 .. aN --
;   ['] xloop ENDLOOP ;  IMMEDIATE
    IMMED(LOO,4,"LOOP",DOCOLON)
        DW lit,xloop,ENDLOOP,EXIT

;C +LOOP   adrs --   L: 0 a1 a2 .. aN --
;   ['] xplusloop ENDLOOP ;  IMMEDIATE
    IMMED(PLUSLOOP,5,"+LOOP",DOCOLON)
        DW lit,xplusloop,ENDLOOP,EXIT

;C LEAVE    --    L: -- adrs
;   ['] UNLOOP ,XT
;   ['] branch ,BRANCH   IHERE ,NONE  >L
;   ; IMMEDIATE      unconditional forward branch
    IMMED(LEAV,5,"LEAVE",DOCOLON)
        DW lit,UNLOOP,COMMAXT
        DW lit,bran,COMMABRANCH
        DW IHERE,COMMANONE,TOL,EXIT

; OTHER OPERATIONS ==============================

;X WITHIN   n1|u1 n2|u2 n3|u3 -- f   n2<=n1<n3?
;  OVER - >R - R> U< ;          per ANS document
    HEADER(WITHIN,6,"WITHIN",DOCOLON)
        DW OVER,MINUS,TOR,MINUS,RFROM,ULESS,EXIT

;C MOVE    addr1 addr2 u --     smart move
;             VERSION FOR 1 ADDRESS UNIT = 1 CHAR
;  >R 2DUP SWAP DUP R@ +     -- ... dst src src+n
;  WITHIN IF  R> CMOVE>        src <= dst < src+n
;       ELSE  R> CMOVE  THEN ;          otherwise
    HEADER(MOVE,4,"MOVE",DOCOLON)
        DW TOR,TWODUP,SWAP,DUP,RFETCH,PLUS
        DW WITHIN,qbran
        DEST(MOVE1)
        DW RFROM,CMOVEUP,bran
        DEST(MOVE2)
MOVE1:  DW RFROM,CMOVE
MOVE2:  DW EXIT

;C DEPTH    -- +n        number of items on stack
;   SP@ S0 SWAP - 2/ ;   16-BIT VERSION!
    HEADER(DEPTH,5,"DEPTH",DOCOLON)
        DW SPFETCH,S0,SWAP,MINUS,TWOSLASH,EXIT

;C ENVIRONMENT?  c-addr u -- false   system query
;                         -- i*x true
;   2DROP 0 ;       the minimal definition!
    HEADER(ENVIRONMENTQ,12,"ENVIRONMENT?",DOCOLON)
        DW TWODROP,lit,0,EXIT

; UTILITY WORDS AND STARTUP =====================

;Z FLALIGNED   a -- a'      align IDP to flash boundary
;   $200 OVER - $1FF AND + ;
    HEADER(FLALIGNED,9,"FLALIGNED",DOCOLON)
        DW lit,0200h,OVER,MINUS,lit,01FFh,ANDD,PLUS,EXIT

;X MARKER   --      create word to restore dictionary
;   LATEST @ IHERE HERE
;   IHERE FLALIGNED IDP !     align new word to flash boundary
;   <BUILDS I, I, I,        save dp,idp,latest
;   DOES>  DUP I@  
;   SWAP CELL+ DUP I@
;   SWAP CELL+ I@           fetch saved   -- dp idp latest 
;   OVER FLALIGNED IHERE OVER - FLERASE    erase Flash from saved to IHERE
;   LATEST ! IDP ! DP ! ;
    HEADER(MARKER,6,"MARKER",DOCOLON)
        DW LATEST,FETCH,IHERE,HERE
        DW IHERE,FLALIGNED,IDP,STORE
        DW BUILDS,ICOMMA,ICOMMA,ICOMMA,XDOES
        MOV #dodoes,PC      ; long direct jump to DODOES
        DW DUP,IFETCH
        DW SWAP,CELLPLUS,DUP,IFETCH
        DW SWAP,CELLPLUS,IFETCH
        DW OVER,FLALIGNED,IHERE,OVER,MINUS,FLERASE
        DW LATEST,STORE,IDP,STORE,DDP,STORE,EXIT

;mk -----------------------------------------------------------------
;X WORDS    --          list all words in dict.
;   LATEST @ BEGIN
;       DUP HCOUNT 7F AND HTYPE SPACE
;       NFA>LFA H@
;   DUP 0= UNTIL
;   DROP ;
;    HEADER(WORDS,5,"WORDS",DOCOLON)
;        DW LATEST,FETCH
;WDS1:   DW DUP,HCOUNT,lit,07Fh,ANDD,HTYPE,SPACE
;        DW NFATOLFA,HFETCH
;        DW DUP,ZEROEQUAL,qbran
;        DEST(WDS1)
;        DW DROP,EXIT

;X   WORDS    --          list all words in dict. Stop and go feature. 
;   LATEST @ BEGIN
;       KEY? IF KEY DROP KEY 0x0D = IF DROP EXIT THEN THEN  
;       DUP HCOUNT 7F AND HTYPE SPACE
;       NFA>LFA H@
;   DUP 0= UNTIL
;   DROP ;
    HEADER(WORDS,5,"WORDS",DOCOLON)
        DW LATEST,FETCH
WDS0:   DW KEYQ,qbran
        DEST(WDS1)
        DW KEY,DROP ; halt
        DW KEY,BLANK,EQUAL,qbran ; do if blank, else quit WORDS
        DEST(WDS2)
WDS1:   DW DUP,HCOUNT,lit,07FH,ANDD,HTYPE,SPACE
        DW NFATOLFA,HFETCH
        DW DUP,ZEROEQUAL,qbran
        DEST(WDS0)
WDS2:   DW DROP,EXIT
;mk \----------------------------------------------------------------

;X U.R    u n --           display u unsigned in n width
;   >R  <# 0 #S #>  R> OVER - 0 MAX SPACES  TYPE ;
    HEADER(UDOTR,3,"U.R",DOCOLON)
        DW TOR,LESSNUM,lit,0,NUMS,NUMGREATER
        DW RFROM,OVER,MINUS,lit,0,MAX,SPACES,TYP,EXIT

;X DUMP  adr n  --   dump memory
;  OVER + SWAP DO
;    CR I 4 U.R SPACE SPACE
;    I $10 + I DO I C@ 3 U.R LOOP  SPACE SPACE
;    I $10 + I DO I C@ $7F AND $7E MIN BL MAX EMIT LOOP
;  10 +LOOP ;
    HEADER(DUMP,4,"DUMP",DOCOLON)
        DW OVER,PLUS,SWAP,xdo
LDUMP1: DW CR,II,lit,4,UDOTR,SPACE,SPACE
        DW II,lit,10h,PLUS,II,xdo
LDUMP2: DW II,CFETCH,lit,3,UDOTR,xloop
        DEST(LDUMP2)
        DW SPACE,SPACE
        DW II,lit,10h,PLUS,II,xdo
LDUMP3: DW II,CFETCH,lit,7Fh,ANDD,lit,7Eh,MIN,BLANK,MAX,EMIT,xloop
        DEST(LDUMP3)
        DW lit,10h,xplusloop
        DEST(LDUMP1)
        DW EXIT

;X .S      --           print stack contents
;   SP@ S0 - IF
;       SP@ S0 2 - DO I @ U. -2 +LOOP
;   THEN ;
    HEADER(DOTS,2,".S",DOCOLON)
        DW SPFETCH,S0,MINUS,qbran
        DEST(DOTS2)
        DW SPFETCH,S0,lit,2,MINUS,xdo
DOTS1:  DW II,FETCH,UDOT,lit,-2,xplusloop
        DEST(DOTS1)
DOTS2:  DW EXIT

;mk -----------------------------------------------------------------
;U   BELL     --                send $07 to Terminal  ;mk
     HEADER(BELL,4,"BELL",DOCOLON)
         DW lit,07h,EMIT,EXIT

;Z   ESC[     --                start esc-sequence  ;mk
; 27 emit 91 emit ;
     HEADLESS(ESCPAR,DOCOLON)
         DW lit,27,EMIT,lit,91,EMIT
         DW EXIT

;Z   PN      --                 send parameter of esc-sequence  ;mk
; base @  swap decimal 0 u.r  base ! ; 
     HEADLESS(PN,DOCOLON)
         DW BASE,FETCH
         DW SWAP,DECIMAL,ZERO,UDOTR
         DW BASE,STORE
         DW EXIT

;Z   ;PN    --                  send delimiter ; followed by parameter ;mk
; 59 emit pn ;
     HEADLESS(SEMIPN,DOCOLON)
         DW lit,59,EMIT,PN
         DW EXIT

;U   AT-XY   x y --          set cursor position in terminal ;mk
; 1+ swap 1+ swap ESC[ pn ;pn 72 emit ; 
     HEADER(ATXY,5,"AT-XY",DOCOLON)
         DW ONEPLUS,SWAP,ONEPLUS,SWAP
         DW ESCPAR,PN
         DW SEMIPN,lit,72,EMIT
         DW EXIT

;U   PAGE    --              send "page" command to terminal to clear screen. ;mk
; esc[  ." 2J" 0 0 at-xy ;
     HEADER(PAGEE,4,"PAGE",DOCOLON)
        DW ESCPAR
        DW XISQUOTE
        DB 2,"2J",0
        DW ITYPE
        DW ZERO,ZERO,ATXY
        DW EXIT
;mk \----------------------------------------------------------------

;mk -----------------------------------------------------------------
;X .VER     --      print version string.  ;mk
    HEADER(DOTVER,4,".VER",DOCOLON)
        DW XISQUOTE
        DB 21,"4e4th-0.5a 20180510",0dh,0ah
        DW ITYPE,EXIT
;mk \----------------------------------------------------------------

;mk -----------------------------------------------------------------
;U   \         --      backslash  ;mk
; everything up to the end of the current line is a comment. 
;   SOURCE >IN ! DROP ; 
;    IMMED(BACKSLASH,1,"\",DOCOLON)   ; geht so nicht, header manuell compilieren:
        DW      link
        DB      0FEh       ; immediate flag set
.set link = $
        DB      1
        DB      5Ch ;    [char] \
        .align 16
BACKSLASH: DW      DOCOLON
        DW SOURCE,TOIN,STORE,DROP,EXIT 
;mk \----------------------------------------------------------------

;mk -----------------------------------------------------------------
;S 1MS  --   wait about 1 millisecond  ;mk
;  xx 0 DO yy 0 DO LOOP LOOP ;  adjust xx and yy to get a msec.
     HEADER(ONEMS,3,"1MS",DOCOLON)
         DW lit,41,ZERO,xdo 
onems1:  DW lit,11,ZERO,xdo
onems2:  DW xloop
         DEST(onems2)
         DW xloop
         DEST(onems1)
         DW EXIT
;mk \----------------------------------------------------------------

;mk -----------------------------------------------------------------
;S MS  n --                wait about n milliseconds  ;mk
;  0 DO 1MS LOOP ;
     HEADER(MS,2,"MS",DOCOLON)
         DW ZERO,xdo
ms1:     DW ONEMS,xloop
         DEST(ms1)
         DW EXIT
;mk \----------------------------------------------------------------
         
;Z COLD     --      cold start Forth system
;   UINIT U0 #INIT I->D      init user area
;   .VER  ; mk
;   ABORT ;
    HEADER(COLD,4,"COLD",DOCOLON)
        DW UINIT,U0,NINIT,ITOD
        DW DOTVER
        DW GREEN,CSETB,RED,CSETB ;mk turn Launchpad LEDs on
        DW ABORT      ; ABORT never returns

; Note: the first character sent from the MSP430 seems to get
; scrambled.  I conjecture this is because the baud rate generator
; has not reset to the new rate when we attempt to send a character.
; See init430f1611.s43 for delay after initialization.
