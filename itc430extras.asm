; ----------------------------------------------------------------------
; CamelForth for the Texas Instruments MSP430 
; (c) 2009,2012,2014 Bradford J. Rodriguez.
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
; itc430extras.asm - utility words for the MSP430 family
; B. Rodriguez  27 oct 2012

;   Forth words are documented as follows:
;x   NAME     stack -- stack    description
;   where x=C for ANS Forth Core words, X for ANS
;   Extensions, Z for internal or private words.
; ----------------------------------------------------------------------
; REVISION HISTORY
; 10 May 18 mk - added CTOGGLE
;  1 mar 14 bjr - adapted from extras430.s43 for naken_asm.
; 22 dec 2013 bjr - added constants 0, 1, 2, -1
; 27 oct 2012 bjr - Created.


        CODEHEADER(E_INT,4,"EINT")
        EINT
        NEXT

        CODEHEADER(D_INT,4,"DINT")
        DINT
        NEXT

;Z SETB    u a-addr --       set bits in memory cell
        CODEHEADER(SETB,4,"SETB")
        BIS     @PSP+,0(TOS)
        MOV     @PSP+,TOS
        NEXT

;Z CLRB    u a-addr --       clear bits in memory cell
        CODEHEADER(CLRB,4,"CLRB")
        BIC     @PSP+,0(TOS)
        MOV     @PSP+,TOS
        NEXT
		
;Z TSTB    u a-addr -- u2	 test bits in memory cell
		CODEHEADER(	TSTB,4,"TSTB")
		MOV		@TOS,TOS
		AND		@PSP+,TOS
		NEXT
		
;Z CSETB	c c-addr -- 	set bits in memory byte
		CODEHEADER(CSETB,5,"CSETB")
		MOV		@PSP+,W
		BIS.B	W,0(TOS)
		MOV		@PSP+,TOS
		NEXT
		
;Z CCLRB	c c-addr -- 	clear bits in memory byte
		CODEHEADER(CCLRB,5,"CCLRB")
		MOV		@PSP+,W
		BIC.B	W,0(TOS)
		MOV		@PSP+,TOS
		NEXT
	
;Z CTSTB    c c-addr -- c2	 test bits in memory byte
		CODEHEADER(	CTSTB,5,"CTSTB")
		MOV.B	@TOS,TOS
		AND		@PSP+,TOS
		NEXT

;U CTOGGLE    mask addr --        flip bit from mask in addr (byte)  ;mk
    CODEHEADER(CTOGGLE,7,"CTOGGLE")
      XOR.B   @PSP,0(TOS)
      ADD     #2,PSP
      MOV     @PSP+,TOS
      NEXT

;Z 0    -- n        constant 0
    HEADER(ZERO,1,"0",DOCON)
        DW 0

;Z 1    -- n        constant 1
    HEADER(ONE,1,"1",DOCON)
        DW 1

;Z 2    -- n        constant 2
    HEADER(TWO,1,"2",DOCON)
        DW 2

;Z -1    -- n       constant -1
    HEADER(MINUSONE,2,"-1",DOCON)
        DW -1
