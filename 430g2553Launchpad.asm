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
;mk 430g2553Launchpad.asm - to use with MSP-EXP430G2 LaunchPad.
;
;   Forth words are documented as follows:
;x   NAME     stack -- stack    description
;   where x=C for ANS Forth Core words, X for ANS
;   Extensions, Z for internal or private words.
;   S for student words
; ----------------------------------------------------------------------
; REVISION HISTORY
; 12 may 18 - created.

; 
; TI document SLAU144I - December 2004 - Revised January 2012 
; The digital I/O registers are listed in Table 8-2. 

;S S2     -- mask port        second button mask and port address : input regster.
; Switch S2 : port.pin P1.3 --->0_0----GND
    HEADER(S2,2,"S2",DOTWOCON)
      DW P1IN
      DW 00001000b

;S S2?    -- f      Test button S2, true if pressed. 
    HEADER(SQEST,3,"S2?",DOCOLON)
        DW S2,CTSTB,ZEROEQUAL,EXIT

;S P1IN   -- adr    P1 input register address. 
    HEADER(p1in,4,"P1IN",DOCON)
      DW P1IN

;S P1OUT   -- adr    P1 output register address. 
    HEADER(p1out,5,"P1OUT",DOCON)
      DW P1OUT

;S RED      -- mask port         red LED mask and port out address
;  P1.0 - red LED
    HEADER(RED,3,"RED",DOTWOCON)
	  DW P1OUT
	  DW 00000001b

;S GREEN     -- mask port        green LED mask and port out address
;  P1.6 - green LED
    HEADER(GREEN,5,"GREEN",DOTWOCON)
      DW P1OUT
      DW 01000000b

; finis