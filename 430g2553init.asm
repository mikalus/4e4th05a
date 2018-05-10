; ----------------------------------------------------------------------
; CamelForth for the Texas Instruments MSP430 
; (c) 2009,2014 Bradford J. Rodriguez.
; modified for the MSP430G2553, 26 Oct 2012
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
; 430g2553init.asm: CPU Configuration - MSP430G2553
; B. Rodriguez  26 Oct 2012
; 
; Revision History
;  1 mar 2014 bjr - adapted for naken_asm from init430g2553.s43.
;       Memory map and RAM allocation moved to camel430g2553.asm.

; ----------------------------------------------------------------------
; This configuration is for the MSP430G2553 microcontroller installed
; in a T.I. "Launchpad" board, with the following I/O assignments:
; ----------------------------------------------------------------------
; P1.0 - LED1 output
; P1.1 - UART RXD
; P1.2 - UART TXD
; P1.3 - S2 input
; P1.4
; P1.5
; P1.6 - LED2 output
; P1.7
;
; P2.0
; P2.1
; P2.2
; P2.3
; P2.4
; P2.5
;
; Clocks:
; on-chip oscillator is used
;
; ----------------------------------------------------------------------
; POWER ON RESET AND INITIALIZATION

; ----------------------------------------------------------------------
; MSP430F1611 INITIALIZATION
; The reset default is for all I/O pins configured as inputs,
; watchdog enabled, SR reset.

main:   ; Debugger requires the 'main' symbol.
reset:
        ; Watchdog Timer
        MOV   #(WDTPW+WDTHOLD),&WDTCTL    ; stop watchdog timer

        ; Basic Clock Module
        ; My thanks to the 4e4th team for the following two lines!
        MOV.B   &CALBC1_8MHZ, &BCSCTL1   ; Set DCO
        MOV.B   &CALDCO_8MHZ, &DCOCTL    ; to 8 MHz.
        
        MOV.B   #00h,&BCSCTL2           ; MCLK=DCO/1, SMCLK=DCO/1

        ; Flash Memory Controller
        ; Flash Timing Generator frequency must be 257-476 kHz.
        ; 8 MHZ/17 = 470.59 kHz.   tFTG=2.125 msec.
        ; At 470 kHz, byte/word program time is 35*tFTG = 75 usec.
        ; Cumulative program time to any 64-byte block (between erasures)
        ; must not exceed 4 msec, thus 53 writes at 250 kHz.  Therefore,
        ; do not use exclusively byte writes in a 64-byte block.
        ; Also, "a flash word (low + high byte) must not
        ; be written more than twice between erasures."
        ; Program/Erase endurance is 10,000 cycles minimum.
        MOV #FWKEY+0,&FCTL1             ; write & erase modes OFF
        MOV #FWKEY+FSSEL1+16,&FCTL2     ; SMCLK/17 = 471 kHz.
        MOV #FWKEY+LOCK,&FCTL3          ; lock flash memory against writing

        ; Digital I/O
        MOV.B   #08h,&P1OUT             ; P1.3 is on for pullup
        MOV.B   #08h,&P1REN             ; P1.3 pullup enabled
        MOV.B   #45h,&P1DIR             ; P1.0,2,6 are outputs
        MOV.B   #0,&P1IE                ; no port 1 interrupts
        MOV.B   #06,&P1SEL              ; P1.1,2 are UART
        MOV.B   #06,&P1SEL2             ; P1.1,2 are UART

        ; MOV.B   # ,P2OUT
        MOV.B   #0,&P2DIR               ; P2 all inputs
        MOV.B   #0,&P2IE                ; no port 2 interrupts
        MOV.B   #0,&P2SEL               ; no functions enabled
        MOV.B   #0,&P2SEL2

        ; Timer A
        ; *TBD*

        ; Timer B
        ; *TBD*

        ; USCI_A0
        BIS.B   #UCSWRST,&UCA0CTL1      ; SWRST while configuring!
        MOV.B   #00h,&UCA0CTL0          ; UART, 8N1, LSB first
        MOV.B   #81h,&UCA0CTL1          ; BRCLK = SMCLK, SWRST set
        MOV.B   #41h,&UCA0BR0           ; 9600 Baud at 8 MHz
        MOV.B   #03h,&UCA0BR1
        MOV.B   #04h,&UCA0MCTL          ; UCBRFx=0, UCBRSx=2 for 9600 baud
        BIC.B   #UCSWRST,&UCA0CTL1      ; done configuring

        MOV     #0,TOS
BRDELAY: SUB     #1,TOS     ; delay to let baud rate settle?
        JNZ     BRDELAY

        ; Comparator A
        ; *TBD*

        ; ADC10
        ; *TBD*

        ; Interrupt Enables
        MOV.B   #0,&IE1                 ; no interrupts enabled
        MOV.B   #0,&IE2                 ; no interrupts enabled

        ; Forth registers
        MOV     #RSTACK,SP              ; set up stack
        MOV     #PSTACK,PSP
        MOV     #UAREA,&UP              ; initial user pointer
        
        ; AUTOSTART LOGIC
		; User Area and Variables RAM are restored from Info Flash.  
		; If BASE has a valid value, and SW2 is not pressed, and
		; LATEST points to a valid colon definition, execute that.  
		; Otherwise execute COLD, which will restore the User Area 
		; from ROM defaults.
        MOV		#UAREA,W		; User Area in RAM
		MOV		#USAVE,X		; saved user data in Info Flash
		MOV		#UAREA_SIZE+VARS_SIZE,TOS
URESTORE:	MOV	@X+,0(W)
        ADD     #2,W
        SUB     #1,TOS
		JNZ		URESTORE		; leaves TOS=0
		
        ; if Launchpad SW2 is pressed (zero), go to COLD
        BIT.B   #8,&P1IN
        JZ      NOAUTO

		; If restored BASE is invalid, go to COLD.
		; If any bit except 4,3, or 1 is set, base is invalid.
		; This allows bases 0x10, 0x0a, 0x08, and 0x02 (and a few others).
BASEOFFSET equ 4		; if User Area is changed, update this constant
		MOV		&UAREA+BASEOFFSET,W
		BIT		#(0xffff-0x1a),W
		JNZ		NOAUTO
		
		; Check that Code Field of LATEST word is DOCOLON.
		; LATEST points to the name field.
		; This is for an indirect-threaded Forth.
		; See forth.h for header structure.
LATESTOFFSET equ 14	; if User Area is changed, update this constant

		MOV		&UAREA+LATESTOFFSET,W
		MOV.B	@W,X			; get name length
		ADD		#1,X			; add 1 for length byte
		ADD		X,W				; compute address of Code Field
		ADD		#1,W			; force it to be even aligned
		BIC		#1,W
		CMP		#DOCOLON,0(W)	; is it a colon definition?
		JNZ		NOAUTO			; if not, do COLD
		ADD		#2,W			; else compute address of Parameter Field
		MOV		W,IP			;  and make that the starting IP
        NEXT

		; default startup is to perform COLD
NOAUTO: MOV     #INITIP,IP
        NEXT

; ----------------------------------------------------------------------
; DEFAULT INTERRUPT HANDLER

nullirq: RETI
