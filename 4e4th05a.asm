; 4e4th Version 0.5a for the Texas Intruments MSP430G2553 LaunchPad
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
; 4e4th-0.5a.asm - build file for naken_asm
; M. Kalus 29. Apr 2018
; used to be: camel430g2553.asm - build file for naken_asm
; B. Rodriguez  1 Mar 2014

; Revision History
; 29 apr 2018 mk - modified to make 4e4th. find ;mk in files to see all modified lines.
;  1 mar 2014 bjr - adapted for naken_asm from init430g2553.s43.
; 27 nov 2012 bjr - moved UP to just below UAREA

.msp430
.include "msp430g2553.inc"  ; MCU-specific register equates
.include "itc430.inc"       ; registers, macros, and header structure

; ----------------------------------------------------------------------
; MEMORY MAP of the MSP430G2553
; 16KB flash ROM, 0.5KB RAM
;
; 0000-01FF = peripherals
; 0200-03FF = 0.5KB RAM
; 0400-0FFF = unused
; 1000-10FF = 256B information memory
; C000-FFFF = 16KB flash ROM 
;   FFE0-FFFF = interrupt vectors
;
; CamelForth REVISED memory map (puts UAREA last, before dictionary):
;   UAREA-128h  HOLD area, 20 bytes, grows down from end
;   UAREA-114h  PAD buffer, 82 bytes, must follow HOLD area
;   UAREA-114h  TIB Terminal Input Buffer, 82 bytes, overlaps PAD
;   UAREA-0C2h  Parameter stack, 96 bytes, grows down from end
;   UAREA-62h   Return stack, 96 bytes, grows down from end
;   UP          User Pointer, 2 bytes
;   UAREA       User area, 32 bytes
;   UAREA+20h   RAM variables space, 96 bytes
;   UAREA+80h   start of Forth dictionary, 88 bytes available
; The User Area and RAM Variables space will be restored from 
; Info ROM, so they should total 128 bytes.
;
; Note all must be word-aligned.
; See also the definitions of U0, S0, and R0 in the "system variables &
; constants" area.  A task w/o terminal input requires 200h bytes.
; Double all except TIB and PAD for 32-bit CPUs.
;
; Because the MSP430G2553 is resource-limited, we use the Info Flash
; to hold user interrupt vectors and reset data.
;   INFO+000h (INFOD):  RAM save area
;   INFO+040h (INFOC):  RAM save area
;   INFO+080h (INFOB):  user interrupt vectors
;   INFO+0C0h (INFOA):  configuration data - do not use

UAREA_SIZE equ 16         ; cells
VARS_SIZE equ 48          ; cells
RSTACK_SIZE equ 48        ; cells
PSTACK_SIZE equ 48        ; cells
; following only required for terminal tasks
HOLD_SIZE equ 20          ; bytes (must be even)
PAD_SIZE equ 0            ; bytes (must be even)
TIB_SIZE equ 82           ; bytes (must be even)

INFO_SIZE equ (UAREA_SIZE+VARS_SIZE)*2    ; bytes

; FLASH MEMORY LIMITS
; for Flash memory operations - this includes information and main
; ROM, but not the main ROM used by the kernel (above E000h)
INFOSTART  equ 01000h
INFOEND    equ 010BFh     ; do not allow config flash to be erased
FLASHSTART equ 0C000h
FLASHEND   equ 0DFFFh
MAINSEG    equ 512
INFOSEG    equ 64


; ----------------------------------------------------------------------
; RAM DATA AREAS 

        .org 0200h      ; start of RAM

        DS8    HOLD_SIZE
; HOLDAREA: ; end of hold area - hold area grows down from PAD
PADAREA: DS8   PAD_SIZE             ; must follow HOLDAREA
TIBAREA: DS8    TIB_SIZE            ; Terminal Input Buffer

LSTACK: DS16    PSTACK_SIZE   ; leave stack grows up into PSTACK area
PSTACK: ; end of parameter stack area
        DS16    RSTACK_SIZE
RSTACK: ; end of return stack area

UP:     DS16    1                   ; User Pointer

UAREA:  DS16    UAREA_SIZE

RAMDICT: DS16   VARS_SIZE     ; 96 bytes for variables
; ROMDICT:          ; all RAM following is program dictionary
ROMDICT EQU     FLASHSTART  ; to use Flash ROM for program dictionary

; ----------------------------------------------------------------------
; DATA FLASH AREAS 

        .org 1000h      ; start of info Flash

        ; 128 bytes at start of Info area, for saved User Area & variables
USAVE:  DS16  UAREA_SIZE
        DS16  VARS_SIZE

.if $>1080h
.error "SAVE area does not fit in Info Flash"
.endif

; ----------------------------------------------------------------------
; SOURCE FILES

.include "430g2553vecs.asm" ; note: sets .org for vector tables
        .org 0E000h         ; start address of CamelForth kernel
.include "itc430core.asm"   ; code primitives
.include "430g2553io.asm"
.include "itc430deps.asm"
.include "itc430hilvl-mk.asm"
.include "itc430extras.asm"
.include "itc430irpts.asm"
.include "430g2553install.asm"
.include "430g2553save.asm"

INITIP   equ COLD+2         ; default coldstart word (high-level)
.include "430g2553init.asm"

; ----------------------------------------------------------------------
; END OF FORTH KERNEL

.set lastword = link           ; last word in dictionary
ENDE:
.if $>intvecs
.error "Forth does not fit in DataFlash"
.endif

        END
