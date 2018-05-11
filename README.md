# 4e4th05a

4e4th for the MSP430, version 0.5a - 11 May 2018
based on:
CAMELFORTH FOR THE MSP430 - VERSION 0.5A - 21 APRIL 2014

What is true for CF is true for 4e4th as well. Earlier Versions used the IAR Workbench "Kickstart" to assemble the CamelForth code. Version 0.5A uses Mike Kohn's naken_asm cross-assembler, which supports
several different target processors, and is available for Windows, Mac, and Linux.  

Find it at <http://www.mikekohn.net/micro/naken_asm.php>

To assemble a fresh 4e4th run:
 naken_asm -l -o 4e4th05a.hex 4e4th05a.asm 

See: readme.430, releasenotes.txt, msp430development.pdf and CFvs4e4th05a.pdf to learn more.  

Have fun, mk
