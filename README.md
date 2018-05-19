# 4e4th Naken Version Forth
 - 11 May 2018

*** debug release ***

4e4th Version 0.5a for the TI MSP430G2553 Value Line LaunchPad Develpoment Tool (MSP-EXP430G2), Rev. 1.5 chip.  
One of the few processors still available in the breadboard friendy 20 Pin DIL Package. 
4e4th and application will run ON this chip.

Based on Brad Rodriguez’s CamelForth for the MSP430, version 0.5a, 21 apr 2014. http://www.camelforth.com/news.php

Uses here the free Naken Assembler https://github.com/mikeakohn/naken_asm  
Ported to Naken Assembler by M.Kalus - 11 May 2018 

## Assembling 4e4th
Open a command prompt window. Change to the directory where your source code is. Type:  
naken_asm -l -o 4e4th05a.hex 4e4th05a.asm 

Have fun, mk

## Try it out directly
It is easy to program 4e4th into the TI MSP430 Launchpad.  
Download and install the free 430 version of the elprotronic programmer at  
 https://www.elprotronic.com/productdata;jsessionid=8FA5E1E626677AABC3683EC0D712B01F  
Select the correct processor 430G2553, point to the hex file you find here and follow the steps.  
Do not forget RESET after programming.

Then start your favourite Terminal program and start writing short examples, like in:   https://wiki.forth-ev.de/doku.php/en:projects:a-start-with-forth:start0  
and there probably chapter 11c as a good starting point.

## Start with Forth 
- Quickstart  
Connect to 4e4th on your Launchpad using a terminal emulator.  
Compile and save demo forth application: blink.4th  
4e4th is case insensitive, type as youlike, upper- or lower case.

- New to Forth?  
https://wiki.forth-ev.de/doku.php/en:projects:a-start-with-forth:start0  
There are various Forth systems mentioned, but the handling is the same.

- New to the TI LaunchPad?
https://wiki.forth-ev.de/doku.php/projects:4e4th:start

- New to 4e4th?
Go through the documentation at https://wiki.forth-ev.de/doku.php/en:projects:4e4th:start

- Need the Programmer? 
https://www.elprotronic.com/  
"Lite FET-Pro430 Elprotronic Programmer" burns image into MCU.

- More books
https://wiki.forth-ev.de/doku.php/en:projects:litlist  
and  
https://wiki.forth-ev.de/doku.php/en:projects:pintaske_s_electronic_forth_bookshelf

## Verification

## To Do
Discard input stream if an error occurs.

19 May 2018   (finis)
