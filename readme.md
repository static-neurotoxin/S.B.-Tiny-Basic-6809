# Santa Barbara Tiny Basic for the MC6809

## by James A. Hinds

This is a version of tiny basic that appears in issue #55, May 1981 issue of [Dr. Dobbs Journal of Computer Calisthenics & Orthodontia](https://www.drdobbs.com).

Road Map:

* [X] Transcribe [source code](09-BAS.asm) to text with minimal changes
* [X] Transcribe original [article](article.md) to markdown
* [X] Port source to work with one or more modern 6809 assemblers
   + [X] [A09](https://github.com/Arakula/A09) - 6800/6801/6809/6301/6309/68HC11 Assembler

* [ ] Not working: 
    + [ ] [LWTOOLS](http://www.lwtools.ca/manual/index.html) '09 assembler
    + [ ] [ASM6809](https://www.6809.org.uk/asm6809/) is a portable cross assembler targeting the Motorola 6809 and Hitachi 6309.
    + [ ] [ASM80](https://asm80.com) Online assembler

      The general issue with these assemblers is that they don't support macros enough to compile the original source.
      The code could be refactored to replace the original macros, but it the effort required seems excessive in the face of
      a working cross platform assembler      

* [ ] Document I/O to allow easy porting to various '09 SBC
