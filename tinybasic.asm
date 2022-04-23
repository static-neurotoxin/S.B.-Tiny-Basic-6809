; palo alto tiny basic by li-chen wang
;
; converted for  ’the mill’ of stellation two
;   by james a. hinds
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
rats            macro
return          set             *
                rts
                endm

 ; This macro had code to fixup some issues in source, will fix code in place
here            macro
tmp             set             *
                org             table
;                ifeq            &1,,1
                FCB             &1
                fdb             tmp+$8000
;                ifc             'goto',&1
;                fcc             "go to"
;                fdb             tmp+$8000
;                endif
table           set             *
                org             tmp
                endm

dispat          macro
                jsr             <exec
                fdb             table
                endm

tstc            macro
                jsr             <ignblk
                cmpa            #&1
                bne             &2
                leay            1,y
                endm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; variable area at u

                org             0               ; offset from u
varbgn          equ             -27*2           ; variable space
ocsw            rmb             1               ; output on/off flag
fldcnt          rmb             1               ; places for numeric output
currnt          rmb             2               ; points to curr line
txtunf          rmb             2               ; -> to last of prg
stkgos          rmb             0               ; save of s in gosub
varnxt          equ             *
stkinp          rmb             2               ; save s during input
lopvar          rmb             2               ; for loop index
lopinc          rmb             2               ; for loop step
loplmt          rmb             2               ; limit
lopln           rmb             2               ; loop beginning line
loppt           rmb             2               ; loop prog restart
seed            rmb             2
hiwat           rmb             2               ; end of @ array
                rmb             2               ; extra for buffer
buffer          rmb             72
bufend          equ             *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; page zero subroutines of 8080
;   are replaced on 6809 by direct page
;   subroutines and can be accessed
;   by 2 byte jsr
;   direct page is page 1 ($100-$1ff)
;   page 0 contains table of command names
;   and their code location equiv locations
;   entries into this table is by
;   macro "here"
                org             0
                lbra            start
table           set             *
tmp             set             0
                org             $100            ; leave lots of space for table
                setdp           * >> 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tstnum returns value and
;    length of ascii coded integer
;    in text. a non-number will have
;    a length of zero.
tstnum          clrb
                clra
                tfr             d,x             ; clear x and accom
                pshs            d,x             ; tos= num and nos=#digits
tn1             jsr             <ignblk
                suba            #'0
                blo             numout          ; char too low
                cmpa            #9
                bhi             numout          ; hit char that isnt digit
                leay            1,y             ; bump to next char
                pshs            a               ; save char
                inc             4,s             ; increment digit count
                ldd             1,s             ; get accumulator
                cmpa            #$0f            ; check if room
                bhi             qhow            ; too big to mult by 10
                aslb
                rola                            ; double
                aslb
                rola                            ; times 4
                addd            1,s             ; times 5
                aslb
                rola                            ; times ten
                addb            ,s+             ; add in digit
                adca            #0
                std             0,s             ; store accom back
                bpl             tn1             ; no err? then get next digit
qhow            lbsr            error
                fcc             'how?'
                fcb             $0d

numout          tst             3,s             ; set flags on g digits
                puls            d,x,pc          ; and go away
tvhow           bvs             qhow            ; direct overflow test for error
                rats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  fin is called to continue
;    execution of next stmt
;    on same or next line.
;    however, if current stmt
;    has more to process control
;    goes back to caller
fin             tstc            ';,fi1
                leas            2,s             ; void return
                lbra            runsml
fi1             cmpa            #$d
                bne             return
                leay            1,y
                leas            2,s
                lbra            runnxl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  endchk is used to verify
;    a command is terminated by
;    a cr before performing
;    the command.
endchk          tstc            $d,qwhat
                rats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fine used to continue execution
;    with next statement. if current
;    statement is not at end
;    a ’what’ error is signalled
fine            bsr             fin             ; if error then fall through
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
qwhat           lbsr            error
                fcc             'what?'
                fcb             $0d


qsorry          lbsr            error
                fcc             'sorry'
                fcb             $0d

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ignblk    scans past any
;  spaces and returns first
;  char with prog ptr updated
i1              leay            1,y
ignblk          lda             0,y
                cmpa            #'              ; a blank
                beq             i1              ; get next
                rats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  output routines
;      space        puts one blank
;      crlf         puts cr
;      outc         puts char in mailbox
;         for 6502 to grab and
;         clear mailbox
space           lda             #'              ; a space
                bra             outc
crlf            lda             #13
outc            exg             a,a             ; give 6502 some extra cycles
                tst             $fbff
                bne             outc
                sta             $fbff
                rats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  input routine
;    chkio return zero if no
;    char, otherwise grab it
;    restart basic if it is
;    control-c
chkio           lda             $fbfe           ; char loc
                beq             return          ; if no char just say so
                clr             $fbfe           ; otherwise clear it
                cmpa            #$03            ; is it cntl c
                bne             return          ; no return char
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   start relay to first of basic
;
start           lbra            sti
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tstve return address and
;     value of variable
tstve           bsr             ignblk
                suba            #'@
                bcs             return
                beq             rlyat           ; is string space
                cmpa            #27             ; is in alphabet??
                bge             tvn
                leay            1,y
                asla    
                nega    
                leax            a,u
                ldd             ,x
                clc     
                rats            return          ; with carry clear
rlyat           lbra            atsign
tvn             sec                             ; set carry
                rats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   exec and table dispatch
;
;      scan table for keyword
;       jump 1ro associated code
exec            jsr     <ignblk
                ldx     [0,s]                   ; get table address from caller
                sty     0,s                     ; save table address        
lct             lda     ,y+
                cmpa    ,x+                     ; match prg against table
                beq     lct
                lda     -1,x                    ; look at mismatch
                bmi     execf                   ; found!
                lda     -1,y                    ; is it an abbrev
                cmpa    #'.
                beq     execf                   ; yes! force match
                ldy     0,s                     ; restore prog ptr
exec1           lda     ,x+
                bpl     exec1                   ; skip rest of keyword in table
                lda     ,x+
                bra     lct                     ; retry
execp           lda     ,x+
                bpl     execp
                leay    1,y
execf           leay   -1,y                     ; back up prg ptr
                ldb     0,x                     ; next transfer byte
                anda    #$7f
                leax    0,pcr
                leax    d,x
                stx     0,s
                rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; expression analyzer
;
expr            bsr     expr2
                pshs    x                       ; save left hand value
                dispat
                here   '>='
                bsr    xp18
                bgt    zer                      ; m>=r
                bra    one

                here   '='
                bsr    xp18                     ; m=r
                beq    one
                bra    zer

                here    '>'
                bsr     xp18                    ; m>r
                blt     one
                bra     zer

                here    '<='
                bsr     xp18                    ; m<=r
                bge     one
                bra     zer

                here    '='
                bsr     xp18
                beq     one
                bra     zer

                here    '<'
                bsr     xp18                    ; m<r
                bls     zer
one             incb
zer             std     ,s                      ; to set flags

                here    .
                ldd    ,s++                     ; pop and set flags
                rats   no                       ; logical op

xp18            bsr     expr2
                cmpx    2,s                     ; compare with left
                pshs    cc
                clra
                clrb                            ; set d to zero (false)
                puls    cc,pc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  expr2 looks after + and -
;        binary operators
expr2           bsr     expr3                   ; go after left hand operand
expr21          pshs    d
                jsr     <ignblk
                cmpa    #'+
                bne     xp290
                leay    +1,y
                bsr     expr3
                addd    ,s++                    ; add in left
xp235           bvc     expr21                  ; and again
                jmp     qhow                    ; overflow
xp290           cmpa    #'-
                bne     xp291
                leay    1,y
                bsr     expr3
                pshs    d
                ldd     2,s
                subd    0,s
                leas    4,s                     ; pop garbage
                bra     xp235                   ; check for overflow

xp291           puls    x,pc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; expr3 handles things with
;        / and * in them
;
expr3           bsr     expr4
                pshs    d
exp3el          dispat
                here    '*'
                bsr     expr4
                lbsr    mult
                beq     exp3el                  ; signal error if d reg has significant bits
exp3er          jmp     <qhow
                here    '/'
                bsr     expr4
                std     -2,s                     ; set flags
                beq     exp3er
                lbsr    div
                std     0,s                      ; replace rem by qou
                bra     exp3el
                here    ,
                puls    d,pc                    ; get result and return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; expr4 handles functions and
;     unary minus and represents
;     the highest priority
;     if there are (<)) then expr
;     is called (oh! no!) recursivly
;
expr4           dispat on                       ; functions
                here    'rnd'
                ldd     seed,u
                pshs    d
                ldd     #%100011
                lbsr    mult
                puls    d
                addd    #$0123
                anda    #$7f                    ; mask sign
                std     seed,u
                pshs    d                       ; save for division
                bsr     parm
                lble    qhow                    ; if zero or neg
                lbsr    div
                puls    d,pc                    ; get remainder and return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   absolute value
;
                here    'abs'
                bsr     parm                    ; get the value
                bpl     return
                pshs    d
                clra
                clrb
                subd    ,s++
                jmp     tvhow                   ; implied return if no overflow

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  size how much storage left
;    between stack and end of
;    program
                here    'size'
size            tfr     s,d
                subd    hiwat,u                 ; subtract end of string addr
                rats

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; unary minus handled as fnct
                here    '-'                     ; unary minus
                jsr    <expr
                coma
                negb
                sbca   #0
                jmp    <tvhow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; check for number,    variable or
;  subexpression
;
                here    ,
                jsr     tstve                   ; not a function must be var
                bcc     return
                jsr     <tstnum                 ; not var try num
                bne     return                  ; digit cnt<>0 means number
parm            tstc    '<,xp43
                jsr     <expr
                pshs    cc,d                    ; save conditions of expression
                tstc    '),xp43
                puls    cc,d,pc                 ; fetch conditions and return
xp43            jmp     qwhat

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get index into string space
; if we go passed the current
; high water mark we bump that
; to keep stack from going
; bananas
;
atsign          leay    1,y
                bsr     parm                    ; get the index
                lbmi    qhow
                ldx     txtunf,u                ; get end of prog
                leax    2,x
                leax    d,x
                leax    d,x
                cmpx    hiwat,u
                bls     hiwok
                stx     hiwat,u
hiwok           bsr     size
                lbmi    qsorry                  ; no room
                clc
                ldd     0,x                     ; get value at index
                rats

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; direct statement execute
direct          leay   2,y                      ; step past line number
                dispat  and                     ; go to it!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; list program possibly from
;      a given line number, otherwise
;      the whole shebang
;
                here    'list'
                jsr     <tstnum                 ; test if a ln #
                pshs    d
                jsr     <endchk                 ; if no # then use 0
                puls    d
                lbsr    fndln                   ; find it
ls1             lbhi    start                   ; hit end?
                lbsr    prtln                   ; print the line
                jsr     <chkio                  ; user interrupt?
                lbsr    fndlnp                  ; find next line
                bra     ls1

                here    'new'
                jsr     <endchk                 ; abort if not only thing in stmt
                leax    txtbgn-1,pcr
                stx     txtunf,u                ; reset beginning of program ptr
                stx     hiwat,u                 ; and hi water string
                jmp     <start

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  run----- fire the rockets and
;         blast off
;     each statement is responsible
;    for coming back to runnxl,
;    runnnl,runtsl and runsml
;    as program pointer <v) needs
;    care and feeding
                here    'run'
                jsr     <endchk
                leay    txtbgn,pcr              ; first line
runnxl          clra
                clrb                            ; start at any line >0
runnnl          lbsr    fndlnp
                lbhi    start
runtsl          sty     currnt,u
                leay    2,y                     ; bump passed line num
runsml          jsr     <chkio                  ; run same line
                dispat  start                   ; doing statement

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   stop will cause graceful
;    termination
;
                here    'stop'
                jsr     <endchk
                jmp     <start

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   print print out expressions,
;       strings. with possible
;        format for digits
;
                here    'print'
                lda     #6
                sta     fldcnt,u
                jsr     <ignblk
                cmpa    #';
                bne     pr2
                leay    1,y
                jsr     <crlf
                bra     runsml
pr2             cmpa    #$0d
                bne     pro
                leay    1,y
                jsr     <crlf
                bra     runnxl
pro             cmpa    #'#                     ; is it a format length?
                bne     pr1
                leay    1,y
                jsr     <expr
                stb     fldcnt,u
                bra     pr3
pr1             lbsr    qtstg                   ; print possible string
                beq     pr3
                jsr     <expr
                lbsr    prtnum
pr3             jsr     <ignblk
                cmpa    #',
                bne     pr6
                leay    1,y
                jsr     <fin                    ; if not endline then come back
                bra     pro
pr6             jsr     <crlf                   ; end of statement with
                jmp     <fine                   ; implied cr and lf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    goto
;
                here    'goto'
                jsr     <expr
                pshs    y                       ; save txt pointer for error sub
                lbsr    fndln
                bne     goerr                   ; couldn’t find destination
                leas    2,s                     ; discard old line num
                bra     runtsl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    if
;
                here    'if'
                jsr     expr                    ; evaluate the expression
                bne     runsml                  ; if <> 0 then cont
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      fall through to
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   rem    ignore this whole line
;
                here    'rem'                   ; looks like ’if 0’
                ldy     currnt,u
                ldd     #1
                addd    0,y      current line plus one
                bra     runnnl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   input
inperr          lds     stkinp,u                ; regain
                puls    d,x,y                   ; currnt,junk,text ptr
                std     currnt,u                ; and retry this input stmt
                here    'input'
ip1             pshs    y                       ; save text pointer
                lbsr    qtstg                   ; print quoted prompt
                bne     ip2                     ; no try variable
                jsr     tstve
                bcs     ip4
                bra     ip3                     ; yes input variable
ip2             pshs    y                       ; save for prtstg
                jsr     tstve
                lbcs    qwhat
                lda     ,y
                clr     ,y                      ; set to zero as stopper
                puls    y
                pshs    a
                lbsr    prtstl                  ; print prompt
                puls    a
                sta     ,-y                     ; replace char of prog
ip3             tfr     x,d                     ; regain address of var
                pshs    y
                ldx     currnt,u
                pshs    x
                leax    ip3,pcr                 ; point currnt to ng as input flag
                stx     currnt,u
                sts     stkinp,u                ; save stack pointer
                pshs    d                       ; the destination address
                lda     #':                     ; prompt
                lbsr    getln                   ; get the input from user
                leay    buffer,u
                jsr     expr                    ; and evaluate it as an expression
                std     [,s++]                 ; save value in var
                puls    x
                stx     currnt,u
                puls    y
ip4             puls    d                       ; purge junk
                tstc    "',",ips                ; is next a comma
                bra     ip1                     ; yes, do another variable
ips             jmp     <fine                   ; finish

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; let      the    assignment statement
;
                here    'let'
let             lbsr    setval                  ; do the assignment
                tstc    ip5                     ; check end line
                bra     let

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; gosub
;
goerr           puls    y                       ; regain current line
                jmp     <qhow                   ; and print diagnostic
                here    'gosub'
                lbsr    pusha                   ; save for parameters
                jsr     <expr                   ; get val in d
                pshs    y                       ; save text pointer
                lbsr    fndln
                bne     goerr                   ; no line say 'how'
                ldd     currnt,u
                pshs    d
                ldd     stkgos,u
                pshs    d
                clra
                clrb
                std     lopvar,u                ; save new ones
                sts     stkgos,u
                lbra    runtsl                  ; and run dest line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     return      rom gosub
;
                here    'return'
                jsr     <endchk                 ; bad if not end of line
                ldd     stkgos,u                ; reload stack pointer
                lbeq    qhow                    ; too many returns
                tfr     d,s                     ; restore stack pointer
                puls    d,x,y
                std     stkgos,u
                stx     currnt,u
                lbsr    popa                    ; restore for—next environment
                jmp     <fine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; next
;
                here    'next'
                ldx     lopvar,u                ; if no for then
                beq     next2                   ; complain loudly
                jsr     <tstve
                bcc     nexto                   ; if no variable specified
                ldx     lopvar,u                ; then ust use latest for variable
nexto           cmpx    lopvar,u
                beq     next3
                lbsr    popa
                bne     nexto
next2           jmp     <qhow
next3           ldd     0,x                     ; and put value into accom
                addd    lopinc,u
                std     0,x
                tst     lopinc,u                ; which direction are we going?
                bmi     next4
                cmpd    loplmt,u
                bgt     next5                   ; done
next6           ldx     lopln,u
                stx     currnt,u
                ldy     loppt,u
                jmp     <fine
next4           subd    loplmt,u
                bge     next6
next5           lbsr    popa
                jmp     <fine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; for
;
                here    'for'
                bra     fore                    ; jump around default stmt code
;   to allow table entry
;      for assignment to be
;      prior to keywords of
;      for statement in table
;      this is required by the
;      macro here and the way it
;      works
                here    ,                       ; default assignment
                jsr     <fin                    ; can be empty
                lbra    let                     ; otherwise treat as let
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; actual code of for
;
fore    lbsr            pusha                   ; save loop vars
        bsr             setval
        stx             lopvar,u                ; save addr of loop var
        dispat
        here            'to'                    ; 'for'1-1 'to'
        jsr             <expr
        std             loplmt,u                ; stopping val
        bra             for2                    ; jump around default
        here            ,                       ; did not have 'to'
        jmp             <qwhat                  ; so complain
for2    dispat
        here            'step'
        jsr             <expr
        bra             for3
        here            ,                       ; no step value
        ldd             #1                      ; so default to one
for3    std             lopinc,u
        ldx             currnt,u
        stx             lopln,u                 ; loop line number
        sty             loppt,u                 ; loop prog pointer
        leay            -10,s                   ; start looking at old ’pusha’ recofds
for7    leay            10,y
        ldx             0,y                     ; get loop var address
        beq             for8                    ; exhausted all fors
        cmpx            lopvar,u                ; is old same as this var?
        bne             for7
        leax            +10,y                   ; found match must delete it
        sts             ,--s                    ; save stack pointer
for7l   lda             ,-y
        sta             ,-x
        cmpy            0,s
        bgt             for7l
        leas            2,x                     ; cut back stack
for8    ldy             loppt,u                 ; get prog pointer
        jmp             <fine
setval  lbsr            tstve                   ; must be variable
        bcs             qw1                     ; no variable
        pshs            x
        tstc            '=,qw1
        jsr             <expr                   ; evaluate the expression
        puls            x
        std             0,x                     ; leave address in x     <for uses it>
        rats
qw1     jmp             <qwhat                  ; relay to qwhat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; utilities and extras
;
; findline line number in d
fndln   tsta
        lbmi            qhow                    ; negative line num
        leay            txtbgn,pcr              ; initial line ptr
fndlnp  cmpy            txtunf,u                ; passed end?
        bhi             return
        cmpd            0,y                     ; look at line number
        bls             return                  ; if reg <= line num
fndnxt  equ             *
        cmpy            txtunf,u                ; dont scan past end
        bhi             return
        leay            2,y
        pshs            a                       ; save line numb high byte
        lda             #13
f1      cmpa            ,y+
        bne             f1                      ; scan to cr of line
        puls            a                       ; regain high line num
        bra             fndlnp                  ; try this line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; getln      ge1 input line and
;            edit according to
;            backspace, cntl x

getln   jsr             <outc                   ; prompt
        leay            buffer,u                ; start at beginning
        leax            bufend,u
        pshs            y,x
gl1     jsr             <chkio
        beq             gl1                     ; nothing yet
        jsr             <outc                   ; echo
        cmpa            #$0a                    ; line feed
        beq             gl1                     ; ignore it
        cmpa            #$1f&'h                 ; delete code
        beq             gl3
        cmpa            #$1f&'x                 ; control x
        beq             gl4
        sta             ,y+
        cmpa            #$d
        beq             glo                     ; return
        cmpy            ,s                      ; with buffer end
        bne             gl1
gl3     cmpy            2,s                     ; anything in buffer?
        beq             gl4
        leay            -1,y
        jsr             <space
        lda             #$1f&'h                 ; control h
        jsr             <outc                   ; and back up over space
        bra             gl1
gl4     jsr             <crlf
        leas            4,s                     ; pop off temporaries
        lda             #'?                     ; question mark prompt
        bra             getln
glo     leas            4,s
        sta             2,y                     ; throw another return
        rats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; prtnum and friends
;
prtnum  ldx             #10
        pshs            d,y,x                   ; save prg pointer and stopper
        ldb             fldcnt,u
        decb
        rola                                    ; put sign of quantity into low b
        rolb
        sex
        tfr             d,y                     ; put into counter

        ldd             ,s++                    ; set flags
        bpl             pn2                     ; complement if negative
        coma
        comb
        addd            #1
pn1     leay            -2,y                    ; decrement place count
pn2     pshs            d                       ; save quotient
        ldd             #10                     ; divide by 10 to get
        lbsr            div                     ; next digit
        bne             pn1                     ; have not got all
        tfr             y,d                     ; put place count into b
        bra             pn3
pn4     jsr             <space                  ; print space
pn3     subb            #2                      ; dec space count        
        bpl             pn4
        rorb                                    ; put sign bit into carry
        bcc             pn6                     ; dont print minus
        lda             #'-                     ; leading minus
pn8     jsr             <outc
pn6     puls            d                       ; get remainder
        cmpb            #10                     ; done?
        beq             pn7
        tfr             b,a
        ora             #$30
        bra             pn8
pn7     puls            y,pc                    ; and return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; prtln     print line of basic
;               program                                                                
;
;                                                                                  

prtln   ldb             #4                      ; four spaces
        stb             fldcnt,u
        ldd             ,y++                    ; get line number
        bsr             prtnum                  ; and print it
        jsr             <space
prtstl  clrb                                    ; force no match

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  and fall into

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; prtstg    print string
;
;

prtstg  cmpb            ,y+                     ; print string at y
        beq             return                  ; if same as stopper
        lda             -1,y                    ; get that char
        jsr             <outc
        cmpa            #13
        bne             prtstg                  ; not a cr so go
        tsta                                    ; if return then non zero
        rats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; print quoted    string
;   return with   not zero if didnt recognize as our stuff

qtstg   tstc            '",qt3                  ; double quote
        ldb             #'"
qt1     bsr             prtstg
        lbne            runnxl                  ; run next line if hit return
        rats            return                  ; w zero set
qt3     tstc            $27,qt4                 ; single quote
        ldb             #$27                    ; print till hatch
        bra             qt1
qt4     tstc            '!,return
        lda             #$8d                    ; funny return
        jsr             <outc                   ; apple does not support this
        clra                                    ; set zero status
        rats

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; error report
error   pshs    y                               ; save line nuhber
        ldy     2,s                             ; get the return addr
        bsr     prtstl                          ; print how or what
        puls    y
        lda     0,y
        pshs    a                               ; save the char at error point
        clr     0,y
        ldy     currnt,u                        ; is this in
        ldd     0,y                             ; immediate mode?
        beq     erro
        lbmi    inperr                          ; input command??
        bsr     prtln                           ; print up to zero
        lda     #'?
        jsr     <outc                           ; print question
        puls    a
        sta     ,-y                             ; restore line
        leas    2,s                             ; pop phony return
        bsr     prtstl
erro    jmp     <start

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      popa and pu8ha
;
;
popa    ldd     2,s
        beq     popao
        std     lopvar,u
        ldd     4,s
        std     lopinc,u
        ldd     6,s
        std     loplmt,u
        ldd     8,s
        std     lopln,u
        ldd     10,s
        std     loppt,u
        puls    d
        leas    10,s
        andcc   #$fb                            ; turn off zero
        tfr     d,pc
popao   std     lopvar,u
        puls    d
        leas    2,s
        tfr     d,pc

pusha   lbsr  size                              ; anu room?
        subd   #12
        lbls   qsorry
        puls   d                                ; return addr
        ldx    lopvar,u                         ; test this
        beq    pu1
        ldx    loppt,u
        pshs   x
        ldx    lopln,u
        pshs   x
        ldx    loplmt,u
        pshs   x
        ldx    lopinc,u
        pshs   x
        ldx    lopvar,u
pu1     pshs   x
        tfr    d,pc                             ; and return
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sti (alias start)
;    initialize and enter command
;     mode
;
ok      fcc    'ok'
        fcb    $13 
sti     leax   tstnum,pcr
        tfr     x,d
        tfr     a,dp
        leau    datas,pcr
        leas    varbgn,u
        jsr     <crlf
        leay    ok,pcr                          ; print ok and prompt
        lbsr    prtstl
        clra                                    ; b is already zero
        std     lopvar,u                        ; no current for loops
        std     stkgos,u                        ; nor gosub returns
st3     lda     #'>                             ; prompt to enter a statement
        lbsr    getln
        pshs    y                               ; save pointer to end of text
        leay    buffer,u                        ; point to start
        jsr     <tstnum                         ; will return zero if no num
        pshs    cc,d
        jsr     <ignblk
        ldd     1,s                             ; regain line num
        std     ,-y                             ; put line number in front
        sty     currnt,u                        ; make the current line
        puls    cc,d
        lbeq    direct                          ; do immediate node if no leading number
; y points to beginning of line
; and top of stack points to end
        pshs   y
        lbsr   fndln
        pshs   y                                ; tos->text area
        bne    st4                              ; insert
        lbsr   fndnxt
        ldx    0,s                              ; beginning of line to delete
        bra    stl
stle    lda    ,y+
        sta    ,x+
stl     cmpy   txtunf,u                         ; are we done moving?
        bls    stle
        leax   -1,x
        stx    txtunf,u                         ; update end pointer
        stx    hiwat,u                          ; and string space
st4     ldd    4,s                              ; point to end of line
        subd   2,s                              ; find if length of line is empty
        cmpd   #3
        beq    sti                              ; yes, do not insert
; make room for new line
        ldy     txtunf,u
        leax    d,y                             ; add in length
        stx     txtunf,u                        ; update end pointer
        stx     hiwat,u
        lda     #$0d
        sta     0,x
st2le   lda     ,-y
        sta     ,-x
        cmpy   0,s                              ; have we reached beginning?
        bhi    st2le
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; start moving line
        puls    y

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  x points to end of line area
;  y points to beginning of line
;  tos points to beginning of text
;  n06 points to end of buffer
        puls    x
st3le   lda     ,x+
        sta     ,y+
        cmpx    ,s
        blo     st3le
        puls    x                               ; discard garbage
        bra    st3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; multiply and divide routines
; calling sequencesl
; mult   ldd ^multiplier
;        pshs d
;        ldd ^multiplicand
;        lbsr mult
; on exit d has high order bits
; t06 has low )rder bits
; eqal zero it true iff the
; contedtb of d are the same
; as the high ht of tos
;    neg is set on the sign
; of   tos
mult    leas    -3,s
        bsr     unsign                                  ; returns w/d=0
        ldx     +16
        ror     5,s
        ror     6,s
m1      bcc     m2
        addd    1,s
m2      rora
        rorb
        ror     5,s
        ror     6,s
; result is now in d cat 5,s
        leax    ,-x
        bne     m1
sign    tst     0,s                                     ; sign loc
        bpl     setflg
        coma
        comb
        com     5,s
        neg     a,s
        bne     setflg
        inc     5,s
        bne     setflg
        incb
        bne     setflg
        inca
setflg  tst     5,s
        bmi     s2
        std     -2,s                                    ; set flags
        andcc   #$f7                                    ; set positive
        leas    3,s                                     ; cut garbage
        rts
s2      cmpd    #$ffff                                  ; set flag
        orcc    #8                                      ; set neg
        leas    3,s                                     ; cut garbage
        rts
unsign  std     3,s
        sta     2,s                                     ; sign position
        asr     2,s                                     ; double the sign
        bpl     us2
        clra
        clrb
        subd    3,s
        std     3,s
us2     lda     7,s
        bpl     usr
        lda     2,s
        eora    #$80
        sta     2,s
        clra
        clrb
        subd    7,s
        std     7,s
usr     clra
        clrb
        rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   divide routine
;    enter with divisor in d
;   dividend on stack top
;   jsr div
;   remainder on top of stack
;   quotient in d
;   flags set as to d
;
div     leas    -3,s                                    ; make room
        bsr     unsign
        ldx     #17
        bra     div1
divs    rolb
        rola
div1    subd    1,s
        bmi     div3
        sec
div2    rol     a,s
        rol     5,s
        leax    -1,x
        bne     divs
        bra     div4                                    ; done
divr    rolb
        rola
        addd     1,s
        bpl      div2
div3    asl      6,s
        rol      5,s
        leax     -1,x
        bne      divr
        addd     1,s
div4    equ      *
; raw remainder   in d, quo in 5,8
        std      1,s                                    ; save it for a while
        asl      0,s                                    ; should we complement quo?
        bcc      ncq                                    ; no
        clra
        clrb
        subd     5,s
        std      5,s
ncq     asl      ,s+                                   ; should we complement remainder?
        bcc      ncr                                    ; no
        clra
        clrb
        subd     0,s
        std      0,s
ncr     ldx      0,s                                    ; get remainder
        ldd      4,s
        stx      4,s
        std      ,s++                                  ; set flags, discard store
        rts                                             ; and dissapear

txtbgn  equ     *
        rmb     1000                                    ; make some room for program
datas   rmb     bufend
eop     equ     *
        end     start
