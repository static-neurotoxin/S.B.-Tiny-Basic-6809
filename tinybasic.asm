; PALO ALTO TINY BASIC BY LI-CHEN WANG
;
; CONVERTED FOR  ’THE MILL’ OF STELLATION TWO
;   BY JAMES A. HINDS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RATS            MACRO
RETURN          SET             *
                RTS
                ENDM

HERE            MACRO
TMP             SET             *
                ORG             TABLE
                IFC             &1,,1
                FCC             &1
                FDB             TMP+$8000
                IFC             'GOTO',&1
                FCC             "GO TO"
                FDB             TMP+$8000
                ENDIF
TABLE           SET             *
                ORG             TMP
                ENDM

DISPAT          MACRO
                JSR             <EXEC
                FDB             TABLE
                ENDM

TSTC            MACRO
                JSR             <IGNBLK
                CMPA            #&1
                BNE             &2
                LEAY            1,Y
                ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VARIABLE AREA AT U

                ORG             0               ; OFFSET FROM U
VARBGN          EQU             -27*2           ; VARIABLE SPACE
OCSW            RMB             1               ; OUTPUT ON/OFF FLAG
FLDCNT          RMB             1               ; PLACES FOR NUMERIC OUTPUT
CURRNT          RMB             2               ; POINTS TO CURR LINE
TXTUNF          RMB             2               ; -> TO LAST OF PRG
STKGOS          RMB             0               ; SAVE OF S IN GOSUB
VARNXT          EQU             *
STKINP          RMB             2               ; SAVE S DURING INPUT
LOPVAR          RMB             2               ; FOR LOOP INDEX
LOPINC          RMB             2               ; FOR LOOP STEP
LOPLMT          RMB             2               ; LIMIT
LOPLN           RMB             2               ; LOOP BEGINNING LINE
LOPPT           RMB             2               ; LOOP PROG RESTART
SEED            RMB             2
HIWAT           RMB             2               ; END OF @ ARRAY
                RMB             2               ; EXTRA FOR BUFFER
BUFFER          RMB             72
BUFEND          EQU             *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PAGE ZERO SUBROUTINES OF 8080
;   ARE REPLACED ON 6809 BY DIRECT PAGE
;   SUBROUTINES AND CAN BE ACCESSED
;   BY 2 BYTE JSR
;   DIRECT PAGE IS PAGE 1 ($100-$1FF)
;   PAGE 0 CONTAINS TABLE OF COMMAND NAMES
;   AND THEIR CODE LOCATION EQUIV LOCATIONS
;   ENTRIES INTO THIS TABLE IS BY
;   MACRO "HERE"
                ORG             0
                LBRA            START
TABLE           SET             *
TMP             SET             0
                ORG             $100            ; LEAVE LOTS OF SPACE FOR TABLE
                SETDP           * >> 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TSTNUM RETURNS VALUE AND
;    LENGTH OF ASCII CODED INTEGER
;    IN TEXT. A NON-NUMBER WILL HAVE
;    A LENGTH OF ZERO.
TSTNUM          CLRB
                CLRA
                TFR             D,X             ; CLEAR X AND ACCOM
                PSHS            D,X             ; TOS= NUM AND NOS=#DIGITS
TN1             JSR             <IGNBLK
                SUBA            #'0
                BLO             NUMOUT          ; CHAR TOO LOW
                CMPA            #9
                BHI             NUMOUT          ; HIT CHAR THAT ISNT DIGIT
                LEAY            1,Y             ; BUMP TO NEXT CHAR
                PSHS            A               ; SAVE CHAR
                INC             4,S             ; INCREMENT DIGIT COUNT
                LDD             1,S             ; GET ACCUMULATOR
                CMPA            #$0F            ; CHECK IF ROOM
                BHI             QHOW            ; TOO BIG TO MULT BY 10
                ASLB
                ROLA                            ; DOUBLE
                ASLB
                ROLA                            ; TIMES 4
                ADDD            1,S             ; TIMES 5
                ASLB
                ROLA                            ; TIMES TEN
                ADDB            ,S+             ; ADD IN DIGIT
                ADCA            #O
                STD             0,S             ; STORE ACCOM BACK
                BPL             TN1             ; NO ERR? THEN GET NEXT DIGIT
QHOW            LBSR            ERROR
                FCC             'HOW?'
                FCB             $0D

NUMOUT          TST             3,S             ; SET FLAGS ON G DIGITS
                PULS            D,X,PC          ; AND GO AWAY
TVHOW           BVS             QHOW            ; DIRECT OVERFLOW TEST FOR ERROR
                RATS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FIN IS CALLED TO CONTINUE
;    EXECUTION OF NEXT STMT
;    ON SAME OR NEXT LINE.
;    HOWEVER, IF CURRENT STMT
;    HAS MORE TO PROCESS CONTROL
;    GOES BACK TO CALLER
FIN             TSTC            ';,FI1
                LEAS            2,S             ; VOID RETURN
                LBRA            RUNSML
FI1             CMPA            #$D
                BNE             RETURN
                LEAY            1,Y
                LEAS            2,S
                LBRA            RUNNXL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  ENDCHK IS USED TO VERIFY
;    A COMMAND IS TERMINATED BY
;    A CR BEFORE PERFORMING
;    THE COMMAND.
ENDCHK          TSTC            $D,QWHAT
                RATS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FINE USED TO CONTINUE EXECUTION
;    WITH NEXT STATEMENT. IF CURRENT
;    STATEMENT IS NOT AT END
;    A ’WHAT’ ERROR IS SIGNALLED
FINE            BSR             FIN             ; IF ERROR THEN FALL THROUGH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
QWHAT           LBSR            ERROR
                FCC             'WHAT?'
                FCB             $0D


QSORRY          LBSR            ERROR
                FCC             'SORRY'
                FCB             $0D

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IGNBLK    SCANS PAST ANY
;  SPACES AND RETURNS FIRST
;  CHAR WITH PROG PTR UPDATED
I1              LEAY            1,Y
IGNBLK          LDA             0,Y
                CMPA            #'              ; A BLANK
                BEQ             I1              ; GET NEXT
                RATS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  OUTPUT ROUTINES
;      SPACE        PUTS ONE BLANK
;      CRLF         PUTS CR
;      OUTC         PUTS CHAR IN MAILBOX
;         FOR 6502 TO GRAB AND
;         CLEAR MAILBOX
SPACE           LDA             #'              ; A SPACE
                BRA             OUTC
CRLF            LDA             #13
OUTC            EXG             A,A             ; GIVE 6502 SOME EXTRA CYCLES
                TST             $FBFF
                BNE             OUTC
                STA             $FBFF
                RATS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  INPUT ROUTINE
;    CHKIO RETURN ZERO IF NO
;    CHAR, OTHERWISE GRAB IT
;    RESTART BASIC IF IT IS
;    CONTROL-C
CHKIO           LDA             $FBFE           ; CHAR LOC
                BEQ             RETURN          ; IF NO CHAR JUST SAY SO
                CLR             $FBFE           ; OTHERWISE CLEAR IT
                CMPA            #$03            ; IS IT CNTL C
                BNE             RETURN          ; NO RETURN CHAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   START RELAY TO FIRST OF BASIC
;
START           LBRA            STI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TSTVE RETURN ADDRESS AND
;     VALUE OF VARIABLE
TSTVE           BSR             IGNBLK
                SUBA            #'@
                BCS             RETURN
                BEQ             RLYAT           ; IS STRING SPACE
                CMPA            #27             ; IS IN ALPHABET??
                BGE             TVN
                LEAY            1,Y
                ASLA    
                NEGA    
                LEAX            A,U
                LDD             ,X
                CLC     
                RATS            RETURN          ; WITH CARRY CLEAR
RLYAT           LBRA            ATSIGN
TVN             SEC                             ; SET CARRY
                RATS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   EXEC AND TABLE DISPATCH
;
;      SCAN TABLE FOR KEYWORD
;       JUMP 1ro ASSOCIATED CODE
EXEC            JSR     <IGNBLK
                LDX     [0,S]                   ; GET TABLE ADDRESS FROM CALLER
                STY     0,S
LCT             LDA     0,Y+
                CMPA    0,X+                    ; MATCH PRG AGAINST TABLE
                BEQ     LCT
                LDA     -1,X                    ; LOOK AT MISMATCH
                BMI     EXECF                   ; FOUND!
                LDA     -1,Y                    ; IS IT AN ABBREV
                CMPA    #'.
                BEQ     EXECF                   ; YES! FORCE MATCH
                LDY     0,8                     ; RESTORE PROG PTR
EXEC1           LDA     0,X+
                BPL     EXEC1                   ; SKIP REST OF KEYWORD IN TABLE
                LDA     0,X+
                BRA     LCT                     ; RETRY
EXECP           LDA     ,X+
                BPL     EXECP
                LEAY    1,Y
EXECF           LEAY    —1,Y                    ; BACK UP PRG PTR
                LDB     0,X                     ; NEXT TRANSFER BYTE
                ANDA    #$7F
                LEAX    0,PCR
                LEAX    D,X
                STX     0,S
                RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXPRESSION ANALYZER
;
EXPR            BSR     EXPR2
                PSHS    X                       ; SAVE LEFT HAND VALUE
                DISPAT
                HERE   '>='
                BSR    XP18
                BGT    ZER                      ; M>=R
                BRA    ONE

                HERE   '='
                BSR    XP18                     ; M=R
                BEQ    ONE
                BRA    ZER

                HERE    '>'
                BSR     XP18                    ; M>R
                BLT     ONE
                BRA     ZER

                HERE    '<='
                BSR     XP18                    ; M<=R
                BGE     ONE
                BRA     ZER

                HERE    '='
                BSR     XP18
                BEQ     ONE
                BRA     ZER

                HERE    '<'
                BSR     XP18                    ; M<R
                BLS     ZER
ONE             INCB
ZER             STD     ,S                      ; TO SET FLAGS

                HERE    .
                LDD    ,S++                     ; POP AND SET FLAGS
                RATS   NO                       ; LOGICAL OP

XP18            BSR     EXPR2
                CMPX    2,S                     ; COMPARE WITH LEFT
                PSHS    CC
                CLRA
                CLRB                            ; SET D TO ZERO (FALSE)
                PULS    CC,PC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  EXPR2 LOOKS AFTER + AND -
;        BINARY OPERATORS
EXPR2           BSR     EXPR3                   ; GO AFTER LEFT HAND OPERAND
EXPR21          PSHS    D
                JSR     <IGNBLK
                CMPA
                BNE     XP290
                LEAY    +1,Y
                BSR     EXPR3
                ADDD    ,S++                    ; ADD IN LEFT
XP235           BVC     EXPR21                  ; AND AGAIN
                JMP     QHOW                    ; OVERFLOW
XP290           CMPA    #'-
                BNE     XP291
                LEAY    1, Y
                BSR     EXPR3
                PSHS    D
                LDD     2,S
                SUBD    0,S
                LEAS    4,S                     ; POP GARBAGE
                BRA     XP235                   ; CHECK FOR OVERFLOW

XP291           PULS    X,PC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXPR3 HANDLES THINGS WITH
;        / AND * IN THEM
;
EXPR3           BSR     EXPR4
                PSHS    D
EXP3EL          DISPAT
                HERE    '*'
                BSR     EXPR4
                LBSR    MULT
                BED     EXP3EL                  ; SIGNAL ERROR IF D REG HAS SIGNIFANT BITS
EXP3ER          JMP     <QHOW
                HERE    '/'
                BSR     EXPR4
                STD     -2,S                     ; SET FLAGS
                BEQ     EXP3ER
                LBSR    DIV
                STD     0,S                      ; REPLACE REM BY QOU
                BRA     EXP3EL
                HERE    ,
                PULS    D,PC                    ; GET RESULT AND RETURN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXPR4 HANDLES FUNCTIONS AND
;     UNARY MINUS AND REPRESENTS
;     THE HIGHEST PRIORITY
;     IF THERE ARE (<)) THEN EXPR
;     IS CALLED (OH! NO!) RECURSIVLY
;
EXPR4           DISPAT ON                       ; FUNCTIONS
                HERE    'RND'
                LDD     SEED,U
                PSHS    D
                LDD     #%100011
                LBSR    MULT
                PULS    D
                ADDD    #$O123
                ANDA    #47F                    ; MASK SIGN
                STD     SEED,U
                PSHS    D                       ; SAVE FOR DIVISION
                BSR     FARM
                LBLE    QHOW                    ; IF ZERO OR NEG
                LBSR    DIV
                PULS    D,PC                    ; GET REMAINDER AND RETURN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   ABSOLUTE VALUE
;
                HERE    'ABS'
                BSR     PARM                    ; GET THE VALUE
                BPL     RETURN
                PSHS    D
                CLRA
                CLRB
                SUBD    S++
                JMP     TVHOW                   ; IMPLIED RETURN IF NO OVERFLOW

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SIZE HOW MUCH STORAGE LEFT
;    BETWEEN STACK AND END OF
;    PROGRAM
                HERE    'SIZE'
SIZE            TFR     S,D
                SUBD    HIWAT,U                 ; SUBTRACT END OF STRING ADDR
                RATS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UNARY MINUS HANDLED AS FNCT
                HERE    '-'                     ; UNARY MINUS
                JSR    <EXPR
                COMA
                NEGB
                SBCA   #0
                JMP    <TVHOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CHECK FOR NUMBER,    VARIABLE OR
;  SUBEXPRESSION
;
                HERE    ,
                JSR     T6TVE                   ; NOT A FUNCTION MUST BE VAR
                BCC     RETURN
                JSR     CTSTNUM                 ; NOT VAR TRY NUM
                BNE     RETURN                  ; DIGIT CNT<>0 MEANS NUMBER
FARM            TSTC    '<,XP43
                JSR     <EXPR
                PSHS    CC,D                    ; SAVE CONDITIONS OF EXPRESSION
                TSTC    '),XP43
                PULS    CC,D,PC                 ; FETCH CONDITIONS AND RETURN
XP43            JMP     QWHAT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GET INDEX INTO STRING SPACE
; IF WE GO PASSED THE CURRENT
; HIGH WATER MARK WE BUMP THAT
; TO KEEP STACK FROM GOING
; BANANAS
;
ATSIGN          LEAY    1,Y
                BSR     FARM                    ; GET THE INDEX
                LBMI    SHOW
                LDX     TXTUNF,U                ; GET END OF PROG
                LEAX    2,X
                LEAX    D,X
                LEAX    D,X
                CMPX    HIWAT,U
                BLS     HIWOK
                STX     HIWAT,U
HIWOK           BSR     SIZE
                LBMI    QSORRY                  ; NO ROOM
                CLC
                LDD     0,X                     ; GET VALUE AT INDEX
                RATS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DIRECT STATEMENT EXECUTE
DIRECT          LEAY   2,Y                      ; STEP PAST LINE NUMBER
                DISPAT  AND                     ; GO TO IT!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LIST PROGRAM POSSIBLY FROM
;      A GIVEN LINE NUMBER, OTHERWISE
;      THE WHOLE SHEBANG
;
                HERE    'LIST'
                JSR     <TSTNUM                 ; TEST IF A LN #
                PSHS    D
                JSR     <ENDCHK                 ; IF NO # THEN USE 0
                PULS    D
                LBSR    FNDLN                   ; FIND IT
LS1             LBHI    START                   ; HIT END?
                LBSR    PRTLN                   ; PRINT THE LINE
                JSR     <CHKIO                  ; USER INTERRUPT?
                LBSR    FNDLNP                  ; FIND NEXT LINE
                BRA     LSI

                HERE    'NEW'
                JSR     <ENDCHK                 ; ABORT IF NOT ONLY THING IN STMT
                LEAX    TXTBGN-1,PCR
                STX     TXTUNF.U                ; RESET BEGINNING OF PROGRAM PTR
                STX     HIWAT,U                 ; AND HI WATER STRING
                JMP     <START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  RUN----- FIRE THE ROCKETS AND
;         BLAST OFF
;     EACH STATEMENT IS RESPONSIBLE
;    FOR COMING BACK TO RUNNXL,
;    RUNNNL,RUNTSL AND RUNSML
;    AS PROGRAM POINTER <V) NEEDS
;    CARE AND FEEDING
                HERE    'RUN'
                JSR     <ENDCHK
                LEAY    TXTBGN,PCR              ; FIRST LINE
RUNNXL          CLRA
                CLRB                            ; START AT ANY LINE >0
RUNNNL          LBSR    FNDLNP
                LBHI    START
RUNTSL          STY     CURRNT,U
                LEAY    2,Y                     ; BUMP PASSED LINE NUM
RUNSML          JSR     <CHKIO                  ; RUN SAME LINE
                DISPAT  START                   ; DOING STATEMENT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   STOP WILL CAUSE GRACEFUL
;    TERMINATION
;
                HERE    'STOP'
                JSR     <ENDCHK
                JMP     <START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   PRINT PRINT OUT EXPRESSIONS,
;       STRINGS. WITH POSSIBLE
;        FORMAT FOR DIGITS
;
                HERE    'PRINT'
                LDA     #6
                STA     FLDCNT,U
                JSR     <IGNBLK
                CMPA    #';
                BNE     PR2
                LEAY    1,Y
                JSR     <CRLF
                BRA     RUNSML
PR2             CMPA    #$0D
                BNE     PRO
                LEAY    1,Y
                JSR     <CRLF
                BRA     RUNNXL
PRO             CMPA    #'#                     ; IS IT A FORMAT LENGTH?
                BNE     PR1
                LEAY    1,Y
                JSR     <EXPR
                STB     FLDCNT,U
                BRA     PR3
PR1             LBSR    QTSTG                   ; PRINT POSSIBLE STRING
                BEQ     PR3
                JSR     <EXPR
                LBSR    PRTNUM
PR3             JSR     <IGNBLK
                CMPA    #',
                BNE     PR6
                LEAY    1,Y
                JSR     <FIN                    ; IF NOT ENDLINE THEN COME BACK
                BRA     PRO
PR6             JSR     <CRLF                   ; END OF STATEMENT WITH
                JMP     <FINE                   ; IMPLIED CR AND LF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    GOTO
;
                HERE    'GOTO'
                JSR     <EXPR
                PSHS    Y                       ; SAVE TXT POINTER FOR ERROR SUB
                LBSR    FNDLN
                BNE     GOERR                   ; COULDN’T FIND DESTINATION
                LEAS    2,S                     ; DISCARD OLD LINE NUM
                BRA     RUNTSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    IF
;
                HERE    'IF'
                JSR     EXPR                    ; EVALUATE THE EXPRESSION
                BNE     RUNSML                  ; IF <> 0 THEN CONT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      FALL THROUGH TO
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   REM    IGNORE THIS WHOLE LINE
;
                HERE    'REM'                   ; LOOKS LIKE ’IF 0’
                LDY     CURRNT,U
                LDD     #1
                ADDD    0,Y      CURRENT LINE PLUS ONE
                BRA     RUNNNL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   INPUT
INPERR          LDS     STKINP,U                ; REGAIN
                PULS    D,X,Y                   ; CURRNT,JUNK,TEXT PTR
                STD     CURRNT,U                ; AND RETRY THIS INPUT STMT
                HERE    'INPUT'
IP1             PSHS    Y                       ; SAVE TEXT POINTER
                LBSR    QTSTG                   ; PRINT QUOTED PROMPT
                BNE     IP2                     ; NO TRY VARIABLE
                JSR     TSTVE
                BCS     IP4
                BRA     IP3                     ; YES INPUT VARIABLE
IP2             PSHS    Y                       ; SAVE FOR PRTSTG
                JSR     TSTVE
                LBCS    QWHAT
                LDA     ,Y
                CLR     ,Y                      ; SET TO ZERO AS STOPPER
                PULS    Y
                PSHS    A
                LBSR    PRTSTL                  ; PRINT PROMPT
                PULS    A
                STA     ,-Y                     ; REPLACE CHAR OF PROG
IP3             TFR     X,D                     ; REGAIN ADDRESS OF VAR
                PSHS    Y
                LDX     CURRNT,U
                PSHS    X
                LEAX    IP3.PCR                 ; POINT CURRNT TO NG AS INPUT FLAG
                STX     CURRNT,U
                STS     STKINP,U                ; SAVE STACK POINTER
                PSHS    D                       ; THE DESTINATION ADDRESS
                LDA     #':                     ; PROMPT
                LBSR    GETLN                   ; GET THE INPUT FROM USER
                LEAY    BUFFER,U
                JSR     EXPR                    ; AND EVALUATE IT AS AN EXPRESSION
                STD     [0,S++]                 ; SAVE VALUE IN VAR
                PULS    X
                STX     CURRNT,U
                PULS    Y
IP4             PULS    D                       ; PURGE JUNK
                TSTC    "',",IPS                ; IS NEXT A COMMA
                BRA     IP1                     ; YES, DO ANOTHER VARIABLE
IPS             CMP     <FINE                   ; FINISH

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LET      THE    ASSIGNMENT STATEMENT
;
                HERE    'LET'
LET             LBSR    SETVAL                  ; DO THE ASSIGNMENT
                TSTC    IP5                     ; CHECK END LINE
                BRA     LET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GOSUB
;
GOERR           PULS    Y                       ; REGAIN CURRENT LINE
                CMP     <QHOW                   ; AND PRINT DIAGNOSTIC
                HERE    'GOSUB'
                LBSR    PUSHA                   ; SAVE FOR PARAMETERS
                JSR     <EXPR                   ; GET VAL IN D
                PSHS    Y                       ; SAVE TEXT POINTER
                LBSR    FNDLN
                BNE     GOERR                   ; NO LINE SAY 'HOW'
                LDD     CURRNT,U
                PSHS    D
                LDD     STKGOS,U
                PSHS    D
                CLRA
                CLRB
                STD     LOPVAR,U                ; SAVE NEW ONES
                STS     STKGOS,U
                LBRA    RUNTSL                  ; AND RUN DEST LINE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     RETURN      ROM GOSUB
;
                HERE    'RETURN'
                JSR     <ENDCHK                 ; BAD IF NOT END OF LINE
                LDD     STKGOS,U                ; RELOAD STACK POINTER
                LBEQ    QHOW                    ; TOO MANY RETURNS
                TFR     D,S                     ; RESTORE STACK POINTER
                PULS    D,X,Y
                STD     STKGOS,U
                STX     CURRNT,U
                LBSR    POPA                    ; RESTORE FOR—NEXT ENVIRONMENT
                CMP     <FINE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NEXT
;
                HERE    'NEXT'
                LDX     LOPVAR,U                ; IF NO FOR THEN
                BEQ     NEXT2                   ; COMPLAIN LOUDLY
                JSR     <TSTVE
                BCC     NEXTO                   ; IF NO VARIABLE SPECIFIED
                LDX     LOPVAR,U                ; THEN UST USE LATEST FOR VARIABLE
NEXTO           CMPX    LOPVAR,U
                BEQ     NEXT3
                LBSR    POPA
                BNE     NEXTO
NEXT2           CMP     <QHOW
NEXT3           LDD     0,X                     ; AND PUT VALUE INTO ACCOM
                ADDD    LOPINC,U
                STD     0,X
                TST     LOPINC,U                ; WHICH DIRECTION ARE WE GOING?
                SMI     NEXT4
                CMPD    LOPLMT,U
                BGT     NEXT5                   ; DONE
NEXT6           LDX     LOPLN,U
                STX     CURRNT,U
                LDY     LOPPT,U
                CMP     <FINE
NEXT4           SUBD    LOPLMT,U
                BGE     NEXT6
NEXT5           LBSR    POPA
                JMP     <FINE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FOR
;
                HERE    'FOR'
                BRA     FORE                    ; JUMP AROUND DEFAULT STMT CODE
;   TO ALLOW TABLE ENTRY
;      FOR ASSIGNMENT TO BE
;      PRIOR TO KEYWORDS OF
;      FOR STATEMENT IN TABLE
;      THIS IS REQUIRED BY THE
;      MACRO HERE AND THE WAY IT
;      WORKS
                HERE    ,                       ; DEFAULT ASSIGNMENT
                JSR     <FIN                    ; CAN BE EMPTY
                LBRA    LET                     ; OTHERWISE TREAT AS LET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ACTUAL CODE OF FOR
;
FORE    LBSR            PUSHA                   ; SAVE LOOP VARS
        BSR             BETVAL
        STX             LOPVAR,U                ; SAVE ADDR OF LOOP VAR
        DISPAT
        HERE            'TO'                    ; 'FOR'1-1 'TO'
        JSR             <EXPR
        STD             LOPLMT,U                ; STOPPING VAL
        BRA             FOR2                    ; JUMP AROUND DEFAULT
        HERE            ,                       ; DID NOT HAVE 'TO'
        JMP             <QWHAT                  ; SO COMPLAIN
FOR2    DISPAT
        HERE            'STEP'
        JSR             <EXPR
        BRA             FOR3
        HERE            ,                       ; NO STEP VALUE
        LDD             #1                      ; SO DEFAULT TO ONE
FOR3    STD             LOPINC,U
        LDX             CURRNT,U
        STX             LOPLN,U                 ; LOOP LINE NUMBER
        STY             LOPPT,U                 ; LOOP PROG POINTER
        LEAY            -10,S                   ; START LOOKING AT OLD ’PUSHA’ RECOFDS
FOR7    LEAY            10,Y
        LDX             0,Y                     ; GET LOOP VAR ADDRESS
        BEQ             FOR8                    ; EXHAUSTED ALL FORS
        CMPX            LOPVAR,U                ; IS OLD SAME AS THIS VAR?
        BNE             FOR7
        LEAX            +10,Y                   ; FOUND MATCH MUST DELETE IT
        STS             ,—S                     ; SAVE STACK POINTER
FOR7L   LDA             ,-Y
        STA             ,-X
        CMPY            0,3
        BGT             FOR7L
        LEAS            2, X                    ; CUT BACK STACK
FOR8    LDY             LOPPT,U                 ; GET PROG POINTER
        CMP             <FINE
SETVAL  LBSR            TSTVE                   ; MUST BE VARIABLE
        BCS             QW1                     ; NO VARIABLE
        PSHS            X
        TSTC            '=,QW1
        JSR             <EXPR                   ; EVALUATE THE EXPRESSION
        PULS            X
        STD             0,X                     ; LEAVE ADDRESS IN X     <FOR USES IT>
        RATS
QW1     JMP             <QWHAT                  ; RELAY TO QWHAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UTILITIES AN! EXTRAS
;
; FINDLINE LINE NUMBER IN D
FNDLN   TSTA
        LBMI            QHOW                    ; NEGATIVE LINE NUM
        LEAY            TXTBGN,PCR              ; INITIAL LINE PTR
FNDLNP  CNPY            TXTUNF,U                ; PASSED END?
        BHI             RETURN
        CMPD            0,Y                     ; LOOK AT LINE NUMBER
        BLS             RETURN                  ; IF REG <= LINE NUM
FNDNXT  EQU             *
        CNPY            TXTUNF,U                ; DONT SCAN PAST END
        BHI             RETURN
        LEAY            2,Y
        PSHS            A                       ; SAVE LINE NUMB HIGH BYTE
        LDA             #13
F1      CMPA            ,Y+
        BNE             F1                      ; SCAN TO CR OF LINE
        PULS            A                       ; REGAIN HIGH LINE NUM
        BRA             FNDLNP                  ; TRY THIS LINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GETLN      GE1 INPUT LINE AND
;            EDIT ACCORDING TO
;            BACKSPACE, CNTL X

GETLN   JSR             <OUTC                   ; PROMPT
        LEAY            BUFFER,U                ; START AT BEGINNING
        LEAX            BUFEND,U
        PSHS            Y,X
GL1     JSR             <CHKIO
        BEQ             GL1                     ; NOTHING YET
        JSR             <OUTC                   ; ECHO
        CMPA            #$OA                    ; LINE FEED
        BEQ             GL1                     ; IGNORE IT
        CMPA            #$1F&'H                 ; DELETE CODE
        BEQ             GL3
        CMPA            #$1F&'X                 ; CONTROL X
        BEQ             GL4
        STA             ,Y+
        CMPA            #$D
        BEQ             OLD                     ; RETURN
        CMPY            S                       ; WITH BUFFER END
        BNE             GL1
GL3     CMPY            2,8                     ; ANYTHING IN BUFFER?
        BEQ             GL4
        LEAY            -1,Y
        JSR             <SPACE
        LDA             #$1F&'H                 ; CONTROL H
        JSR             <OUTC                   ; AND BACK UP OVER SPACE
        BRA             GL1
GL4     JSR             <CRLF
        LEAS            4,5                     ; POP OFF TEMPORARIES
        LDA             #'?                     ; QUESTION MARK PROMPT
        BRA             GETLN
GLO     LEAS            4,S
        STA             2,Y                     ; THROW ANOTHER RETURN
        RATS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PRTNUM AND FRIENDS
;
PRTNUM  LDX             #10
        PSHS            D,Y,X                   ; SAVE PRG POINTER AND STOPPER
        LDB             FLDCNT,U
        DECB
        ROLA                                    ; PUT SIGN OF QUANTITY INTO LOW B
        ROLB
        SEX
        TFR             D,Y                     ; PUT INTO COUNTER

        LDD             0,S++                   ; SET FLAGS
        BPL             PN2                     ; COMPLEMENT IF NEGATIVE
        COMA
        COMB
        ADDD            #1
PN1     LEAY            -2,Y                    ; DECREMENT PLACE COUNT
PN2     PSHS            D                       ; SAVE QUOTIENT
        LDD             #10                     ; DIVIDE BY 10 TO GET
        LBSR            DIV                     ; NEXT DIGIT
        BNE             PN1                     ; HAVE NOT GOT ALL
        TFR             Y,D                     ; PUT PLACE COUNT INTO B
        BRA             PN3
PN4     JSR             <SPACE                  ; PRINT SPACE
PN3     SUBB            #2                      ; DEC SPACE COUNT        
        BPL             PN4
        RORB                                    ; PUT SIGN BIT INTO CARRY
        BCC             PN6                     ; DONT PRINT MINUS
        LDA             #'-                     ; LEADING MINUS
PN8     JSR             <OUTC
PN6     PULS            D                       ; GET REMAINDER
        CMPB            #10                     ; DONE?
        BEQ             PN7
        TFR             B,A
        ORA             #$30
        BRA             PN8
PN7     PULS            Y,PC                    ; AND RETURN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PRTLN     PRINT LINE OF BASIC
;               PROGRAM                                                                
;
;                                                                                  

PRTLN   LDB             #4                      ; FOUR SPACES
        STB             FLDCNT,U
        LDD             ,Y++                    ; GET LINE NUMBER
        BSR             PRTNUM                  ; AND PRINT IT
        JSR             <SPACE
PRTSTL  CLRB                                    ; FORCE NO MATCH

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  AND FALL INTO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PRTSTG    PRINT STRING
;
;

PRTSTG  CMPB            ,Y+                     ; PRINT STRING AT Y
        BEQ             RETURN                  ; IF SAME AS STOPPER
        LDA             -1,Y                    ; GET THAT CHAR
        JSR             <OUTC
        CMPA            #13
        BNE             PRTSTG                  ; NOT A CR SO GO
        TSTA                                    ; IF RETURN THEN NON ZERO
        RATS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PRINT QUOTED    STRING
;   RETURN WITH   NOT ZERO IF DIDNT RECOGNIZE AS OUR STUFF

QTSTG   TSTC            '",QT3                  ; DOUBLE QUOTE
        LDB             #'"
QT1     BSR             PRTSTG
        LBNE            RUNNXL                  ; RUN NEXT LINE IF HIT RETURN
        RATS            RETURN                  ; W ZERO SET
QT3     TSTC            $27,QT4                 ; SINGLE QUOTE
        LDB             ##27                    ; PRINT TILL HATCH
        BRA             QT1
QT4     TSTC            '',RETURN
        LDA             #$8D                    ; FUNNY RETURN
        JSR             <OUTC                   ; APPLE DOES NOT SUPPORT THIS
        CLRA                                    ; SET ZERO STATUS
        RATS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ERROR REPORT
ERROR   PSHS    Y                               ; SAVE LINE NUHBER
        LDY     2,S                             ; GET THE RETURN ADDR
        BSR     PRTSTL                          ; PRINT HOW OR WHAT
        PULS    Y
        LDA     0,Y
        PSHS    A                               ; SAVE THE CHAR AT ERROR POINT
        CLR     0,Y
        LDY     CURRNT,U                        ; IS THIS IN
        LDD     0,Y                             ; IMMEDIATE MODE?
        BEQ     ERRO
        LBNI    INPERR                          ; INPUT COMMAND??
        BSR     PRTLN                           ; PRINT UP TO ZERO
        LDA     #'?
        JSR     <OUTC                           ; PRINT QUESTION
        PULS    A
        STA     0,-Y                            ; RESTORE LINE
        LEAS    2,8                             ; POP PHONY RETURN
        BSR     PRTSTL
ERRO    JMP     <START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      POPA AND PU8HA
;
;
POPA    LDD     2,S
        BEQ     POPAO
        STD     LOPVAR,U
        LDD     4,S
        STD     LOPINC,U
        LDD     6,S
        STD     LOPLMT,U
        LDD     8,8
        STD     LOPLN,U
        LDD     10,S
        STD     LOPPT,U
        PULS    D
        LEAS    10,S
        ANDCC   #SFB                            ; TURN OFF ZERO
        TFR     D,PC
POPAO   STD     LOPVAR,U
        PULS    D
        LEAS    2,S
        TFR     D,PC

PUSHA   LBSR  SIZE                              ; ANU ROOM?
        SUBD   #12
        LBLS   QSORRY
        PULS   D                                ; RETURN ADDR
        LDX    LOPVAR,U                         ; TEST THIS
        BEQ    PU1
        LDX    LOPPT,U
        PSHS   X
        LDX    LOPLN,U
        PSHS   X
        LDX    LOPLMT,U
        PSHS   X
        LDX    LOPINC,U
        PSHS   X
        LDX    LOPVAR,U
PU1     PSHS   X
        TFR    D,PC                             ; AND RETURN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; STI (ALIAS START)
;    INITIALIZE AND ENTER COMMAND
;     MODE
;
OK      FCC    'OK',13
STI     LEAX   TSTNUM,PCR
        TFR     X,D
        TFR     A,DP
        LEAU    DATAS,PCR
        LEAS    VARBGN,U
        JSR     CCRLF
        LEAY    OK,PCR                          ; PRINT OK AND PROMPT
        LBSR    PRTSTL
        CLRA                                    ; B IS ALREADY ZERO
        STD     LOPVAR,U                        ; NO CURRENT FOR LOOPS
        STD     STKGOS,U                        ; NOR GOSUB RETURNS
ST3     LDA     #'>                             ; PROMPT TO ENTER A STATEMENT
        LBSR    GETLN
        PSHS    Y                               ; SAVE POINTER TO END OF TEXT
        LEAY    BUFFER,U                        ; POINT TO START
        JSR     <TSTNUM                         ; WILL RETURN ZERO IF NO NUM
        PSHS    CC,D
        JSR     <IGNBLK
        LDD     1,S                             ; REGAIN LINE NUM
        STD     ,—Y                             ; PUT LINE NUMBER IN FRONT
        STY     CURRNT,U                        ; MAKE THE CURRENT LINE
        PULS    CC,D
        LBEQ    DIRECT                          ; DO IMMEDIATE NODE IF NO LEADING NUMBER
; Y POINTS TO BEGINNING OF LINE
; AND TOP OF STACK POINTS TO END
        PGH8   Y
        LBSR   FNDLN
        PSHS   Y                                ; TOS->TEXT AREA
        BNE    ST4                              ; INSERT
        LBSR   FNDNXT
        LDX    0,S                              ; BEGINNING OF LINE TO DELETE
        BRA    STL
STLE    LDA    ,Y+
        STA    ,X+
STL     CMPY   TXTUNF,U                         ; ARE WE DONE MOVING?
        BLS    STLE
        LEAX   -1,X
        STX    TXTUNF,U                         ; UPDATE END POINTER
        STX    HIWAT,U                          ; AND STRING SPACE
ST4     LDD    4,S                              ; POINT TO END OF LINE
        SUBD   2,S                              ; FIND IF LENGTH OF LINE IS EMPTY
        CMPD   #3
        BEQ    STI                              ; YES, DO NOT INSERT
; MAKE ROOM FOR NEW LINE
        LDY     TXTUNF,U
        LEAX    D,Y                             ; ADD IN LENGTH
        STX     TXTUNF,U                        ; UPDATE END POINTER
        STX     HIWAT,U
        LDA     GOOD
        STA     0,X
ST2LE   LDA     ,-Y
        STA     ,-X
        CMPY   0,S                              ; HAVE WE REACHED BEGINNING?
        BHI    ST2LE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; START MOVING LINE
        PULS    Y

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  X POINTS TO END OF LINE AREA
;  Y POINTS TO BEGINNING OF LINE
;  TOS POINTS TO BEGINNING OF TEXT
;  N06 POINTS TO END OF BUFFER
        PULS    X
ST3LE   LDA     ,X*
        STA     ,Y*
        CMPX    ,8
        BLO     ST3LE
        PULS    X                               ; DISCARD GARBAGE
        BRA    ST3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MULTIPLY AND DIVIDE ROUTINES
; CALLING SEQUENCESl
; MULT   LDD ^MULTIPLIER
;        PSHS D
;        LDD ^MULTIPLICAND
;        LBSR MULT
; ON EXIT D HAS HIGH ORDER BITS
; T06 HAS LOW )RDER BITS
; EQAL ZERO IT TRUE IFF THE
; CONTEDTB OF D ARE THE SAME
; AS THE HIGH HT OF TOS
;    NEG IS SET ON THE SIGN
; OF   TOS
MULT    LEAS    -3,8
        BSR     UNSIGN                                  ; RETURNS W/D=0
        LDX     +16
        ROR     5,S
        ROR     6,S
M1      BCC     M2
        ADDD    1,S
M2      RORA
        RORB
        ROR     5,S
        ROR     6,S
; RESULT IS NOW IN D CAT 5,S
        LEAX    ,-x
        BNE     Ml
SIGN    TST     0,S                                     ; SIGN LOC
        BPL     SETFLG
        COMA
        COMB
        COM     5,S
        NEG     A,S
        BNE     SETFLG
        INC     5,S
        BNE     SETFLG
        INCB
        BNE     SETFLG
        INCA
SETFLG  TST     5,S
        BMI     S2
        STD     -2,S                                    ; SET FLAGS
        ANDCC   #$F7                                    ; SET POSITIVE
        LEAS    3,S                                     ; CUT GARBAGE
        RTS
S2      CMPD    #$FFFF                                  ; SET FLAG
        ORCC    #8                                      ; SET NEG
        LEAS    3,S                                     ; CUT GARBAGE
        RTS
UNSIGN  STD     3,S
        STA     2,S                                     ; SIGN POSITION
        ASR     2,S                                     ; DOUBLE THE SIGN
        BPL     U82
        CLRA
        CLRB
        SUBD    3,S
        STD     3,S
US2     LDA     7,S
        BPL     USR
        LDA     2,S
        EORA    #$80
        STA     2,S
        CLRA
        CLRB
        SUBD    7,S
        STD     7,S
USR     CLRA
        CLRB
        RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   DIVIDE ROUTINE
;    ENTER WITH DIVISOR IN D
;   DIVIDEND ON STACK TOP
;   JSR DIV
;   REMAINDER ON TOP OF STACK
;   QUOTIENT IN D
;   FLAGS SET AS TO D
;
DIV     LEAS    -3,S                                    ; MAKE ROOM
        BSR     UNSIGN
        LDX     #17
        BRA     DIV1
DIVS    ROLB
        ROLA
DIV1    SUBD    1,S
        BMI     DIV3
        SEC
DIV2    ROL     A,S
        ROL     5,S
        LEAX    -1,X
        BNE     DIVS
        BRA     DIV4                                    ; DONE
DIVR    ROLB
        ROLA
        ADDD     1,S
        BPL      DIV2
DIV3    ASL      6,S
        ROL      5,S
        LEAX     -1,X
        BNE      DIVR
        ADDD     1,S
DIV4    EQU      *
; RAW REMAINDER   IN D, QUO IN 5,8
        STD      1,S                                    ; SAVE IT FOR A WHILE
        ASL      0,S                                    ; SHOULD WE COMPLEMENT QUO?
        BCC      NCQ                                    ; NO
        CLRA
        CLRB
        SUBD     5,S
        STD      5,S
NCQ     ASL      0,S+                                   ; SHOULD WE COMPLEMENT REMAINDER?
        BCC      NCR                                    ; NO
        CLRA
        CLRB
        SUBD     0,S
        STD      0,S
NCR     LDX      0,S                                    ; GET REMAINDER
        LDD      4,S
        STX      4,S
        STD      0,S++                                  ; SET FLAGS, DISCARD STORE
        RTS                                             ; AND DISSAPEAR

TXTBGN  EQU     *
        RMB     1000                                    ; MAKE SOME ROOM FOR PROGRAM
DATAS   RMB     BUFEND
EOP     EQU     *
        END     START
