# Santa Barbara Tiny BASIC for 6809's

## By James A. Hinds

### Original Introduction to Palo Alto Tiny BASIC by Li-Chen Wang

DDJ is pleased So present this version of Tiny BASIC for
the 6809. It is a direct descendant of Li-Chen Wang’s Palo Alto
Tiny BASIC (refer to DDJ #5) which its author, James A.
Hinds, has named Santa Barbara Tiny BASIC.
SBTB is an example of a transliteration of a large 8080 pro­gram,
and contains a number of 6809 techniques which should
be useful to those who want to get familiar with the 6809. The
program was developed on an Apple II with The Mill (which
puts the 6809 under the hood) and Technical Systems Consul­tants'
operating system, FLEX.

The following introductory paragraphs are the same as
those printed with the original Palo Alto Tiny BASIC. The few
differences in documentation can be noted as follows: in this
version, there is no `Control 0` function; `NEXT` statements do
not require a variable name; there is no `overprint` function; and
no spaces are allowed in the input language except in `GO TO`
commands.

In addition, this program is completely relocatable on a
page basis to any position in memory. It requires about the
same amount of space as did Li-Chen Wang’s PA TB. - Ed.

* Numbers. All numbers are integers and must be less than 32767.

* Variables. There are 26 variables denoted by letters `A`
    through `Z`. There is also a single array `@(I)`. The dimension of
    this array is set automatically to make use of all the memory
    space that is left unused by the program. (I.e., 0 through
    `SIZE / 2`, see `SIZE` function below.)
    
* Functions. There are 3 functions:
* `ABS(X)` gives the absolute value of X.
* `RND(X)` gives a random number between 1 and X (inclusive).
* `SIZE` gives the number of bytes left unused by the program.

## Arithmetic and Compare Operators

| Operator | Description                         |
|----------|-------------------------------------|
| /        | divide.                             |
| *        | multiply.                           |
| -        | subtract.                           |
| +        | add.                                |
| >        | greater than (compare).             |
| <        | less than (compare).                |
| =        | equal to (compare).                 |
| #        | not equal to (compare).             |
| >=       | greater than or equal to (compare). |
| <=       | less than or equal to (compare).    |

+,  *, and / operations result in a value between -32767 and 32767
(-32768 is also allowed in some cases.) All compare
operators result in a 1 if true and a 0 if not true.

## Expressions

Expressions are formed with numbers, varia­bles, and functions with arithmetic and compare operators be­tween them. + and - signs can also be used at the beginning of
an expression. The value of an expression is evaluated from left
to right, except that * and / are always done first, and then +
and -, and then compare operators. Parentheses can also be used
to alter the order of evaluation. Note that compare operators
can be used in any expression. For example:

```Basic
10 LET A= (X > Y) * 123 + (X = Y) * 456 + ( X < Y) * 789
20 IF (U = 1) * (V < 2) + (U > V) * (U < 99) * (V > 3) PRINT "YES"
30 LET R = RND(100), A = (R > 3)+(R > 15)+(R > 56)+ (R > 98)
```

In statement 10, A will be set to 123 if `X > Y`, to `456` if `X = Y`,
and to `789` if `X < Y`. In statement 20, the operator acts like
a logical `AND`, and the `+` operator acts like a logical `OR`. In
statement 30, `Y` will be a random number between `0` and `4` with
a prescribed probability distribution of:

* 3% of being 0,
* `15-3`  = 12% of being 1,
* `56-15` = 41% of being 2,
* `98-56` = 42% of being 3,
* `100-98` = 2% of being 4.

## Direct Commands

All the commands described later can be
used as direct commands except the following three; they can
only be used as direct commands and not as part of a statement:

* `RUN` will start to execute the program starting at the lowest state­ment number.
* `LIST` will print out all the statements in numerical order.
* `LIST 120` will print out all the statements in numerical order starting at statement 120.
`NEW` will delete all statements.

Abbreviation and blanks. You may use blanks freely, ex­cept that numbers, command keywords, and function names can not have embedded blanks.

You may truncate all command keywords and function
names and follow them by a period. `P.`, `PR.`, `PRI.`, and
`PRIN.` all stand for `PRINT`. Also the word `LET` in `LET`
command can be omitted. The “shortest” abbreviation for all
keywords are as follows:

| Abbreviation | Keyword  |
|--------------|----------|
| Implied      | `LET`    |
| `A.`         | `ABS`    |
| `F.`         | `FOR`    |
| `GOS.`       | `GOSUB`  |
| `G.`         | `GOTO`   |
| `IF`         | `IF`     |
| `IN.`        | `INPUT`  |
| `L.`         | `LIST`   |
| `N.`         | `NEW`    |
| `N.`         | `NEXT`   |
| `P.`         | `PRINT`  |
| `REM`        | `REMARK` |
| `R.`         | `RETURN` |
| `R.`         | `RND`    |
| `R.`         | `RUN`    |
| `S.`         | `SIZE`   |
| `S.`         | `STEP`   |
| `S.`         | `STOP`   |
| `TO`         | `TO`     |

## Statements

A statement consists of a statement number of
between 1 and 32767 followed by one or more commands.
Commands in the same statement are separated by a semi-colon.
`GOTO`, `STOP`, and `RETURN` commands must be the last command in any given statement.
Commands. Tiny BASIC commands are listed below with
examples. Remember that commands can be concatenated with
semi-colons. In order to store the statement, you must also have
a statement number in front of the commands. The statement
number and the concatenation are not shown in the examples.

### REM or REMARK Command

```basic
REM anything goes
```

This line will be ignored by TBI.

### LET Command

```basic
LET A=234-5*6, A=A/2, X-A-100, @(X+9)=A-1
```

will set the variable `A` to the value of the expression `234-5*6`
(i.e., `204`), set the variable `A` (again) to the value of the expres­sion `A/2` (i.e., `102`), set the variable `X` to the value of the expres­sion `A-100` (i.e., 2), and then set the variable `@(11)` to `101`
(where `11` is the value of the expression `X+9` and `101` is the
value of the expression `A - 1`).

```basic
LET U=A#B,V=(A>B)*X+(A<B)*Y
```
will set the variable `U` to either `1` or `0` depending on whether `A`
is not equal to or is equal to `B`; and set the variable `V` to either
`X`, `Y` or `0` depending on whether `A` is greater than, less than, or
equal to `B`.

### PRINT Command
```basic
PRINT
```

will cause a carriage-return (CR) and a line-feed (LF) on the
output device.

```basic
PRINT A *3 + 1, "ABC 123 !@ # " , ' CBA '
```
will print the value of the expression `A * 3 + 1` (i.e., `307` ), the
string of characters `"ABC 123 ! @ # "` , and the string “ CBA ” ,
and then a CR-LF. N ote that either single or double quotes can
be used to quote strings, but pairs must be matched.

```basic
PRINT A * 3 + 1 , "ABC 123 !@ # ", ' CBA ',
```

will produce the same output as before, except that there is no
CR-LF after the last item is printed. This enables the program
to continue printing on the same line with another `PRINT`.

```basic
PRINT A, B, #3, C, D, E, #10, F, G
```

will print the values of `A` and `B` in six spaces, the values of `C`, `D`,
and `E` in 3 spaces, and the values of `F` and `G` in 10 spaces. If there
are not enough spaces specified for a given value to be printed,
the value will be printed with enough spaces anyway.

```basic
PRINT ‘A B C ’, - , ‘X X X ’
```

will print the string “ ABC” , a CR without a LF, and then the
string “ X X X ” (over ABC) followed by a CR-LF.

### INPUT Command
```basic
INPUT A, B
```

When this command is executed, Tiny BASIC will print “ A :”
and wait to read in an expression from the input device. The
variable A will be set to the value of this expression. Then
“ B:” is printed and variable B is set to the value of the next expression
read from the input device. Note that not only numbers,
but also expressions can be read as input.

```basic
INPUT 'WHAT IS THE WEIGHT', A, " AND SIZE, B
```

This is the same as the command above, except the prompt
"A :" is replaced by "WHAT IS THE WEIGHT:" and the
prompt " B :" is replaced by " AND SIZE:". Again, both single
and double quotes can be used as long as they are matched.

```basic
INPUT A, 'STRING', - , " ANOTHER STRING" , B
```

The strings and the have the same effect as in “PRINT” .

### IF Command

```basic
IF A < B LET X = 3 ; PRINT ‘THIS STRING’
```

will test the value o f the expression `A < B` . If it is not zero (i.e.,
if it is true), the commands in the rest of this statement will be
executed. If the value o f the expression is zero (i.e., if it is not
true), the rest of this statement will be skipped over and execution
continues at next statement. Note that the word `THEN`
is not used.

### GOTO Command
```basic
GOTO 120
```
will cause the execution to jump to statement 120. Note that
GOTO comm and cannot be followed by a semicolon and other
commands. It must be ended with a CR.

```basic
GOTO A * 10 + B
```
will cause the execution to jump to a different statement number
as computed from the value of the expression.

### GOSUB and RETURN Commands

`GOSUB` command is similar to `GOTO` command except
that: 
1. the current statement number and position within the
statement is remembered; and 
2. a semicolon and other commands can follow it in the same statement.

```basic
GOSUB 120
```
will cause the execution to jump to statement 120.
```basic
GOSUB A *10+B
```
will cause the execution to jump to different statements as computed
from the value of the expression `A * 10 + B`.

### RETURN

A `RETURN` command must be the last command in a statement
and follow ed by a CR. When a RETURN command is encountered,
it will cause the execution to jump back to the command
following the most recent `GOSUB` command.

`GOSUB` can be nested. The depth of nesting is limited only
by the stack space.

### FOR and NEXT Commands

```basic
FOR X = A + 1 TO 3 * B STEP C -1
```

The variable `X` is set to the value of the expression `A + 1`. The
values of the expressions (not the expressions themselves) `3 * B`
and `C - 1` are remembered. The name of the variable X, the statement
number and the position of this comm and within the
statement are also remembered. Execution then continues the
normal way until a `NEXT` command is encountered.
The STEP can be positive, negative or even zero. The word
`STEP` and the expression following it can be omitted if the desired
`STEP` is + 1.

### NEXT X

The name of the variable (X) is checked with that of the most
recent `FOR` command. If they do not agree, that `FOR` is terminated
and the next recent `FOR` is checked, etc. When a match is
found, this variable will be set to its current value plus the value
of the `STEP` expression saved by the `FOR` command. The updated
value is then compared with the value of the `TO` expression
also saved by the FOR com m and. If this is within the limit,
execution will jum p back to the com m and following the `FOR`
command. If this is outside the limit, execution continues following
the `NEXT` command itself.

`FOR` can be nested. The depth of nesting is limited only by
the stack space. If a new `FOR` command with the same control
variable as that of an old FOR command is encountered, the old
`FOR` will be terminated automatically.

### STOP Command 

This command stops the execution of the program and returns
control to direct commands from the input device. It can
appear many times in a program but must be the last command
in any given statement; i.e., it cannot be followed by a semicolon
and other commands.

### Stopping the Execution.

By the Control-C key on the input device.
Control of Output Device. The Control-O key on the input
device can be used to turn the output device ON and OFF. This
is useful when you want to read in a program punched on paper
tape.

To produce such a paper tape, type “ LIST” without CR.
Turn on the paper tape punch and type a few Control-Shift-P’s
and then a CR. When listing is finished, type more Control-Shift-P’s
and turn off the punch.

To read back such a paper tape, type “ NEW” , CR, and
Control-O, then turn on the paper tape reader. When the paper
tape is read, turn the reader off and type a Control-O again.

### Error Report.

 There are only three error conditions in Tiny
BASIC. The statement with the error is printed out with a question
mark inserted at the point where the error is detected.

1. WHAT? means it does not understand you. Example:
    WHAT?

    ```basic
    210 P?TINT "THIS" 
    ```
    
    where PRINT is mistyped

    WHAT?
    
    ```basic
    260 LET A = B + 3, C = (3 + 4 ? , X = 4
    ```

2. HOW? means it understands you but does not know how to do it.
    HOW?

    ```basic
    310 LET A = B * C ? + 2 
    ```

    where `B * C` is greater than 32767

    HOW?
    
    ```basic
    380 GOTO 412?
    ```
    
    where 412 does not exist

3. SORRY means it understands you and knows how to do it but there is not enough memory to do it.

### Error Corrections.ß

If you notice an error in typing before
you hit the CR, you can delete the last character by the Rub-Out 
key or delete the entire line by the Alt-Mode key. Tiny
BASIC will echo a back-slash for each Rub-Out. Echo for Alt-
Mode consists of a LF, a CR, and an up-arrow.
To correct a statement, you can retype the statement number
and the correct commands. Tiny BASIC will replace the old
statement with the new one.
To delete a statement, type the statement number and a CR
only.
Verify the corrections by `LIST nnnn` and hit the Control-C
key while the line is being printed.