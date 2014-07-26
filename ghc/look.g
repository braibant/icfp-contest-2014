%x :: [12]
%y :: [13]
%dir :: [14]
%return :: [15]
%walls  :: [16]
%nwalls :: [17]

; Mapping from direction i to 2^i
MOV [0], 1
MOV [1], 2
MOV [2], 4
MOV [3], 8
; Mapping from 2^i to direction i
MOV [4], 0
MOV [5], 1
MOV [7], 2
MOV [11], 3

; compute the next position in one direction
next:
        MUL %dir, 2
        ADD %dir, 2
        MOV C, PC
        ADD C, %dir
        MOV PC, C
        SUB %x, 1                ; code for 0
        JMP next_end
        ADD %y, 1                ; code for 1
        JMP next_end
        ADD %x, 1                ; code for 2
        JMP next_end
        SUB %y, 1                ; code for 3
next_end:
        MOV PC, H

; describe the walling aroung %x,%y
walling:
        MOV %walls, 0
        MOV %nwalls, 0
        MOV %dir, 0
loop:
        MOV A, %x
        MOV B, %y
        MOV C, %dir
        MOV H, PC
        ADD H, 3
        JMP next
        ; restore %x and %y
        MOV %x, A
        MOV %y, B
        MOV %dir, C
        INT 7
        JEQ loop_not_wall, A, 0; is wall
        MOV C, %dir
        OR %walls, [C] ; assume that we have [1;2;4;8] in the [0;3] addresses
        INC %nwalls
loop_not_wall:
        INC %dir
        JLT loop, %dir, 4; else, we have finished
        MOV PC, %return

; pick one possible value
pick:
        MOV %return, PC
        ADD %return, 3
        JMP walling
        ; remove the direction we are comming from
        INT 6 ; get direction in B
        MOV A, [B]
        XOR A, 255
        OR %walls, A
        ; here %walls contains all forbidden directions
        ; what to do next ?
