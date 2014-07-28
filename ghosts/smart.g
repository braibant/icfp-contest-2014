; smart.g

; alternate between scatter mode and chase mode
; in scatter mode, move out from current position in a direction that depends
; on the ghost number
; in chase mode, move into the direction of lambda-man.

%lastx :: [1]
%lasty :: [2]
%clkl :: [3]
%clkh :: [4]
%curmode :: [5]        ; 0 = chase, 1 = scatter
%scatterx :: [6]
%scattery :: [7]

; clock
  add %clkl, 8               ; clock high period 256/8 = 32
  jgt clock_done, %clkl, 0
  inc %clkh
clock_done:

; detect sudden jumps in position and reset the clock
  int 3
  int 5
  sub a, %lastx
  sub b, %lasty
  add a, b
  add a, 1
  jlt detect_done, a, 3
  mov %clkl, 0
  mov %clkh, 0
detect_done:
  int 3
  int 5
  mov %lastx, a
  mov %lasty, b

; decide between chase and scatter

; if panic mode, scatter

  int 3
  int 6
  jeq scatter, a, 1    ; panic mode -> scatter
  jeq chase, a, 2      ; invisible mode -> chase
                       ; normal mode -> scatter/chase according to clock

; if clock-high = 0 mod 4, scatter
  mov c, %clkh
  and c, 3
  jgt chase, c, 0

scatter:
  jeq scatter_continue, %curmode, 1
  mov %curmode, 1
  mov c, %clkh
  div c, 8
  int 3
  add a, c
  mov %scatterx, a
  div a, 2
  mov %scattery, a
  and %scatterx, 1
  and %scattery, 1
  mul %scatterx, 30
  mul %scattery, 30
  sub %scatterx, 15
  sub %scattery, 15
  int 3
  int 1
  add %scatterx, a
  add %scattery, b
  jlt clipxdone, %scatterx, 128
  mov %scatterx, 0
clipxdone:
  jlt clipydone, %scattery, 128
  mov %scattery, 0
clipydone:
  mov a, %scatterx
  mov b, %scattery
  int 8
scatter_continue:
  mov a, %scatterx
  mov b, %scattery
  jmp go_to

chase:
  mov %curmode, 0
  int 1     ; get lman's coordinates in A and B

go_to:
  mov c, a  ; c = lman.x
  mov d, b  ; d = lman.y
  int 3     ; get this ghost's index
  int 5
  mov e, a  ; e = my.x
  mov f, b  ; f = my.y

; compute direction

  mov a, 1     ; alpha
  mov b, 1     ; phi
  mov g, c
  sub g, e     ; absx = dx  = lman.x - my.x
  mov h, f
  sub h, d     ; absy = -dy = my.y - lman.y

  jgt l1, c, e     ; if lman.x =< my.x then
    add a, 6
    mov g, e
    sub g, c   ; absx = -dx
    xor b, 254 ; phi := -phi
l1:
  jlt l2, d, f     ; if lman.y >= my.y then
    xor a, 2
    mov h, d
    sub h, f   ; absy = dy
    xor b, 254 ; phi := -phi


l2:
; from now on, c and d are the main and secondary direction
  mov c, a
  mov d, a

  jlt l3, g, h    ; if absx >= absy then
    xor b, 254    ; phi := -phi
l3:
    sub c, b      ; add phi for main direction
    add d, b      ; subtract phi for secondary direction

l4:
  div c, 2        ; main direction of lambda-man
  div d, 2        ; secondary direction
  and c, 3
  and d, 3

test:
  int 3
  int 5
  mov [254], c
  mov [255], ret1
  mov PC, next    ; get coordinates of square in direction c
ret1:
  int 7           ; get square
  jeq secondary, a, 0    ; go in main direction if not wall

  int 3
  int 6
  add b, 2
  and b, 3
  and c, 3
  jeq secondary, b, c
  jmp go

secondary:
  mov c, d        ; go in secondary direction

go:
  and c, 3        ; reduce modulo 4
  mov a, c
  int 0           ; direction is in a
  hlt


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; next a <- X, b <- Y, [254] <- DIR, [255] <- RETURN
;;  this function has zero bug.
next:
        MUL [254], 2
        ADD [254], 3
        MOV H, PC
        ADD H, [254]
        MOV PC, H
        SUB B, 1                ; code for 0
        JMP next_end
        ADD A, 1                ; code for 1
        JMP next_end
        ADD B, 1                ; code for 2
        JMP next_end
        SUB A, 1                ; code for 3
next_end:
        MOV PC, [255]



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
