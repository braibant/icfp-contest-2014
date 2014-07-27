; idiot.g

; a variant of stupid.g

; start by moving up for 6 steps, then right for 6 steps, etc,
; then always move into the available direction closest to lambda-man's position


; detect sudden jumps in position and reset the start counter
  int 3
  int 5
  sub a, [1]
  sub b, [2]
  add a, b
  add a, 1
  jlt nojump, a, 3
  mov [0], 0

nojump:
  int 3
  int 5
  mov [1], a
  mov [2], b
  jgt follow, [0], 24
  ; the start counter is low: try to get out of the start box
  inc [0]
  mov b, [0]
  div b, 6
  int 3
  add a, b
  and a, 3
  int 0
  hlt

follow:
  int 1     ; get lman's coordinates in A and B
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
; if panic mode, reverse direction

  int 3
  int 6
  jeq test, a, 0

  add c, 2
  add d, 2

test:
  int 3
  int 5
  and c, 3
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
  int 8
  mov c, d        ; go in secondary direction

go:
  and c, 3        ; reduce modulo 4
  mov a, c
  int 0           ; direction is in a
  hlt



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; next a <- X, b <- Y, [254] <- DIR, [255] <- RETURN
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
