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
  jgt follow, [0], 100
  ; the start counter is low: try to get out of the start box
  inc [0]
  mov b, [0]
  div b, 25
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
  jeq go, a, 0

  add c, 2
  int 8

go:
  and c, 3        ; reduce modulo 4
  mov a, c
  int 0           ; direction is in a
  hlt
