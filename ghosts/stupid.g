; stupid.g

; start by moving randomly for 100 steps, then
; always move into the direction closest to lambda-man's position

  ; mov a, [0]
  ; and a, 7
  ; jgt follow, a, 0
  ; inc [0]

  ; int 3
  ; add a, [0]
  ; mul a, 27
  ; and a, 3
  ; int 0
  ; hlt

follow:
  int 1     ; get lman's coordinates in A and B
  mov c, a  ; c = lman.x
  mov d, b  ; d = lman.y
  int 3     ; get this ghost's index
  int 4
  mov e, a  ; e = my.x
  mov f, b  ; f = my.y

; compute direction

  mov a, 1     ; alpha
  mov b, 255   ; phi
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
  jlt l3, g, h    ; if absx >= absy then
    add a, b      ; add phi
    jmp l4
l3:               ; else
    sub a, b      ; subtract phi

l4:
  div a, 2
  and a, 3        ; reduce modulo 4

l5:
  int 0 ; direction is in a
  hlt
