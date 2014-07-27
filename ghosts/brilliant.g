; idiot.g

; a variant of stupid.g

; start by moving up for 6 steps, then right for 6 steps, etc,
; then always move into the available direction closest to lambda-man's position

%lastx :: [1]
%lasty :: [2]
%clkl :: [3]
%clkh :: [4]
%curmode :: [5]        ; 0 = chase, 1 = scatter
%scatterx :: [6]
%scattery :: [7]
%principal :: [8]
%secondary :: [9]
; declarations for follow
%good_dir_val     :: [248]
%good_dir_num     :: [249]
%current_x   :: [250]
%current_y   :: [251]
%current_dir :: [252]

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
  mov %principal, c
  mov %secondary, d

  mov [254], c
  mov [255], ret1
  mov PC, next    ; get coordinates of square in direction c
ret1:
  int 7           ; get square
  jeq secondary, a, 0    ; go in main direction if not wall

  int 3
  int 6           ; get ghost current direction
  add b, 2        ; reverse the direction
  and b, 3        ; reverse the direction, cont.
  and %principal, 3        ; clip the principal direction
  jeq secondary, b, %principal          ; check if principal direction is forbidden
  int 3
  int 5
  mov %current_dir, %principal
  mov [253], testing_if_good_path
  mov PC, follow
testing_if_good_path:
  MOV H, A
  INT 3
  INT 5
  ;; MOV F, A
  ;; MOV G, B
  ;; MOV D, %principal
  ;; MOV E, %secondary
  ;; MOV C, %curmode
  ;;  int
  jmp go          ; principal direction is valid

secondary:
  mov %principal, %secondary        ; go in secondary direction

go:
  and %principal, 3        ; reduce modulo 4
  mov a, %principal
  int 0           ; direction is in a
  hlt


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; follow a <- X, b <- Y, %current_dir<- DIR, [253] <- RETURN
; return 0 in a if it is a good choice, and something else otherwise
follow:
   ;; MOV C, %current_dir          ; debug
   ;; MOV H, 42                    ; debug
   ;; int 8                        ; debug
   MOV [254], %current_dir
   MOV [255], follow_start
   MOV PC, next
follow_start:
  ; we are in the first tile of the path
  ;; MOV H, 43
  ;; INT 8
  MOV %good_dir_num, 0
  MOV %current_x, a
  MOV %current_y, b
  MOV [254], %current_dir      ; prepare the call to next for current dir
  MOV [255], lwalling0         ; prepare the call...
  MOV PC, next
lwalling0:
  ;; MOV H, 44
  ;; INT 8
  INT 7
  JEQ lwalling2, A, 0
  MOV %good_dir_val, %current_dir
  INC %good_dir_num
lwalling1:
  INC %current_dir
  AND %current_dir, 3
  MOV a, %current_x
  MOV b, %current_y
  ;; mov c, %current_dir           ; debug
  ;; mov h, 45                     ; debug
  ;; INT 8                         ; debug
  MOV [254], %current_dir      ; prepare the call to next for current dir
  MOV [255], lwalling2         ; prepare the call...
  MOV PC, next
lwalling2:
  ;; MOV H, 44
  ;; INT 8
  INT 7
  JEQ lwalling3, A, 0
  MOV %good_dir_val, %current_dir
  INC %good_dir_num
lwalling3:
  ADD %current_dir, 2
  AND %current_dir, 3
  MOV a, %current_x
  MOV b, %current_y
  MOV [254], %current_dir      ; prepare the call to next for current dir
  MOV [255], lwalling4         ; prepare the call...
  MOV PC, next
lwalling4:
  ;; MOV H, 44
  ;; INT 8
  INT 7
  JEQ lwalling5, A, 0
  MOV %good_dir_val, %current_dir
  INC %good_dir_num
lwalling5:
  JEQ not_ok, %good_dir_num, 0
  JEQ continue, %good_dir_num, 1
  MOV A,0
  MOV PC, [253]
not_ok:
  MOV A,1
  MOV PC, [253]
continue:
  MOV %current_dir, %good_dir_val
  MOV A, %current_x
  MOV B, %current_y
  MOV PC, follow

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
