; genius.g

; a variant of brilliant.g
; avoid following another [lower-numbered] ghost

%lastx :: [1]
%lasty :: [2]
%clkl :: [3]
%clkh :: [4]
%curmode :: [5]        ; 0 = chase, 1 = scatter
%scatterx :: [6]
%scattery :: [7]
%principal :: [8]
%secondary :: [9]
        %uturn :: [10]
        %test_dir :: [11]

;;; declarations for check_presence
        %return_eq :: [245]
        %return_neq :: [246]

;;; declarations for follow
        %ghost_index :: [247]
%good_dir_val     :: [248]
%good_dir_num     :: [249]
%current_x   :: [250]
%current_y   :: [251]
%current_dir :: [252]
;;; %follow_return :: [253]

;;; declarations for next
;;; %next_arg_dir :: [254]
;;; %next_return :: [255]

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
int 8
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
        and %principal, 3
  mov %secondary, d
        and %secondary, 3
  int 3
  int 6           ; get ghost current direction
  add b, 2        ; reverse the direction
  and b, 3        ; reverse the direction, cont.
        mov %uturn, b

;;; check for a wall in the principal direction
        int 3
        int 5
        mov [254], %principal
        mov [255], ret1
        mov PC, next    ; get coordinates of square in direction c
ret1:
        int 7           ; get square
        jeq secondary, a, 0    ; if wall, go check secondary direction

  jeq secondary, %uturn, %principal ; go to secondary if principal is forbidden
  int 3
  int 5
  mov %current_dir, %principal
  mov [253], testing_if_good_path
  mov PC, follow
testing_if_good_path:
        jeq go, a, 0

secondary:
;;; check if secondary direction is valid
;;; check for wall
        int 3
        int 5                   ; get my coordinates
        mov [254], %secondary
        mov [255], ret2
        mov PC, next
ret2:
        int 7                   ; get square
        jeq random_dir, a, 0    ; secondary direction is a wall
;;; check for u-turn
        jeq random_dir, %uturn, %secondary ; secondary is forbidden
;;; check for dead-end
        int 3
        int 5
        mov %current_dir, %secondary
        mov [253], ret3
        mov PC, follow
ret3:
        jeq go_secondary, a, 0

random_dir:
        int 3                   ; a contains ghost number
        mov %test_dir, a                ; "random" direction
random_loop:
        inc %test_dir
        and %test_dir, 3
        jeq random_loop, %test_dir, %principal
        jeq random_loop, %test_dir, %secondary
        jeq random_loop, %test_dir, %uturn
        int 3
        int 5
        mov [254], %test_dir
        mov [255], ret4
        mov PC, next
ret4:
        int 7
        jeq random_loop, a, 0

;;; go in %test_dir
        mov %secondary, %test_dir
        ;; fall through

go_secondary:
  mov %principal, %secondary        ; go in secondary direction

go:
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
   MOV [255], lfollow1
   MOV PC, next
lfollow1:
  ; we are in the first tile of the path
  ;; MOV H, 43
  ;; INT 8
  MOV %good_dir_num, 0
  MOV %current_x, a
  MOV %current_y, b

; check for the presence of lambda-man
  int 1
        mov %return_eq, return_ok_lambda
        mov %return_neq, checklamdone
        jmp check_presence
checklamdone:
        ;;  check for the presence of another (lower-numbered) ghost
        int 3
        mov %ghost_index, a
checkghostloop:
        dec %ghost_index
        jeq checkghostdone, %ghost_index, 255
        mov a, %ghost_index
        int 5
        mov %return_eq, not_ok_ghost
        mov %return_neq, checkghostloop
        jmp check_presence
checkghostdone:
  mov a, %current_x
  mov b, %current_y
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
return_ok_lambda:
return_ok:
  MOV A,0
  MOV PC, [253]
;; not_ok_ghost:
not_ok:
  MOV A,1
  MOV PC, [253]
continue:
  MOV %current_dir, %good_dir_val
  MOV A, %current_x
  MOV B, %current_y
  MOV PC, follow

;; return_ok_lambda:     ; debug
;;   int 8               ; debug
;;   jmp return_ok       ; debug

not_ok_ghost:                   ; debug
        int 8                   ; debug
        jmp not_ok              ; debug


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check equality of (a,b) with (%current_x,%current_y)
;;; jump to %return_eq if they are equal
;;; jump to %return_neq if they are not equal
;;; preserve the value of all registers and memory
check_presence:
        jeq same_x, a, %current_x
        jmp %return_neq
same_x:
        jeq same_y, b, %current_y
        jmp %return_neq
same_y:
        jmp %return_eq

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
