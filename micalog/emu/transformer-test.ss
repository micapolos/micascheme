(import
  (micascheme)
  (micalog emu transformer)
  (prefix (micalog keywords) %)
  (prefix (micalog emu keywords) %)
  (prefix (micac) %%)
  (prefix (micac lib emu) %%)
  (prefix (micac lib emu) %%))

(define-check-datum-> micac)

; === types ===

(check-micac (type 1) bool)
(check-micac (type 2) uint8_t)
(check-micac (type 8) uint8_t)
(check-micac (type 9) uint16_t)
(check-micac (type 16) uint16_t)
(check-micac (type 17) uint32_t)
(check-micac (type 32) uint32_t)
(check-micac (type 33) uint64_t)
(check-micac (type 64) uint64_t)

; === values ===

(check-micac (value foo) foo)
(check-micac (value 123) 123)

; === expressions ===

(check-micac (expr foo) foo)
(check-micac (expr 123) 123)

(check-micac
  (expr (%append (2 a)))
  a)

(check-micac
  (expr (%append (2 a) (4 b)))
  (%%bitwise-ior (%%bitwise-arithmetic-shift-left a 4) b))

(check-micac
  (expr (%append (2 a) (4 b) (5 c)))
  (%%bitwise-ior
    (%%bitwise-arithmetic-shift-left
      (%%bitwise-ior
        (%%bitwise-arithmetic-shift-left a 4)
        b)
      5)
    c))

(check-micac
  (expr (%take 6 a 6))
  (%%bitwise-and a #x3f))

(check-micac
  (expr (%drop 6 a 2))
  (%%bitwise-arithmetic-shift-right a 2))

(check-micac (expr (%= 6 a b)) (%%= a b))
(check-micac (expr (%!= 6 a b)) (%%not (%%= a b)))
(check-micac (expr (%< 6 a b)) (%%< a b))
(check-micac (expr (%<= 6 a b)) (%%<= a b))
(check-micac (expr (%> 6 a b)) (%%> a b))
(check-micac (expr (%>= 6 a b)) (%%>= a b))

(check-micac (expr (%wrap+ 6 a b)) (%%bitwise-and (%%+ a b) #x3f))
(check-micac (expr (%wrap- 6 a b)) (%%bitwise-and (%%- a b) #x3f))
(check-micac (expr (%wrap* 6 a b)) (%%bitwise-and (%%* a b) #x3f))
(check-micac (expr (%wrap- 6 a)) (%%bitwise-and (%%- a) #x3f))

(check-micac (expr (%+ 6 a b)) (%%+ a b))
(check-micac (expr (%- 6 a b)) (%%- a b))
(check-micac (expr (%* 6 a b)) (%%* a b))
(check-micac (expr (%- 6 a)) (%%- a))

(check-micac (expr (%and 1 a b)) (%%and a b))
(check-micac (expr (%or 1 a b)) (%%or a b))
(check-micac (expr (%xor 1 a b)) (%%bitwise-xor a b))

(check-micac (expr (%and 6 a b)) (%%bitwise-and a b))
(check-micac (expr (%or 6 a b)) (%%bitwise-ior a b))
(check-micac (expr (%xor 6 a b)) (%%bitwise-xor a b))

(check-micac (expr (%nand 1 a b)) (%%bitwise-and (%%not (%%and a b)) 1))
(check-micac (expr (%nor 1 a b)) (%%bitwise-and (%%not (%%or a b)) 1))
(check-micac (expr (%xnor 1 a b)) (%%bitwise-and (%%not (%%bitwise-xor a b)) 1))

(check-micac (expr (%nand 6 a b)) (%%bitwise-and (%%bitwise-not (%%bitwise-and a b)) #x3f))
(check-micac (expr (%nor 6 a b)) (%%bitwise-and (%%bitwise-not (%%bitwise-ior a b)) #x3f))
(check-micac (expr (%xnor 6 a b)) (%%bitwise-and (%%bitwise-not (%%bitwise-xor a b)) #x3f))

(check-micac (expr (%not 1 a)) (%%bitwise-and (%%not a) 1))
(check-micac (expr (%not 6 a)) (%%bitwise-and (%%bitwise-not a) #x3f))

(check-micac (expr (%if 6 a b c)) (%%if a b c))

(check-micac
  (expr (%if 6 (%not 1 a) (%not 6 b) (%and 6 c d)))
  (%%if
    (%%bitwise-and (%%not a) 1)
    (%%bitwise-and (%%bitwise-not b) #x3f)
    (%%bitwise-and c d)))

; === registers ===

(check-micac
  (register (%register 8 foo))
  (%%var uint8_t foo))

; === instructions ===

(check-micac
  (instruction (%wire 8 foo bar))
  (%%const uint8_t foo bar))

(check-micac
  (instruction (%output 8 foo bar))
  (%%var uint8_t foo bar))

(check-micac
  (instruction (%set 8 foo bar))
  (%%set foo bar))

(check-micac
  (instruction (%set 8 foo bar))
  (%%set foo bar))

(check-micac
  (instruction (%log foo 16 (%wrap+ 16 1 2)))
  (%%printf "%s: %u\\n" "foo" (%%bitwise-and (%%+ 1 2) #xffff)))

(check-micac
  (instruction (%on (%posedge prev next)))
  (%%when (%%not (%%= prev next))
    (%%when (%%= next 1))))

(check-micac
  (instruction (%on (%negedge prev next)))
  (%%when (%%not (%%= prev next))
    (%%when (%%= next 0))))

(check-micac
  (instruction
    (%on (%posedge prev next)
      (%set 8 foo bar)
      (%set 8 goo gar)))
  (%%when (%%not (%%= prev next))
    (%%when (%%= next 1)
      (%%set foo bar)
      (%%set goo gar))))

(check-micac
  (instruction
    (%cond
      (foo (%set 8 foo bar))
      (%else (%set 8 zoo zar))))
  (%%cond
    (foo (%%set foo bar))
    (%%else (%%set zoo zar))))

(check-micac
  (instruction
    (%cond
      (foo (%set 8 foo bar))
      (bar (%set 8 zoo zar))))
  (%%cond
    (foo (%%set foo bar))
    (bar (%%set zoo zar))))

; === module ===

(check-micac
  (module
    (%module foo))
  (%%run-emu
    (%%video 352 288 96 24 4)
    (%%update)))

(check-micac
  (module
    (%module foo
      (%input 1 %reset?)
      (%input 1 %clock)
      (%input 9 %video-x)
      (%input 9 %video-y)
      (%input 9 %mouse-x)
      (%input 9 %mouse-y)
      (%input 1 %mouse-pressed?)
      (%output 1 inv-clock (%not 1 %clock))
      (%output 8 %video-red 20)
      (%output 8 %video-green 30)
      (%output 8 %video-blue 40)))
  (%%run-emu
    (%%video 352 288 96 24 4)
    (%%var bool %clock 0)
    (%%var int reset-counter 32)
    (%%var bool %reset? 1)
    (%%var int %video-x)
    (%%var int %video-y)
    (%%var int %mouse-x)
    (%%var int %mouse-y)
    (%%var bool %mouse-pressed?)
    (%%update
      (%%set %clock (%%xor %clock 1))
      (%%if (%%= reset-counter 0)
        (%%then (%%set %reset? 0))
        (%%else (%%set reset-counter (%%- reset-counter 1))))
      (%%set %video-x %%video-x)
      (%%set %video-y %%video-y)
      (%%set %mouse-x %%mouse-x)
      (%%set %mouse-y %%mouse-y)
      (%%set %mouse-pressed? %%mouse-pressed?)
      (%%var bool inv-clock (%%bitwise-and (%%not %clock) 1))
      (%%var uint8_t %video-red 20)
      (%%var uint8_t %video-green 30)
      (%%var uint8_t %video-blue 40)
      (%%set %%red %video-red)
      (%%set %%green %video-green)
      (%%set %%blue %video-blue))))
















