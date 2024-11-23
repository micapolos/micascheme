(import
  (micascheme)
  (micalog micac-transformer)
  (prefix (micalog keywords) %)
  (prefix (micac) %%)
  (prefix (micac lib emu) %%))

(define-check-datum-> micac)

; === types ===

(check-micac (type 1) %%bool)
(check-micac (type 2) %%uint8_t)
(check-micac (type 8) %%uint8_t)
(check-micac (type 9) %%uint16_t)
(check-micac (type 16) %%uint16_t)
(check-micac (type 17) %%uint32_t)
(check-micac (type 32) %%uint32_t)
(check-micac (type 33) %%uint64_t)
(check-micac (type 64) %%uint64_t)

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
  (expr (%slice 6 a 2))
  (%%bitwise-and (%%bitwise-arithmetic-shift-right a 2) #x3f))

(check-micac (expr (%= 6 a b)) (%%= a b))
(check-micac (expr (%!= 6 a b)) (%%not (%%= a b)))
(check-micac (expr (%< 6 a b)) (%%< a b))
(check-micac (expr (%<= 6 a b)) (%%<= a b))
(check-micac (expr (%> 6 a b)) (%%> a b))
(check-micac (expr (%>= 6 a b)) (%%>= a b))

(check-micac (expr (%+ 6 a b)) (%%bitwise-and (%%+ a b) #x3f))
(check-micac (expr (%- 6 a b)) (%%bitwise-and (%%- a b) #x3f))
(check-micac (expr (%- 6 a)) (%%bitwise-and (%%- a) #x3f))

(check-micac (expr (%+ 8 a b)) (%%+ a b))
(check-micac (expr (%- 8 a b)) (%%- a b))
(check-micac (expr (%- 8 a)) (%%- a))

(check-micac (expr (%and 6 a b)) (%%bitwise-and a b))
(check-micac (expr (%or 6 a b)) (%%bitwise-ior a b))
(check-micac (expr (%xor 6 a b)) (%%bitwise-xor a b))

(check-micac (expr (%nand 6 a b)) (%%bitwise-and (%%bitwise-not (%%bitwise-and a b)) #x3f))
(check-micac (expr (%nor 6 a b)) (%%bitwise-and (%%bitwise-not (%%bitwise-ior a b)) #x3f))
(check-micac (expr (%xnor 6 a b)) (%%bitwise-and (%%bitwise-not (%%bitwise-xor a b)) #x3f))

(check-micac (expr (%nand 8 a b)) (%%bitwise-not (%%bitwise-and a b)))
(check-micac (expr (%nor 8 a b)) (%%bitwise-not (%%bitwise-ior a b)))
(check-micac (expr (%xnor 8 a b)) (%%bitwise-not (%%bitwise-xor a b)))

(check-micac (expr (%not 6 a)) (%%bitwise-and (%%bitwise-not a) #x3f))
(check-micac (expr (%not 8 a)) (%%bitwise-not a))

(check-micac (expr (%if 6 a b c)) (%%if a b c))

(check-micac
  (expr (%if 6 (%not 1 a) (%not 6 b) (%and 6 c d)))
  (%%if
    (%%not a)
    (%%bitwise-and (%%bitwise-not b) 63)
    (%%bitwise-and c d)))

; === registers ===

(check-micac
  (register (%register 8 foo))
  (%%var %%uint8_t foo))

; === input params ===

(check-micac
  (input-param (%input 8 foo))
  foo)

; === instructions ===

(check-micac
  (instruction (%capture 8 foo bar))
  (%%const %%uint8_t foo bar))

(check-micac
  (instruction (%wire 8 foo bar))
  (%%const %%uint8_t foo bar))

(check-micac
  (raises
    (instruction (%output 8 foo bar))))

(check-micac
  (instruction (%set 8 foo bar))
  (%%set foo bar))

(check-micac
  (instruction
    (%on (prev next)
      (%posedge)))
  (%%when (%%not (%%= prev next))
    (%%when (%%= next 1))))

(check-micac
  (instruction
    (%on (prev next)
      (%negedge)))
  (%%when (%%not (%%= prev next))
    (%%when (%%= next 0))))

(check-micac
  (instruction
    (%on (prev next)
      (%posedge)
      (%else)))
  (%%when (%%not (%%= prev next))
    (%%if (%%= next 1)
      (%%then)
      (%%else))))

(check-micac
  (instruction
    (%on (prev next)
      (%negedge)
      (%else)))
  (%%when (%%not (%%= prev next))
    (%%if (%%= next 0)
      (%%then)
      (%%else))))

(check-micac
  (instruction
    (%on (prev next)
      (%posedge
        (%set 8 foo bar)
        (%set 8 goo gar))))
  (%%when (%%not (%%= prev next))
    (%%when (%%= next 1)
      (%%set foo bar)
      (%%set goo gar))))

(check-micac
  (instruction
    (%on (prev next)
      (%negedge
        (%set 8 foo bar)
        (%set 8 goo gar))
      (%else
        (%set 16 zoo zar)
        (%set 16 moo mar))))
  (%%when (%%not (%%= prev next))
    (%%if (%%= next 0)
      (%%then
        (%%set foo bar)
        (%%set goo gar))
      (%%else
        (%%set zoo zar)
        (%%set moo mar)))))

; === module ===

(check-micac
  (module
    (%module (empty prev-clock clock)))
  (%%macro (empty)
    (%%var uint8_t prev-clock 0)
    (%%var uint8_t clock 1)
    (%%update
      (%%set prev-clock clock)
      (%%set clock (%%xor clock 1)))))

(check-micac
  (module
    (%module (counter prev-clock clock)
      (%register 16 counter)
      (%on (prev-clock clock)
        (%posedge
          (%capture 16 previous-counter counter)
          (%set 16 counter (%+ 16 previous-counter 1))))))
  (%%macro (counter)
    (%%var uint8_t prev-clock 0)
    (%%var uint8_t clock 1)
    (%%var %%uint16_t counter)
    (%%update
      (%%set prev-clock clock)
      (%%set clock (%%xor clock 1))
      (%%when (%%not (%%= prev-clock clock))
        (%%when (%%= clock 1)
          (%%const %%uint16_t previous-counter counter)
          (%%set counter (%%+ previous-counter 1)))))))
