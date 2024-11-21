(import
  (micascheme)
  (micalog micac)
  (prefix (micalog keywords) %)
  (prefix (micac syntax) %%))

(define-case-syntax (check-micac (kind micalog) micac)
  #`(check
    (equal?
      (syntax->datum
        (
          #,(identifier-append #'kind #'kind #'->micac)
          #'micalog))
      'micac)))

(define-case-syntax (check-micacs (kind micalog ...) micac)
  #`(check
    (equal?
      (syntax->datum
        (
          #,(identifier-append #'kind #'kind #'->micacs)
          #'(micalog ...)))
      'micac)))

(check-micac (value foo) foo)
(check-micac (value 123) 123)

(check-micac (expr foo) foo)
(check-micac (expr 123) 123)

(check-micac
  (expr (%append 2 a 4 b))
  (%%bitwise-ior (%%bitwise-arithmetic-shift-left a 4) b))

(check-micac
  (expr (%slice 6 a 2))
  (%%bitwise-and (%%bitwise-arithmetic-shift-right a 2) #x3f))

(check-micac (expr (%= 6 a b)) (%%if (%%= a b) 1 0))
(check-micac (expr (%!= 6 a b)) (%%if (%%not (%%= a b)) 1 0))
(check-micac (expr (%< 6 a b)) (%%if (%%< a b) 1 0))
(check-micac (expr (%<= 6 a b)) (%%if (%%<= a b) 1 0))
(check-micac (expr (%> 6 a b)) (%%if (%%> a b) 1 0))
(check-micac (expr (%>= 6 a b)) (%%if (%%>= a b) 1 0))

(check-micac (expr (%add 6 a b)) (%%bitwise-and (%%+ a b) #x3f))
(check-micac (expr (%sub 6 a b)) (%%bitwise-and (%%- a b) #x3f))
(check-micac (expr (%neg 6 a)) (%%bitwise-and (%%- a) #x3f))

(check-micac (expr (%and 6 a b)) (%%bitwise-and a b))
(check-micac (expr (%or 6 a b)) (%%bitwise-ior a b))
(check-micac (expr (%xor 6 a b)) (%%bitwise-xor a b))

(check-micac (expr (%nand 6 a b)) (%%bitwise-not (%%bitwise-and a b)))
(check-micac (expr (%nor 6 a b)) (%%bitwise-not (%%bitwise-ior a b)))
(check-micac (expr (%nxor 6 a b)) (%%bitwise-not (%%bitwise-xor a b)))

(check-micac (expr (%not 6 a)) (%%bitwise-and (%%bitwise-not a) #x3f))

(check-micac (expr (%if 6 a b c)) (%%if (%%= a 1) b c))

(check-micac (size 1) %%uint8_t)
(check-micac (size 8) %%uint8_t)
(check-micac (size 9) %%uint16_t)
(check-micac (size 16) %%uint16_t)
(check-micac (size 17) %%uint32_t)
(check-micac (size 32) %%uint32_t)
(check-micac (size 33) %%uint64_t)
(check-micac (size 64) %%uint64_t)

(check-micac (declaration (%input 8 foo)) (%%extern foo))
(check-micac (declaration (%output 8 foo)) (%%var uint8_t foo))
(check-micac (declaration (%wire 8 foo)) (%%var uint8_t foo))
(check-micac (declaration (%reg 8 foo)) (%%var uint8_t foo))

(check-micac
  (instr (%define foo (%expr 9 12)))
  (%%unit
    (%%init)
    (%%update (%%const %%uint16_t foo 12))))

(check-micac
  (instr (%define foo (%expr (%reg 9) (%reg (%expr 9 128)))))
  (%%unit
    (%%init (%%var %%uint16_t foo 128))
    (%%update)))

(check-micac
  (instr (%define foo (%expr (%reg 9) (%reg))))
  (%%unit
    (%%init (%%var %%uint16_t foo))
    (%%update)))

(check-micac
  (instr (%set! (%expr _ x) (%expr _ y)))
  (%%unit
    (%%init)
    (%%update (%%set x y))))

(parameterize ((generates-identifier? #f))
  (check-micac
    (instr
      (%on (%expr _ clock)
        (%posedge
          (%define pos-reg (%expr (%reg 8) (%reg (%expr 8 10))))
          (%define pos-val (%expr 8 10)))
        (%negedge
          (%define neg-reg (%expr (%reg 8) (%reg (%expr 8 10))))
          (%define neg-val (%expr 8 10)))))
    (%%unit
      (%%init
        (%%var %%uint8_t previous-clock 0)
        (%%var %%uint8_t pos-reg 10)
        (%%var %%uint8_t neg-reg 10))
      (%%update
        (%%when (%%not (%%= previous-clock id))
          (%%set previous-clock id)
          (%%if (not (zero? id))
            (%%then
              (%%const %%uint8_t pos-val 10))
            (%%else
              (%%const %%uint8_t neg-val 10))))))))
