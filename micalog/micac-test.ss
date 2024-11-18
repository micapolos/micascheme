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

(check-micac
  (expr (%expr _ x))
  x)

(check-micac
  (expr (%expr _ 128))
  128)

(check-micac
  (expr (%expr 6 (%+ (%expr _ a) (%expr _ b))))
  (%%bitwise-and (%%+ a b) #x3f))

(check-micac
  (expr (%expr 6 (%- (%expr _ a) (%expr _ b))))
  (%%bitwise-and (%%- a b) #x3f))

(check-micac
  (expr (%expr _ (%append (%expr _ a) (%expr 4 b))))
  (%%bitwise-ior (%%bitwise-arithmetic-shift-left a 4) b))

(check-micac
  (expr (%expr _ (%slice (%expr _ a) 3 6)))
  (%%bitwise-and (%%bitwise-arithmetic-shift-right a 3) #x3f))

(check-micac
  (expr (%expr _ (%and (%expr _ a) (%expr _ b))))
  (%%bitwise-and a b))

(check-micac
  (expr (%expr _ (%or (%expr _ a) (%expr _ b))))
  (%%bitwise-ior a b))

(check-micac
  (expr (%expr 6 (%not (%expr _ a))))
  (%%bitwise-and (%%bitwise-not a) #x3f))

(check-micac
  (expr (%expr _ (%reg-ref (%expr _ a))))
  a)

(check-micac (size 1) %%uint8_t)
(check-micac (size 8) %%uint8_t)
(check-micac (size 9) %%uint16_t)
(check-micac (size 16) %%uint16_t)
(check-micac (size 17) %%uint32_t)
(check-micac (size 32) %%uint32_t)
(check-micac (size 33) %%uint64_t)
(check-micac (size 64) %%uint64_t)

(check-micac
  (instr (%val foo (%expr 9 12)))
  (%%unit
    (%%init)
    (%%update (%%const %%uint16_t foo 12))))

(check-micac
  (instr (%val foo (%expr (%reg 9) 12)))
  (%%unit
    (%%init (%%var %%uint16_t foo 12))
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
          (%val pos-reg (%expr (%reg 8) 10))
          (%val pos-val (%expr 8 10)))
        (%negedge
          (%val neg-reg (%expr (%reg 8) 10))
          (%val neg-val (%expr 8 10)))))
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
