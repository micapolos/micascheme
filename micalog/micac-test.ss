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
(check-micac (expr (%xnor 6 a b)) (%%bitwise-not (%%bitwise-xor a b)))

(check-micac (expr (%not 6 a)) (%%bitwise-and (%%bitwise-not a) #x3f))

(check-micac (expr (%if 6 a b c)) (%%if (%%= a 1) b c))

(check-micac
  (expr (%if 6 (%not 1 a) (%not 6 b) (%and 6 c d)))
  (%%if
    (%%= (%%bitwise-and (%%bitwise-not a) 1) 1)
    (%%bitwise-and (%%bitwise-not b) 63)
    (%%bitwise-and c d)))

(check-micac (size 1) %%uint8_t)
(check-micac (size 8) %%uint8_t)
(check-micac (size 9) %%uint16_t)
(check-micac (size 16) %%uint16_t)
(check-micac (size 17) %%uint32_t)
(check-micac (size 32) %%uint32_t)
(check-micac (size 33) %%uint64_t)
(check-micac (size 64) %%uint64_t)
