(import
  (micascheme)
  (micalog micac)
  (prefix (micalog keywords) %)
  (prefix (micac syntax) %%))

(define-rule-syntax (check-expr micalog micac)
  (check
    (equal?
      (syntax->datum (expr->micac #'micalog))
      'micac)))

(check-expr
  (%expr _ x)
  x)

(check-expr
  (%expr _ 128)
  128)

(check-expr
  (%expr 6 (%+ (%expr _ a) (%expr _ b)))
  (%%bitwise-and (%%+ a b) #x3f))

(check-expr
  (%expr 6 (%- (%expr _ a) (%expr _ b)))
  (%%bitwise-and (%%- a b) #x3f))

(check-expr
  (%expr _ (%append (%expr _ a) (%expr 4 b)))
  (%%bitwise-ior (%%bitwise-arithmetic-shift-left a 4) b))

(check-expr
  (%expr _ (%slice (%expr _ a) 3 6))
  (%%bitwise-and (%%bitwise-arithmetic-shift-right a 3) #x3f))

(check-expr
  (%expr _ (%and (%expr _ a) (%expr _ b)))
  (%%bitwise-and a b))

(check-expr
  (%expr _ (%or (%expr _ a) (%expr _ b)))
  (%%bitwise-ior a b))

(check-expr
  (%expr 6 (%not (%expr _ a)))
  (%%bitwise-and (%%bitwise-not a) #x3f))

(check-expr
  (%expr _ (%reg-ref (%expr _ a)))
  a)
