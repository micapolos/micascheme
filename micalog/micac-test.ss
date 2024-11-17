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

(check-expr (_ _ x) x)

(check-expr (_ _ 128) 128)

(check-expr
  (_ 6 (%+ (_ _ a) (_ _ b)))
  (%%bitwise-and (%%+ a b) #x3f))

(check-expr
  (_ 6 (%- (_ _ a) (_ _ b)))
  (%%bitwise-and (%%- a b) #x3f))

(check-expr
  (_ _ (%append (_ _ a) (_ 4 b)))
  (%%bitwise-ior (%%bitwise-arithmetic-shift-left a 4) b))

(check-expr
  (_ _ (%and (_ _ a) (_ _ b)))
  (%%bitwise-and a b))

(check-expr
  (_ _ (%or (_ _ a) (_ _ b)))
  (%%bitwise-ior a b))

(check-expr
  (_ 6 (%not (_ _ a)))
  (%%bitwise-and (%%bitwise-not a) #x3f))
