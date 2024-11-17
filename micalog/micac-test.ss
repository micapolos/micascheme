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

(check-expr
  (_ _ (%+ (_ _ a) (_ _ b)))
  (%%+ a b))

(check-expr
  (_ _ (%- (_ _ a) (_ _ b)))
  (%%- a b))

(check-expr
  (_ _ (%append (_ _ a) (_ 4 b)))
  (%%bitwise-ior (%%bitwise-arithmetic-shift-left a 4) b))

