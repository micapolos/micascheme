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

(check-expr (_ x) x)

(check-expr (_ 128) 128)

(check-expr
  (6 (%+ (_ a) (_ b)))
  (%%bitwise-and (%%+ a b) #x3f))

(check-expr
  (6 (%- (_ a) (_ b)))
  (%%bitwise-and (%%- a b) #x3f))

(check-expr
  (_ (%append (_ a) (4 b)))
  (%%bitwise-ior (%%bitwise-arithmetic-shift-left a 4) b))

(check-expr
  (_ (%and (_ a) (_ b)))
  (%%bitwise-and a b))

(check-expr
  (_ (%or (_ a) (_ b)))
  (%%bitwise-ior a b))

(check-expr
  (6 (%not (_ a)))
  (%%bitwise-and (%%bitwise-not a) #x3f))
