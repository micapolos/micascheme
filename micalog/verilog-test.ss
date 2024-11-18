(import
  (micascheme)
  (micalog verilog)
  (prefix (micalog keywords) %)
  (prefix (verilog keywords) %%))

(define-rule-syntax (check-expr micalog verilog)
  (check
    (equal?
      (syntax->datum (expr->verilog #'micalog))
      'verilog)))

(check-expr
  (%expr _ x)
  x)

(check-expr
  (%expr _ 128)
  128)

(check-expr
  (%expr 6 (%+ (%expr _ a) (%expr _ b)))
  (%%+ a b))

(check-expr
  (%expr 6 (%- (%expr _ a) (%expr _ b)))
  (%%- a b))

(check-expr
  (%expr _ (%append (%expr _ a) (%expr 4 b)))
  (%%append a b))

(check-expr
  (%expr _ (%slice (%expr _ a) 3 6))
  (%%ref a (8 %%to 3)))

(check-expr
  (%expr _ (%and (%expr _ a) (%expr _ b)))
  (%%and a b))

(check-expr
  (%expr _ (%or (%expr _ a) (%expr _ b)))
  (%%or a b))

(check-expr
  (%expr 6 (%not (%expr _ a)))
  (%%inv a))

(check-expr
  (%expr _ (%reg-ref (%expr _ a)))
  a)
