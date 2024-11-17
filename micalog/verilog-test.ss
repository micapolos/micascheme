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

(check-expr (_ _ x) x)

(check-expr (_ _ 128) 128)

(check-expr
  (_ _ (%+ (_ _ a) (_ _ b)))
  (%%+ a b))

(check-expr
  (_ _ (%- (_ _ a) (_ _ b)))
  (%%- a b))

(check-expr
  (_ _ (%append (_ _ a) (_ 4 b)))
  (%%append a b))

(check-expr
  (_ _ (%and (_ _ a) (_ _ b)))
  (%%and a b))

(check-expr
  (_ _ (%or (_ _ a) (_ _ b)))
  (%%or a b))

(check-expr
  (_ _ (%not (_ _ a)))
  (%%inv a))

