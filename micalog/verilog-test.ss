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

(check-expr (_ x) x)

(check-expr (_ 128) 128)

(check-expr
  (_ (%+ (_ a) (_ b)))
  (%%+ a b))

(check-expr
  (_ (%- (_ a) (_ b)))
  (%%- a b))

(check-expr
  (_ (%append (_ a) (_ b)))
  (%%append a b))

(check-expr
  (_ (%and (_ a) (_ b)))
  (%%and a b))

(check-expr
  (_ (%or (_ a) (_ b)))
  (%%or a b))

(check-expr
  (_ (%not (_ a)))
  (%%inv a))

