(library (micalog verilog)
  (export expr->verilog)
  (import
    (micascheme)
    (micalog model)
    (prefix (micalog keywords) %)
    (prefix (verilog keywords) %%))

  (define (expr->verilog $expr)
    (syntax-case $expr ()
      (expr
        (syntax-case (expr-term #'expr) (%+ %- %append)
          (id (identifier? #'id)
            #'id)
          (integer (integer? (datum integer))
            #'integer)
          ((%+ lhs rhs)
            #`(%%+
              #,(expr->verilog #'lhs)
              #,(expr->verilog #'rhs)))
          ((%- lhs rhs)
            #`(%%-
              #,(expr->verilog #'lhs)
              #,(expr->verilog #'rhs)))
          ((%append lhs rhs)
            #`(%%append
              #,(expr->verilog #'lhs)
              #,(expr->verilog #'rhs)))))))
)
