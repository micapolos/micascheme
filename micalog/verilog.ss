(library (micalog verilog)
  (export expr->verilog)
  (import
    (micascheme)
    (micalog model)
    (prefix (micalog keywords) %)
    (prefix (verilog keywords) %%))

  (define (expr->verilog $expr)
    (syntax-case $expr ()
      ((type term)
        (syntax-case #'term (%+ %- %append %and %or %not)
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
              #,(expr->verilog #'rhs)))
          ((%and lhs rhs)
            #`(%%and
              #,(expr->verilog #'lhs)
              #,(expr->verilog #'rhs)))
          ((%or lhs rhs)
            #`(%%or
              #,(expr->verilog #'lhs)
              #,(expr->verilog #'rhs)))
          ((%not rhs)
            #`(%%inv
              #,(expr->verilog #'rhs)))))))
)
