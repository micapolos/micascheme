(library (micalog verilog)
  (export expr->verilog)
  (import
    (micascheme)
    (micalog model)
    (prefix (micalog keywords) %)
    (prefix (verilog keywords) %%))

  (define (expr->verilog $expr)
    (syntax-case $expr (%expr)
      ((%expr type value)
        (syntax-case #'value (%append %slice %+ %- %and %or %not %reg-ref)
          (id (identifier? #'id)
            #'id)
          (integer (integer? (datum integer))
            #'integer)
          ((%append lhs rhs)
            #`(%%append
              #,(expr->verilog #'lhs)
              #,(expr->verilog #'rhs)))
          ((%slice lhs shift cut)
            #`(%%ref
              #,(expr->verilog #'lhs)
              (
                #,(literal->syntax (+ (datum shift) (datum cut) -1))
                %%to
                shift)))
          ((%+ lhs rhs)
            #`(%%+
              #,(expr->verilog #'lhs)
              #,(expr->verilog #'rhs)))
          ((%- lhs rhs)
            #`(%%-
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
              #,(expr->verilog #'rhs)))
          ((%reg-ref rhs)
            (expr->verilog #'rhs))))))
)
