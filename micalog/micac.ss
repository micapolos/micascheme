(library (micalog micac)
  (export
    expr->micac)
  (import
    (micascheme)
    (micalog model)
    (prefix (micalog keywords) %)
    (prefix (micac syntax) %%))

  (define (expr->micac $expr)
    (syntax-case $expr ()
      (expr
        (syntax-case (expr-term #'expr) (%+ %- %append)
          (id (identifier? #'id)
            #'id)
          (integer (integer? (datum integer))
            #'integer)
          ((%+ lhs rhs)
            #`(%%+
              #,(expr->micac #'lhs)
              #,(expr->micac #'rhs)))
          ((%- lhs rhs)
            #`(%%-
              #,(expr->micac #'lhs)
              #,(expr->micac #'rhs)))
          ((%append lhs rhs)
            #`(%%bitwise-ior
              (%%bitwise-arithmetic-shift-left
                #,(expr->micac #'lhs)
                #,(expr-size #'rhs))
              #,(expr->micac #'rhs)))))))
)
