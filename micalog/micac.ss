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
        (syntax-case (expr-term #'expr) (%+ %- %append %and %or %not)
          (id (identifier? #'id)
            #'id)
          (integer (integer? (datum integer))
            #'integer)
          ((%+ lhs rhs)
            (micac-mask
              #`(%%+
                #,(expr->micac #'lhs)
                #,(expr->micac #'rhs))
              (expr-size #'expr)))
          ((%- lhs rhs)
            (micac-mask
              #`(%%-
                #,(expr->micac #'lhs)
                #,(expr->micac #'rhs))
              (expr-size #'expr)))
          ((%append lhs rhs)
            #`(%%bitwise-ior
              (%%bitwise-arithmetic-shift-left
                #,(expr->micac #'lhs)
                #,(expr-size #'rhs))
              #,(expr->micac #'rhs)))
          ((%and lhs rhs)
            #`(%%bitwise-and
              #,(expr->micac #'lhs)
              #,(expr->micac #'rhs)))
          ((%or lhs rhs)
            #`(%%bitwise-ior
              #,(expr->micac #'lhs)
              #,(expr->micac #'rhs)))
          ((%not rhs)
            (micac-mask
              #`(%%bitwise-not
                #,(expr->micac #'rhs))
              (expr-size #'expr)))))))

  (define (micac-mask $micac $size)
    #`(%%bitwise-and
      #,$micac
      #,(datum->syntax #'%%bitwise-and
        (- (bitwise-arithmetic-shift-left 1 $size) 1))))
)
