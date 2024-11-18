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
      ((type term)
        (syntax-case #'term (%+ %- %append %and %or %not)
          (id (identifier? #'id)
            #'id)
          (integer (integer? (datum integer))
            #'integer)
          ((%+ lhs rhs)
            (micac-mask #'type
              #`(%%+
                #,(expr->micac #'lhs)
                #,(expr->micac #'rhs))))
          ((%- lhs rhs)
            (micac-mask #'type
              #`(%%-
                #,(expr->micac #'lhs)
                #,(expr->micac #'rhs))))
          ((%append lhs (rhs-type rhs-term))
            #`(%%bitwise-ior
              (%%bitwise-arithmetic-shift-left
                #,(expr->micac #'lhs)
                #,(syntax->datum #'rhs-type))
              #,(expr->micac #'(rhs-type rhs-term))))
          ((%and lhs rhs)
            #`(%%bitwise-and
              #,(expr->micac #'lhs)
              #,(expr->micac #'rhs)))
          ((%or lhs rhs)
            #`(%%bitwise-ior
              #,(expr->micac #'lhs)
              #,(expr->micac #'rhs)))
          ((%not rhs)
            (micac-mask #'type
              #`(%%bitwise-not
                #,(expr->micac #'rhs))))))))

  (define (micac-mask $type $micac)
    #`(%%bitwise-and
      #,$micac
      #,(datum->syntax #'%%bitwise-and
        (- (bitwise-arithmetic-shift-left 1 (syntax->datum $type)) 1))))
)
