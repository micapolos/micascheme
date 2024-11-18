(library (micalog micac)
  (export
    expr->micac)
  (import
    (micascheme)
    (micalog model)
    (prefix (micalog keywords) %)
    (prefix (micac syntax) %%))

  (define (expr->micac $expr)
    (syntax-case $expr (%expr)
      ((%expr type term)
        (syntax-case #'term (%+ %- %append %slice %and %or %not %reg-ref)
          (id (identifier? #'id)
            #'id)
          (integer (integer? (datum integer))
            #'integer)
          ((%append lhs rhs)
            #`(%%bitwise-ior
              (%%bitwise-arithmetic-shift-left
                #,(expr->micac #'lhs)
                #,(type-size (expr-type #'rhs)))
              #,(expr->micac #'rhs)))
          ((%slice lhs shift size)
            (size-micac-mask (datum size)
              #`(%%bitwise-arithmetic-shift-right
                #,(expr->micac #'lhs)
                shift)))
          ((%+ lhs rhs)
            (type-micac-mask #'type
              #`(%%+
                #,(expr->micac #'lhs)
                #,(expr->micac #'rhs))))
          ((%- lhs rhs)
            (type-micac-mask #'type
              #`(%%-
                #,(expr->micac #'lhs)
                #,(expr->micac #'rhs))))
          ((%and lhs rhs)
            #`(%%bitwise-and
              #,(expr->micac #'lhs)
              #,(expr->micac #'rhs)))
          ((%or lhs rhs)
            #`(%%bitwise-ior
              #,(expr->micac #'lhs)
              #,(expr->micac #'rhs)))
          ((%not rhs)
            (type-micac-mask #'type
              #`(%%bitwise-not
                #,(expr->micac #'rhs))))
          ((%reg-ref rhs)
            (expr->micac #'rhs))))))

  (define (size-micac-mask $size $micac)
    #`(%%bitwise-and
      #,$micac
      #,(datum->syntax #'+
        (- (bitwise-arithmetic-shift-left 1 (syntax->datum $size)) 1))))

  (define (type-micac-mask $type $micac)
    (size-micac-mask (type-size $type) $micac))
)
