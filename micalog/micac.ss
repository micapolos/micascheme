(library (micalog micac)
  (export
    expr->micac
    instr->micac
    instrs->micac
    size->micac
    %%unit %%init %%update)
  (import
    (micascheme)
    (micalog model)
    (prefix (micalog keywords) %)
    (prefix (micac syntax) %%))

  (define-aux-keywords %%unit %%init %%update)
  (data (block inits updates))

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

  (define (instr->micac $instr)
    (fluent
      (empty-block)
      (block+instr $instr)
      (block->micac)))

  (define (instrs->micac $instrs)
    (fluent
      (empty-block)
      (with $block (fold-left block+instr $block (syntax->list $instrs)))
      (block->micac)))

  (define (empty-block)
    (block (stack) (stack)))

  (define (block+init $block $init)
    (block-with-inits $block
      (push (block-inits $block) $init)))

  (define (block+update $block $update)
    (block-with-updates $block
      (push (block-updates $block) $update)))

  (define (block->micac (block $inits $updates))
    #`(%%unit
      (%%init #,@(reverse $inits))
      (%%update #,@(reverse $updates))))

  (define (block+instr $block $instr)
    (syntax-case $instr (%val %set %on %posedge %negedge)
      ((%val id expr)
        (syntax-case (expr-type #'expr) (%reg)
          ((%reg size)
            (block+init $block
              #`(%%var
                #,(size->micac #'size)
                #,(id->micac #'id)
                #,(expr->micac #'expr))))
          (size
            (block+update $block
              #`(%%const
                #,(size->micac #'size)
                #,(id->micac #'id)
                #,(expr->micac #'expr))))))
      ((%set! lhs rhs)
        (block+update $block
          #`(%%set
            #,(expr->micac #'lhs)
            #,(expr->micac #'rhs))))
      ((%on (%expr 1 id)
        (%posedge pos-instr ...)
        (%negedge neg-instr ...))
        (lets
          ($previous-id (generate-identifier (identifier id)))
          (fluent $block
            (block+init
              #`(%%var
                #,(size->micac #'1)
                #,$previous-id
                0))
            (block+update
              #`(%%when (%%not (%%= #,$previous-id id))
                (%%set #,$previous-id id)
                (%%if (not (zero? id))
                  (%%then pos-instr ...)
                  (%%else neg-instr ...)))))))))

  (define (size->micac $size)
    (syntax-case $size ()
      (number (positive-integer? (datum number))
        (lets
          ($number (datum number))
          (cond
            ((<= $number 8) #'%%uint8_t)
            ((<= $number 16) #'%%uint16_t)
            ((<= $number 32) #'%%uint32_t)
            ((<= $number 64) #'%%uint64_t)
            (else (syntax-error $size)))))))

  (define (id->micac $id) $id)

  (define (size-micac-mask $size $micac)
    #`(%%bitwise-and
      #,$micac
      #,(literal->syntax
        (- (bitwise-arithmetic-shift-left 1 (syntax->datum $size)) 1))))

  (define (type-micac-mask $type $micac)
    (size-micac-mask (type-size $type) $micac))
)
