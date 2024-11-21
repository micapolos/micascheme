(library (micalog micac)
  (export
    value->micac
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

  (define (value->micac $value)
    (syntax-case $value ()
      (id (identifier? #'id)
        (id->micac #'id))
      (integer (integer? (datum integer))
        #'integer)))

  (define (expr->micac $expr)
    (syntax-case $expr (%= %!= %< %<= %> %>= %append %slice %add %sub %neg %and %or %xor %nand %nor %xnor %not %if)
      ((%= type lhs rhs)
        (boolean->number
          #`(%%=
            #,(value->micac #'lhs)
            #,(value->micac #'rhs))))
      ((%!= type lhs rhs)
        (boolean->number
          #`(%%not
            (%%=
              #,(value->micac #'lhs)
              #,(value->micac #'rhs)))))
      ((%< type lhs rhs)
        (boolean->number
          #`(%%<
            #,(value->micac #'lhs)
            #,(value->micac #'rhs))))
      ((%<= type lhs rhs)
        (boolean->number
          #`(%%<=
            #,(value->micac #'lhs)
            #,(value->micac #'rhs))))
      ((%> type lhs rhs)
        (boolean->number
          #`(%%>
            #,(value->micac #'lhs)
            #,(value->micac #'rhs))))
      ((%>= type lhs rhs)
        (boolean->number
          #`(%%>=
            #,(value->micac #'lhs)
            #,(value->micac #'rhs))))
      ((%append lhs-type lhs rhs-type rhs)
        #`(%%bitwise-ior
          (%%bitwise-arithmetic-shift-left
            #,(value->micac #'lhs)
            #,(type-size #'rhs-type))
          #,(value->micac #'rhs)))
      ((%slice type rhs shift)
        (type-micac-mask #'type
          #`(%%bitwise-arithmetic-shift-right
            #,(value->micac #'rhs)
            shift)))
      ((%add type lhs rhs)
        (type-micac-mask #'type
          #`(%%+
            #,(value->micac #'lhs)
            #,(value->micac #'rhs))))
      ((%sub type lhs rhs)
        (type-micac-mask #'type
          #`(%%-
            #,(value->micac #'lhs)
            #,(value->micac #'rhs))))
      ((%neg type rhs)
        (type-micac-mask #'type
          #`(%%-
            #,(value->micac #'rhs))))
      ((%and type lhs rhs)
        #`(%%bitwise-and
          #,(value->micac #'lhs)
          #,(value->micac #'rhs)))
      ((%or type lhs rhs)
        #`(%%bitwise-ior
          #,(value->micac #'lhs)
          #,(value->micac #'rhs)))
      ((%xor type lhs rhs)
        #`(%%bitwise-xor
          #,(value->micac #'lhs)
          #,(value->micac #'rhs)))
      ((%nand type lhs rhs)
        (expr->micac #`(%not type (%and type lhs rhs))))
      ((%nor type lhs rhs)
        (expr->micac #`(%not type (%or type lhs rhs))))
      ((%xnor type lhs rhs)
        (expr->micac #`(%not type (%xor type lhs rhs))))
      ((%not type rhs)
        (type-micac-mask #'type
          #`(%%bitwise-not
            #,(value->micac #'rhs))))
      ((%if type cond true false)
        #`(%%if
          (%= #,(value->micac #'cond) 1)
          #,(value->micac #'true)
          #,(value->micac #'false)))
      (other
        (value->micac #'other))))

  (define (boolean->number $boolean?)
    #`(%%if #,$boolean? 1 0))

  (define (reg-value->micac? $reg)
    (syntax-case $reg (%reg)
      ((%reg) #f)
      ((%reg expr) (expr->micac #'expr))))

  (define (instr->micac $instr)
    (fluent
      (empty-block)
      (block+instr $instr)
      (block->micac)))

  (define (instrs->block $instrs)
    (fluent
      (empty-block)
      (with $block (fold-left block+instr $block (syntax->list $instrs)))))

  (define (instrs->micac $instrs)
    (block->micac (instrs->block $instrs)))

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
    (syntax-case $instr (%define %set %on %posedge %negedge)
      ((%define id expr)
        (syntax-case (expr-type #'expr) (%reg)
          ((%reg size)
            (block+init $block
              #`(%%var
                #,(size->micac #'size)
                #,(id->micac #'id)
                #,@(opt->list (reg-value->micac? (expr-value #'expr))))))
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
      ((%on expr
        (%posedge pos-instr ...)
        (%negedge neg-instr ...))
        (lets
          ($previous-id
            (generate-identifier
              (identifier-append #'%on #'previous- (value-id (expr-value #'expr)))))
          ($pos-block (instrs->block #'(pos-instr ...)))
          ($neg-block (instrs->block #'(neg-instr ...)))
          (block
            (append
              (block-inits $neg-block)
              (block-inits $pos-block)
              (list
                #`(%%var
                  #,(size->micac #'1)
                  #,$previous-id
                  0))
              (block-inits $block))
            (cons
              #`(%%when (%%not (%%= #,$previous-id id))
                (%%set #,$previous-id id)
                (%%if (not (zero? id))
                  (%%then
                    #,@(reverse
                      (block-updates $pos-block)))
                  (%%else
                    #,@(reverse
                      (block-updates $neg-block)))))
                (block-updates $block)))))))

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

  (define (value-id $value)
    (syntax-case $value ()
      (id (identifier? #'id) #'id)
      (_ #'value)))

  (define (id->micac $id) $id)

  (define (size-micac-mask $size $micac)
    #`(%%bitwise-and
      #,$micac
      #,(literal->syntax
        (- (bitwise-arithmetic-shift-left 1 (syntax->datum $size)) 1))))

  (define (type-micac-mask $type $micac)
    (size-micac-mask (type-size $type) $micac))
)
