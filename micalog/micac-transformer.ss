(library (micalog micac-transformer)
  (export
    type->micac
    module->micac
    input-param->micac
    register->micac
    instruction->micac
    expr->micac
    value->micac)
  (import
    (micascheme)
    (micalog utils)
    (prefix (micalog keywords) %)
    (prefix (micac) %%)
    (prefix (micac lib emu) %%)
    (prefix (micac lib std) %%))

  ; Requirements:
  ; - module with explicit previous-clock and clock names
  ; - fully typed
  ; - all inputs and registers declared at the top-level
  ; - no outputs
  ; - explicit captures to capture previous register values
  ; - "on" statement with explicit previous value
  (define (module->micac $module)
    (syntax-case $module (%module)
      ((%module (previous-clock clock) statement ...)
        (lets
          ($inputs (declaration-syntaxes-of %input statement ...))
          ($registers (declaration-syntaxes-of %register statement ...))
          ($instructions
            (filter
              (lambda ($statement)
                (and
                  (not (declaration-kind-of? #'%input $statement))
                  (not (declaration-kind-of? #'%register $statement))))
              (syntaxes statement ...)))
          #`(%%run-emu
            (%%video 352 288 96 24 4)
            (%%var uint8_t previous-clock 0)
            (%%var uint8_t clock 1)
            #,@(map register->micac $registers)
            (%%update
              (%%set previous-clock clock)
              (%%set clock (%%xor clock 1))
              #,@(map instruction->micac $instructions)))))))

  (define (input-param->micac $input)
    (syntax-case $input (%input)
      ((%input type name)
        (name->micac #'name))))

  (define (register->micac $statement)
    (syntax-case $statement (%register)
      ((%register type name)
        #`(%%var
          #,(type->micac #'type)
          #,(name->micac #'name)))))

  (define (instruction->micac $statement)
    (syntax-case $statement (%capture %output %wire %set %on %cond %else)
      ((%capture type name expr)
        #`(%%const
          #,(type->micac #'type)
          #,(name->micac #'name)
          #,(expr->micac #'expr)))
      ((%output type name)
        #`(%%var
          #,(type->micac #'type)
          #,(name->micac #'name)))
      ((%wire type name expr)
        #`(%%const
          #,(type->micac #'type)
          #,(name->micac #'name)
          #,(expr->micac #'expr)))
      ((%set type name expr)
        #`(%%set
          #,(name->micac #'name)
          #,(expr->micac #'expr)))
      ((%cond clause ... (%else els ...))
        #`(%%cond
          #,@(map clause->micac (syntaxes clause ...))
          (%%else #,@(map instruction->micac (syntaxes els ...)))))
      ((%cond clause-1 clause ...)
        #`(%%cond
          #,@(map clause->micac (syntaxes clause-1 clause ...))))
      ((%on (previous-name name) (edge statement ...))
        #`(%%when (%%not (%%= #,(name->micac #'previous-name) #,(name->micac #'name)))
          (%%when (%%= #,(name->micac #'name) #,(edge->micac #'edge))
            #,@(map instruction->micac (syntaxes statement ...)))))
      ((%on (previous-name name) (edge statement ...) (%else else-statement ...))
        #`(%%when (%%not (%%= #,(name->micac #'previous-name) #,(name->micac #'name)))
          (%%if (%%= #,(name->micac #'name) #,(edge->micac #'edge))
            (%%then #,@(map instruction->micac (syntaxes statement ...)))
            (%%else #,@(map instruction->micac (syntaxes else-statement ...))))))))

  (define (clause->micac $clause)
    (syntax-case $clause ()
      ((cond instruction ...)
        #`(
          #,(expr->micac #'cond)
          #,@(map instruction->micac (syntaxes instruction ...))))))

  (define (edge->micac $edge)
    (syntax-case $edge (%posedge %negedge)
      (%posedge #'1)
      (%negedge #'0)))

  (define (value->micac $value)
    (syntax-case $value ()
      (id (identifier? #'id)
        (name->micac #'id))
      (integer (integer? (datum integer))
        #'integer)))

  (define (expr->micac $expr)
    (syntax-case $expr (%= %!= %< %<= %> %>= %append %slice %+ %- %and %or %xor %nand %nor %xnor %not %if)
      ((%= type lhs rhs)
        #`(%%=
          #,(expr->micac #'lhs)
          #,(expr->micac #'rhs)))
      ((%!= type lhs rhs)
        #`(%%not
          (%%=
            #,(expr->micac #'lhs)
            #,(expr->micac #'rhs))))
      ((%< type lhs rhs)
        #`(%%<
          #,(expr->micac #'lhs)
          #,(expr->micac #'rhs)))
      ((%<= type lhs rhs)
        #`(%%<=
          #,(expr->micac #'lhs)
          #,(expr->micac #'rhs)))
      ((%> type lhs rhs)
        #`(%%>
          #,(expr->micac #'lhs)
          #,(expr->micac #'rhs)))
      ((%>= type lhs rhs)
        #`(%%>=
          #,(expr->micac #'lhs)
          #,(expr->micac #'rhs)))
      ((%append (lhs-type lhs))
        (expr->micac #'lhs))
      ((%append (lhs-type lhs) (rhs-type rhs))
        #`(%%bitwise-ior
          (%%bitwise-arithmetic-shift-left
            #,(expr->micac #'lhs)
            #,(type-size #'rhs-type))
          #,(expr->micac #'rhs)))
      ((%append (lhs-type lhs-expr) rhs ...)
        (fold-left
          micac-append
          (expr->micac #'lhs-expr)
          (syntaxes rhs ...)))
      ((%slice type rhs shift)
        (type-micac-mask #'type
          #`(%%bitwise-arithmetic-shift-right
            #,(expr->micac #'rhs)
            shift)))
      ((%+ type lhs rhs)
        (type-micac-mask #'type
          #`(%%+
            #,(expr->micac #'lhs)
            #,(expr->micac #'rhs))))
      ((%- type lhs rhs)
        (type-micac-mask #'type
          #`(%%-
            #,(expr->micac #'lhs)
            #,(expr->micac #'rhs))))
      ((%- type rhs)
        (type-micac-mask #'type
          #`(%%-
            #,(expr->micac #'rhs))))
      ((%and type lhs rhs)
        #`(
          #,(if (type-boolean? #'type) #'%%and #'%%bitwise-and)
          #,(expr->micac #'lhs)
          #,(expr->micac #'rhs)))
      ((%or type lhs rhs)
        #`(
          #,(if (type-boolean? #'type) #'%%or #'%%bitwise-ior)
          #,(expr->micac #'lhs)
          #,(expr->micac #'rhs)))
      ((%xor type lhs rhs)
        #`(
          %%bitwise-xor
          #,(expr->micac #'lhs)
          #,(expr->micac #'rhs)))
      ((%nand type lhs rhs)
        (expr->micac #`(%not type (%and type lhs rhs))))
      ((%nor type lhs rhs)
        (expr->micac #`(%not type (%or type lhs rhs))))
      ((%xnor type lhs rhs)
        (expr->micac #`(%not type (%xor type lhs rhs))))
      ((%not type rhs)
        (type-micac-mask #'type
          #`(
            #,(if (type-boolean? #'type) #'%%not #'%%bitwise-not)
            #,(expr->micac #'rhs))))
      ((%if type cond true false)
        #`(%%if
          #,(expr->micac #'cond)
          #,(expr->micac #'true)
          #,(expr->micac #'false)))
      (other
        (value->micac #'other))))

  (define (micac-append $lhs-micac $rhs)
    (syntax-case $rhs ()
      ((type expr)
        #`(%%bitwise-ior
          (%%bitwise-arithmetic-shift-left
            #,$lhs-micac
            #,(type-size #'type))
          #,(expr->micac #'expr)))))

  (define (boolean->number $boolean?)
    #`(%%if #,$boolean? 1 0))

  (define (reg-value->micac? $reg)
    (syntax-case $reg (%reg)
      ((%reg) #f)
      ((%reg expr) (expr->micac #'expr))))

  (define (type->micac $size)
    (syntax-case $size ()
      (number (positive-integer? (datum number))
        (lets
          ($number (datum number))
          (cond
            ((= $number 1) #'bool)
            ((<= $number 8) #'uint8_t)
            ((<= $number 16) #'uint16_t)
            ((<= $number 32) #'uint32_t)
            ((<= $number 64) #'uint64_t)
            (else (syntax-error $size)))))))

  (define (name->micac $id) $id)

  (define (size-micac-mask $size $micac)
    (if (size-needs-mask? $size)
      #`(%%bitwise-and
        #,$micac
        #,(literal->syntax
          (- (bitwise-arithmetic-shift-left 1 (syntax->datum $size)) 1)))
      $micac))

  (define (size-needs-mask? $size)
    (syntax-case $size ()
      (1 #f)
      (8 #f)
      (16 #f)
      (32 #f)
      (64 #f)
      (_ #t)))

  (define (type-boolean? $type)
    (syntax-case $type ()
      (1 #t)
      (_ #f)))

  (define (type-micac-mask $type $micac)
    (size-micac-mask (type-size $type) $micac))
)
