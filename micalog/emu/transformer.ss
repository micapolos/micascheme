(library (micalog emu transformer)
  (export
    type->micac
    module->micac
    register->micac
    instruction->micac
    expr->micac
    value->micac)
  (import
    (micascheme)
    (micalog core utils)
    (prefix (micalog keywords) %)
    (prefix (micalog emu keywords) %)
    (prefix (micac) %%)
    (prefix (micac lib emu) %%)
    (prefix (micac lib std) %%))

  ; Requirements:
  ; - module with explicit previous-clock and clock names
  ; - fully typed
  ; - inputs and registers declared at the top-level
  ; - explicit captures to capture previous register values
  ; - "on" statement with explicit previous value
  (define (module->micac $module)
    (syntax-case $module (%module)
      ((%module name statement ...)
        (lets
          ($statements (syntaxes statement ...))
          ($registers (declaration-syntaxes-of %register statement ...))
          ($instructions
            (filter
              (lambda ($statement)
                (and
                  (not (declaration-kind-of? #'%input $statement))
                  (not (declaration-kind-of? #'%register $statement))))
              (syntaxes statement ...)))
          ($video-x-input? (kind-name-find-statement #'%input #'9 #'%video-x $statements))
          ($video-y-input? (kind-name-find-statement #'%input #'9 #'%video-y $statements))
          ($video-red-output? (kind-name-find-statement #'%output #'8 #'%video-red $statements))
          ($video-green-output? (kind-name-find-statement #'%output #'8 #'%video-green $statements))
          ($video-blue-output? (kind-name-find-statement #'%output #'8 #'%video-blue $statements))
          ($mouse-x-input? (kind-name-find-statement #'%input #'9 #'%mouse-x $statements))
          ($mouse-y-input? (kind-name-find-statement #'%input #'9 #'%mouse-y $statements))
          ($mouse-pressed?-input? (kind-name-find-statement #'%input #'1 #'%mouse-pressed? $statements))
          #`(%%run-emu
            (%%video 352 288 96 24 4)
            (%%var bool clock 0)
            #,@(opt->list (and $video-x-input? #`(%%var int %video-x)))
            #,@(opt->list (and $video-y-input? #`(%%var int %video-y)))
            #,@(opt->list (and $mouse-x-input? #`(%%var int %mouse-x)))
            #,@(opt->list (and $mouse-y-input? #`(%%var int %mouse-y)))
            #,@(opt->list (and $mouse-pressed?-input? #`(%%var bool %mouse-pressed?)))
            #,@(map register->micac $registers)
            (%%update
              (%%set clock (%%xor clock 1))
              #,@(opt->list (and $video-x-input? #`(%%set %video-x %%video-x)))
              #,@(opt->list (and $video-y-input? #`(%%set %video-y %%video-y)))
              #,@(opt->list (and $mouse-x-input? #`(%%set %mouse-x %%mouse-x)))
              #,@(opt->list (and $mouse-y-input? #`(%%set %mouse-y %%mouse-y)))
              #,@(opt->list (and $mouse-pressed?-input? #`(%%set %mouse-pressed? %%mouse-pressed?)))
              #,@(map instruction->micac $instructions)
              #,@(opt->list (and $video-red-output? #`(%%set %%red %video-red)))
              #,@(opt->list (and $video-green-output? #`(%%set %%green %video-green)))
              #,@(opt->list (and $video-blue-output? #`(%%set %%blue %video-blue)))))))))

  (define (register->micac $statement)
    (syntax-case $statement (%register)
      ((%register type name)
        #`(%%var
          #,(type->micac #'type)
          #,(name->micac #'name)))))

  (define (instruction->micac $statement)
    (syntax-case $statement (%output %wire %set %on %cond %else)
      ((%output type name expr)
        #`(%%var
          #,(type->micac #'type)
          #,(name->micac #'name)
          #,(expr->micac #'expr)))
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

  (define (kind-name-find-statement $kind $type $name $statements)
    (find
      (lambda ($statement)
        (syntax-case $statement ()
          ((kind type name body ...)
            (and
              (free-identifier=? #'kind $kind)
              (free-identifier=? #'name $name)
              (or
                (syntax=? #'type $type)
                (syntax-error $statement
                  (format "invalid type ~a, expected ~a in"
                    (syntax->datum #'type)
                    (syntax->datum $type))))
              $statement))))
       $statements))
)
