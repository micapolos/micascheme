(library (micalog typed)
  (export
    literal->typed
    expr->typed
    type->syntax
    scope-expr->typed
    scope-instr->typed-syntax
    scope-instrs->typed-syntax)
  (import
    (micascheme)
    (syntax scope)
    (syntax scoped)
    (prefix (micalog keywords) %))

  (define (binding-kind $binding)
    (syntax-case $binding ()
      ((kind type) #'kind)))

  (define (binding-type $binding)
    (syntax-case $binding ()
      ((kind type) #'type)))

  (define (binding $kind $type)
    #`(#,$kind #,$type))

  (define (type->syntax $type)
    (literal->syntax (type-size $type)))

  (define (literal->typed? $literal)
    (syntax-case $literal ()
      (0 #`(1 0))
      (1 #`(1 1))
      (id (identifier? #'id)
        (or
          (prefix-size-id->typed? "bin-" 1 #'id)
          (prefix-size-id->typed? "oct-" 3 #'id)
          (prefix-size-id->typed? "hex-" 4 #'id)))
      (else #f)))

  (define (prefix-size-id->typed? $prefix $size $id)
    (lets
      ($string (symbol->string (syntax->datum $id)))
      ($length (string-length $string))
      ($prefix-length (string-length $prefix))
      ($radix (bitwise-arithmetic-shift-left 1 $size))
      (and (> $length $prefix-length)
        (lets
          ($string-prefix (substring $string 0 $prefix-length))
          ($data (substring $string $prefix-length $length))
          ($data-length (string-length $data))
          (and (string=? $string-prefix $prefix)
            (lets
              ($number (string->number $data $radix))
              (if $number
                #`(
                  #,(literal->syntax (* $data-length $size))
                  #,(literal->syntax $number))
                (syntax-error $id "illegal digits"))))))))

  (define (literal->typed $literal)
    (or
      (literal->typed? $literal)
      (syntax-error $literal "invalid literal")))

  (define (scope-id->typed $scope $id)
    #`(#,(binding-type (scope-item $scope $id)) #,$id))

  (define (scope-type-expr->typed $scope $expected-type $expr)
    (lets
      ($typed (scope-expr->typed $scope $expr))
      ($type (typed-type $typed))
      (if (type=? $type $expected-type)
        $typed
        (syntax-error $expr
          (format "type mismatch ~a, expected ~a in"
            (syntax->datum $type)
            (syntax->datum $expected-type))))))

  (define (scope-expr->typed $scope $expr)
    (or
      (literal->typed? $expr)
      (syntax-case $expr (%append %slice %= %!= %< %<= %> %>= %not %and %or %xor %nand %nor %xnor %+ %- %if)
        (id (identifier? #'id) (scope-id->typed $scope #'id))
        ((%append x ...) (scope-append->typed $scope $expr))
        ((%slice x ...) (scope-slice->typed $scope $expr))
        ((%= x ...) (scope-op2->typed $scope $expr))
        ((%!= x ...) (scope-op2->typed $scope $expr))
        ((%< x ...) (scope-op2->typed $scope $expr))
        ((%<= x ...) (scope-op2->typed $scope $expr))
        ((%> x ...) (scope-op2->typed $scope $expr))
        ((%>= x ...) (scope-op2->typed $scope $expr))
        ((%not x ...) (scope-op1->typed $scope $expr))
        ((%and x ...) (scope-op2->typed $scope $expr))
        ((%or x ...) (scope-op2->typed $scope $expr))
        ((%xor x ...) (scope-op2->typed $scope $expr))
        ((%nand x ...) (scope-op2->typed $scope $expr))
        ((%nor x ...) (scope-op2->typed $scope $expr))
        ((%xnor x ...) (scope-op2->typed $scope $expr))
        ((%+ x ...) (scope-op2->typed $scope $expr))
        ((%- x ...) (scope-op1/2->typed $scope $expr))
        ((%if x ...) (scope-if->typed $scope $expr)))))

  (define expr->typed
    (partial scope-expr->typed (empty-scope)))

  (define (scope-op1->typed $scope $expr)
    (syntax-case $expr ()
      ((op a)
        (lets
          ($typed-a (scope-expr->typed $scope #'a))
          #`(op
            #,(typed-type $typed-a)
            #,(typed-value $typed-a))))))

  (define (scope-op2->typed $scope $expr)
    (syntax-case $expr ()
      ((op a b)
        (lets
          ($typed-a (scope-expr->typed $scope #'a))
          ($type-a (typed-type $typed-a))
          ($typed-b (scope-type-expr->typed $scope $type-a #'b))
          #`(op
            #,$type-a
            #,(typed-value $typed-a)
            #,(typed-value $typed-b))))))

  (define (scope-op1/2->typed $scope $expr)
    (syntax-case $expr ()
      ((_ _) (scope-op1->typed $scope $expr))
      ((_ _ _) (scope-op2->typed $scope $expr))))

  (define (scope-if->typed $scope $if)
    (syntax-case $if (%if)
      ((%if a b c)
        (lets
          ($typed-a (scope-type-expr->typed $scope #'1 #'a))
          ($typed-b (scope-expr->typed $scope #'b))
          ($type-b (typed-type $typed-b))
          ($typed-c (scope-type-expr->typed $scope $type-b #'c))
          #`(%if
            #,$type-b
            #,(typed-value $typed-a)
            #,(typed-value $typed-b)
            #,(typed-value $typed-c))))))

  (define (scope-append->typed $scope $append)
    (syntax-case $append (%append)
      ((%append a b)
        (lets
          ($typed-a (scope-expr->typed $scope #'a))
          ($typed-b (scope-expr->typed $scope #'b))
          ($type-a (typed-type $typed-a))
          ($type-b (typed-type $typed-b))
          #`(%append
            #,(size->type (+ (type-size $type-a) (type-size $type-b)))
            #,(typed-value $typed-a)
            #,(typed-value $typed-b))))))

  (define (scope-slice->typed $scope $slice)
    (syntax-case $slice (%slice)
      ((%slice a shift size)
        (lets
          ($typed-a (scope-expr->typed $scope #'a))
          ($type-a (typed-type $typed-a))
          ($a-size (type-size $type-a))
          ($shift (shift-number #'shift))
          ($size (type-size #'size))
          (if
            (>= $a-size (+ $shift $size))
            #`(%slice
              size
              #,(typed-value $typed-a)
              shift)
            (syntax-error #'a
              (format "type mismatch ~a, expected ~a in"
                (syntax->datum $type-a)
                (syntax->datum #`(>= (+ shift size))))))))
      ((%slice a size)
        (lets
          ($typed-a (scope-expr->typed $scope #'a))
          ($type-a (typed-type $typed-a))
          ($a-size (type-size $type-a))
          ($size (type-size #'size))
          (if
            (>= $a-size $size)
            #`(%slice
              size
              #,(typed-value $typed-a)
              0)
            (syntax-error #'a
              (format "type mismatch ~a, expected ~a in"
                (syntax->datum $type-a)
                (syntax->datum #`(>= size)))))))))

  (define (scoped-syntaxes+instr (scoped $scope $syntaxes) $instr)
    (syntax-case $instr (%input %output %wire %register %set %when %if %on)
      ((%input id type)
        (lets
          ($type (type->syntax #'type))
          (scoped
            (scope+ $scope (identifier id) (binding #'%wire $type))
            (push $syntaxes #`(%input #,$type id)))))
      ((%output id expr)
        (lets
          ($typed (scope-expr->typed $scope #'expr))
          ($type (typed-type $typed))
          (scoped
            (scope+ $scope (identifier id) (binding #'%wire $type))
            (push $syntaxes #`(%output #,$type id #,(typed-value $typed))))))
      ((%wire id expr)
        (lets
          ($typed (scope-expr->typed $scope #'expr))
          ($type (typed-type $typed))
          (scoped
            (scope+ $scope (identifier id) (binding #'%wire $type))
            (push $syntaxes #`(%wire #,$type id #,(typed-value $typed))))))
      ((%set id expr)
        (lets
          ($id-binding (scope-item $scope (identifier id)))
          ($id-kind (binding-kind $id-binding))
          ($id-type (binding-type $id-binding))
          ($typed (scope-type-expr->typed $scope $id-type #'expr))
          (if (syntax=? $id-kind #'%register)
            (scoped $scope
              (push $syntaxes #`(%set #,$id-type id #,(typed-value $typed))))
            (syntax-error $instr
              (format "type mismatch ~a, expected ~a in"
                (syntax->datum #`(%set #,$id-binding #,$id-type))
                (syntax->datum #`(%set (%register #,$id-type) #,$id-type)))))))
      ((%when cond body ...)
        (scoped $scope
          (push $syntaxes
            #`(%when
              #,(typed-value (scope-type-expr->typed $scope #'1 #'cond))
              #,@(syntax->list (scope-instrs->typed-syntax $scope #'(body ...)))))))
      ((%register xs ...)
        (scoped $scope $syntaxes))))

  (define (scoped-syntaxes+instrs $scoped $instrs)
    (fold-left scoped-syntaxes+instr $scoped (syntax->list $instrs)))

  (define (scope+init $scope $init)
    (syntax-case $init (%register)
      ((%register id type)
        (scope+ $scope
          (identifier id)
          (binding #'%register (type->syntax #'type))))
      (_
        $scope)))

  (define (scope+inits $scope $inits)
    (fold-left scope+init $scope (syntax->list $inits)))

  (define (scope-instrs->typed-syntax $scope $instrs)
    (fluent
      (scoped (scope+inits $scope $instrs) (stack))
      (scoped-syntaxes+instrs (syntax->list $instrs))
      (scoped-value)
      (reverse)
      (list->syntax)))

  (define (scope-instr->typed-syntax $scope $instr)
    (syntax-single (scope-instrs->typed-syntax $scope #`(#,$instr))))

  (define (typed $type $value)
    #`(#,$type #,$value))

  (define (typed-type $typed)
    (syntax-case $typed ()
      ((type value) #'type)))

  (define (typed-value $typed)
    (syntax-case $typed ()
      ((type value) #'value)))

  (define (type-size $type)
    (syntax-case $type ()
      (size (positive-integer? (datum size)) (datum size))
      (_ (syntax-error $type "illegal type"))))

  (define (shift-number $shift)
    (syntax-case $shift ()
      (size (nonnegative-integer? (datum size)) (datum size))
      (_ (syntax-error $shift "illegal shift"))))

  (define (size->type $size)
    (literal->syntax $size))

  (define type=? syntax=?)
)
