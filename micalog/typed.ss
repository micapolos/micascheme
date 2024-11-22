(library (micalog typed)
  (export
    literal->typed
    expr->typed
    scope-expr->typed
    scope-instrs->typed
    scope-instr->typed)
  (import
    (micascheme)
    (syntax scope)
    (syntax scoped)
    (prefix (micalog keywords) %))

  (define (literal->typed? $literal)
    (syntax-case $literal ()
      (0 #`(1 0))
      (1 #`(1 1))
      (id (identifier? #'id)
        (lets
          ($string (symbol->string (datum id)))
          ($length (string-length $string))
          ($prefix-template "xxx-")
          ($prefix-length (string-length $prefix-template))
          (and (> $length $prefix-length)
            (lets
              ($prefix (substring $string 0 $prefix-length))
              ($data (substring $string $prefix-length $length))
              ($data-length (string-length $data))
              (case $prefix
                (("bin-")
                  (opt-lets
                    ($number (string->number $data 2))
                    #`(
                      #,(literal->syntax $data-length)
                      #,(literal->syntax $number))))
                (("hex-")
                  (opt-lets
                    ($number (string->number $data 16))
                    #`(
                      #,(literal->syntax (* $data-length 4))
                      #,(literal->syntax $number))))
                (("oct-")
                  (opt-lets
                    ($number (string->number $data 8))
                    #`(
                      #,(literal->syntax (* $data-length 3))
                      #,(literal->syntax $number))))
                (else #f))))))
      (else #f)))

  (define (literal->typed $literal)
    (or
      (literal->typed? $literal)
      (syntax-error $literal "invalid literal")))

  (define (scope-id->typed $scope $id)
    #`(#,(scope-item $scope $id) #,$id))

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
          ($typed-b (scope-expr->typed $scope #'b))
          ($type-a (typed-type $typed-a))
          ($type-b (typed-type $typed-b))
          (if (type=? $type-a $type-b)
            #`(op
              #,$type-a
              #,(typed-value $typed-a)
              #,(typed-value $typed-b))
            (syntax-error $expr
              (format "type mismatch ~a, expected ~a in"
                (syntax->datum #`(op #,$type-a #,$type-b))
                (syntax->datum #`(op #,$type-a #,$type-a)))))))))

  (define (scope-op1/2->typed $scope $expr)
    (syntax-case $expr ()
      ((_ _) (scope-op1->typed $scope $expr))
      ((_ _ _) (scope-op2->typed $scope $expr))))

  (define (scope-if->typed $scope $if)
    (syntax-case $if (%if)
      ((%if a b c)
        (lets
          ($typed-a (scope-expr->typed $scope #'a))
          ($typed-b (scope-expr->typed $scope #'b))
          ($typed-c (scope-expr->typed $scope #'c))
          ($type-a (typed-type $typed-a))
          ($type-b (typed-type $typed-b))
          ($type-c (typed-type $typed-c))
          (if
            (and (type=? $type-a #`1) (type=? $type-b $type-c))
            #`(%if
              #,$type-b
              #,(typed-value $typed-a)
              #,(typed-value $typed-b)
              #,(typed-value $typed-c))
            (syntax-error $if
              (format "type mismatch ~a, expected ~a in"
                (syntax->datum #`(%if #,$type-a #,$type-b #,$type-c))
                (syntax->datum #`(%if 1 #,$type-b #,$type-b)))))))))

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
            (syntax-error $slice
              (format "type mismatch ~a, expected ~a in"
                (syntax->datum #`(%slice #,$type-a shift size))
                (syntax->datum #`(>= #,$type-a (+ shift size))))))))
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
            (syntax-error $slice
              (format "type mismatch ~a, expected ~a in"
                (syntax->datum #`(%slice #,$type-a size))
                (syntax->datum #`(>= #,$type-a size)))))))))

  (define (scoped-syntaxes+instr (scoped $scope $syntaxes) $instr)
    (syntax-case $instr (%wire %set)
      ((%wire id expr)
        (lets
          ($typed (scope-expr->typed $scope #'expr))
          ($type (typed-type $typed))
          (scoped
            (scope+ $scope #'id (typed-type $typed))
            (push $syntaxes #`(%wire #,$type id #,(typed-value $typed))))))
      ((%set id expr)
        (lets
          ($id-type (scope-item $scope #'id))
          ($typed (scope-expr->typed $scope #'expr))
          ($type (typed-type $typed))
          (if (type=? $id-type $type)
            (scoped $scope
              (push $syntaxes #`(%set #,$type id #,(typed-value $typed))))
            (syntax-error $instr
              (format "type mismatch ~a, expected ~a in"
                (syntax->datum #`(%set #,$id-type #,$type))
                (syntax->datum #`(%set #,$id-type #,$id-type)))))))))

  (define (scope-instrs->typed $scope $instrs)
    (fluent
      (fold-left
        scoped-syntaxes+instr
        (scoped $scope (stack))
        (syntax->list $instrs))
      (scoped-value)
      (reverse)
      (list->syntax)))

  (define (scope-instr->typed $scope $instr)
    (syntax-single (scope-instrs->typed $scope #`(#,$instr))))

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
