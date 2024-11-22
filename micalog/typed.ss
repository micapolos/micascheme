(library (micalog typed)
  (export
    literal->typed
    expr->typed
    scope-expr->typed)
  (import
    (micascheme)
    (micac scope)
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
    (or
      (scope-ref $scope $id)
      (syntax-error $id "not bound")))

  (define (scope-expr->typed $scope $expr)
    (or
      (literal->typed? $expr)
      (syntax-case $expr (%= %!= %< %<= %> %>= %not %and %or %xor %nand %not %xnor %+ %- %if)
        (id (identifier? #'id) (scope-id->typed $scope #'id))
        ((%= a b) (scope-op2->typed $scope $expr))
        ((%!= a b) (scope-op2->typed $scope $expr))
        ((%< a b) (scope-op2->typed $scope $expr))
        ((%<= a b) (scope-op2->typed $scope $expr))
        ((%> a b) (scope-op2->typed $scope $expr))
        ((%>= a b) (scope-op2->typed $scope $expr))
        ((%not a) (scope-op1->typed $scope $expr))
        ((%and a b) (scope-op2->typed $scope $expr))
        ((%or a b) (scope-op2->typed $scope $expr))
        ((%xor a b) (scope-op2->typed $scope $expr))
        ((%nand a b) (scope-op2->typed $scope $expr))
        ((%nor a b) (scope-op2->typed $scope $expr))
        ((%xnor a b) (scope-op2->typed $scope $expr))
        ((%- a) (scope-op1->typed $scope $expr))
        ((%+ a b) (scope-op2->typed $scope $expr))
        ((%- a b) (scope-op2->typed $scope $expr))
        ((%if a b c) (scope-if->typed $scope $expr)))))

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
              (format "type mismatch ~a in"
                (syntax->datum
                  #`(op #,$type-a #,$type-b)))))))))

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
              (format "type mismatch ~a in"
                (syntax->datum
                  #`(op #,$type-a #,$type-b #,$type-c)))))))))

  (define (typed $type $value)
    #`(#,$type #,$value))

  (define (typed-type $typed)
    (syntax-case $typed ()
      ((type value) #'type)))

  (define (typed-value $typed)
    (syntax-case $typed ()
      ((type value) #'value)))

  (define type=? syntax=?)
)
