(library (micalog typed)
  (export
    literal->typed
    expr->typed)
  (import
    (micascheme)
    (micac scope)
    (prefix (micalog keywords) %))

  (define (literal->typed? $literal)
    (syntax-case $literal ()
      (0 #`(1 0))
      (1 #`(1 1))
      (id (identifier? #'id)
        (or
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
                    #`(
                      #,(literal->syntax $data-length)
                      #,(literal->syntax (string->number $data 2))))
                  (("hex-")
                    #`(
                      #,(literal->syntax (* $data-length 4))
                      #,(literal->syntax (string->number $data 16))))
                  (("oct-")
                    #`(
                      #,(literal->syntax (* $data-length 3))
                      #,(literal->syntax (string->number $data 8))))
                  (else #f)))))
            (syntax-error $literal "invalid literal")))))

  (define (literal->typed $literal)
    (or
      (literal->typed? $literal)
      (syntax-error $literal)))

  (define (scope-expr->typed $scope $expr)
    (syntax-case $expr ()
      ((%= a b) (scope-op2->typed $scope $expr))
      ((%!= a b) (scope-op2->typed $scope $expr))
      ((%< a b) (scope-op2->typed $scope $expr))
      ((%<= a b) (scope-op2->typed $scope $expr))
      ((%> a b) (scope-op2->typed $scope $expr))
      ((%>= a b) (scope-op2->typed $scope $expr))
      (literal (literal->typed #'literal))))

  (define expr->typed
    (partial scope-expr->typed (empty-scope)))

  (define (scope-op2->typed $scope $expr)
    (syntax-case $expr ()
      ((op a b)
        (lets
          ($typed-a (scope-expr->typed $scope #'a))
          ($typed-b (scope-expr->typed $scope #'b))
          (if (type=? (typed-type $typed-a) (typed-type $typed-b))
            #`(op
              #,(typed-type $typed-a)
              #,(typed-value $typed-a)
              #,(typed-value $typed-b))
            (syntax-error $expr "invalid types"))))))

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
