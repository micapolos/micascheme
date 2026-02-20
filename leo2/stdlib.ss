(library (leo2 stdlib)
  (export
    boolean-type
    fixnum-type
    number-type
    char-type
    string-type

    type-term
    boolean-term
    fixnum-term
    number-term
    char-term
    string-term

    native-type
    native-term
    variable-term
    native-application-term
    abstraction-type-term
    abstraction-term
    application-term
    branch-term
    branch-type-term
    error-term

    symbolic-type-term
    symbolic-term

    unit-type-term
    unit-term)

  (import
    (leo2 base)
    (leo2 term))

  (define (type-term $depth)
    (type $depth))

  (define (native-type $symbol)
    (typed (type 0) (native $symbol)))

  (define boolean-type
    (native-type 'a-boolean))

  (define fixnum-type
    (native-type 'a-fixnum))

  (define number-type
    (native-type 'a-number))

  (define char-type
    (native-type 'a-char))

  (define string-type
    (native-type 'a-string))

  (define (native-term $type $value)
    (typed $type (native $value)))

  (define (boolean-term $boolean)
    (native-term boolean-type $boolean))

  (define (number-term $number)
    (native-term number-type $number))

  (define (fixnum-term $fixnum)
    (native-term fixnum-type $fixnum))

  (define (char-term $char)
    (native-term char-type $char))

  (define (string-term $string)
    (native-term string-type $string))

  (define (variable-term $type $symbol)
    (typed $type (variable $symbol)))

  (define (native-application-term $type $procedure . $args)
    (typed $type
      (native-application $procedure $args)))

  (define (abstraction-type-term $param $procedure)
    (typed
      (type 0)
      (abstraction-type $param $procedure)))

  (define (abstraction-term $param $procedure)
    (typed
      (abstraction-type-term $param
        (lambda ($x)
          (type-of ($procedure $x))))
      (abstraction $procedure)))

  (define (application-term $lhs $rhs)
    (typed
      (abstraction-type-apply (typed-ref (type-of $lhs)) $rhs)
      (application $lhs $rhs)))

  (define (branch-type-term $cond $cons $alt)
    (typed (type 0)
      (branch $cond $cons $alt)))

  (define (branch-term $cond $cons $alt)
    (typed
      (branch-type-term $cond (type-of $cons) (type-of $alt))
      (branch $cond $cons $alt)))

  (define (symbolic-type-term $symbol $term)
    (typed (type 0)
      (symbolic $symbol $term)))

  (define (symbolic-term $symbol $term)
    (typed
      (symbolic-type-term $symbol (type-of $term))
      (symbolic $symbol $term)))

  (define unit-type-term (typed (type 0) unit))

  (define unit-term
    (typed unit-type-term unit))

  (define (error-term $type)
    (typed $type 'error))
)
