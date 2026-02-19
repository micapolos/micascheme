(library (leo2 stdlib)
  (export
    native-type
    boolean-type
    fixnum-type
    number-type
    char-type
    string-type

    native-value
    boolean-value
    fixnum-type
    number-value
    char-value
    string-value)

  (import
    (leo2 base)
    (leo2 term))

  (define (native-type $symbol)
    (typed (type 0) (native $symbol)))

  (define (native-value $type $value)
    (typed $type (native $value)))

  (define boolean-type
    (native-type 'boolean))

  (define fixnum-type
    (native-type 'fixnum))

  (define number-type
    (native-type 'number))

  (define char-type
    (native-type 'char))

  (define string-type
    (native-type 'string))

  (define (boolean-value $boolean)
    (native-value boolean-type $boolean))

  (define (number-value $number)
    (native-value number-type $number))

  (define (fixnum-value $fixnum)
    (native-value fixnum-type $fixnum))

  (define (char-value $char)
    (native-value char-type $char))

  (define (string-value $string)
    (native-value string-type $string))
)
