(library (z80-parser)
  (export
    byte byte? byte-integer
    word word? word-integer
    indirect indirect? indirect-argument
    offset offset? offset-argument offset-integer
    byte-register byte-register byte-register-symbol
    word-register word-register word-register-symbol
    label label? label-symbol

    syntax->argument
    syntax->bytevector)
  (import (micascheme))

  (data (context bytes address label-addresses address-labels))

  (data (byte integer))
  (data (word integer))
  (data (indirect argument))
  (data (offset argument integer))
  (data (byte-register symbol))
  (data (word-register symbol))
  (data (label symbol))

  (define (syntax->argument $syntax)
    (syntax-case $syntax ()
      ($item
        (switch (syntax->datum #`$item)
          ((symbol? $symbol)
            (case $symbol
              ((A) (byte-register `A))
              ((B) (byte-register `B))
              ((C) (byte-register `C))
              ((D) (byte-register `D))
              ((E) (byte-register `E))
              ((H) (byte-register `H))
              ((L) (byte-register `L))
              ((BC) (word-register `BC))
              ((DE) (word-register `DE))
              ((HL) (word-register `HL))
              ((PC) (word-register `PC))
              ((SP) (word-register `SP))
              (else
                (lets
                  ($string (symbol->string $symbol))
                  (or
                    (string->hex-byte-opt $string)
                    (string->hex-word-opt $string)
                    (string->label-opt $string)
                    (syntax-error $syntax "invalid"))))))
          ((number? $number)
            (cond
              ((not (integer? $number)) (syntax-error $syntax "not integer"))
              ((>= $number 256) (syntax-error $syntax "not byte"))
              ((>= $number 0) (byte $number))
              ((>= $number -128) (byte (+ $number 256)))
              (else (syntax-error $syntax "not byte"))))
          ((else $other) (syntax-error $syntax "invalid"))))
      (($item)
        (indirect (syntax->argument #`$item)))))

  (define (char->hex-number-opt $char)
    (case $char
      ((#\0) 0)
      ((#\1) 1)
      ((#\2) 2)
      ((#\3) 3)
      ((#\4) 4)
      ((#\5) 5)
      ((#\6) 6)
      ((#\7) 7)
      ((#\8) 8)
      ((#\9) 9)
      ((#\A) 10)
      ((#\B) 11)
      ((#\C) 12)
      ((#\D) 13)
      ((#\E) 14)
      ((#\F) 15)))

  (define (string->hex-byte-opt $string)
    (and
      (= (string-length $string) 3)
      (char=? (string-ref $string 2) #\h)
      (opt-lift byte
        (opt-lift +
          (opt-lift bitwise-arithmetic-shift-left (char->hex-number-opt (string-ref $string 0)) 4)
          (char->hex-number-opt (string-ref $string 1))))))

  (define (string->hex-word-opt $string)
    (and
      (= (string-length $string) 5)
      (char=? (string-ref $string 4) #\h)
      (opt-lift word
        (opt-lift +
          (opt-lift bitwise-arithmetic-shift-left (char->hex-number-opt (string-ref $string 0)) 12)
          (opt-lift bitwise-arithmetic-shift-left (char->hex-number-opt (string-ref $string 1)) 8)
          (opt-lift bitwise-arithmetic-shift-left (char->hex-number-opt (string-ref $string 2)) 4)
          (char->hex-number-opt (string-ref $string 3))))))

  (define (string->label-opt $string)
    (and
      (> (string-length $string) 1)
      (char=? (string-ref $string 0) #\$)
      (label (string->symbol (substring $string 1 (string-length $string))))))

  (define (empty-context)
    (context (stack) 0 (stack) (stack)))

  (define (context+byte $context $byte)
    (context
      (push (context-bytes $context) $byte)
      (+ (context-address $context) 1)
      (context-label-addresses $context)
      (context-address-labels $context)))

  (define (context+word $context $word)
    (context+bytes $context
      (bitwise-and $word #xFF)
      (bitwise-arithmetic-shift-right $word 8)))

  (define (context+label $context $label)
    (context
      (context-bytes $context)
      (context-address $context)
      (push
        (context-label-addresses $context)
        (cons $label (context-address $context)))
      (context-address-labels $context)))

  (define (context+label-ref $context $label)
    (context
      (cons 0 (cons 0 (context-bytes $context)))
      (+ (context-address $context) 2)
      (context-label-addresses $context)
      (push
        (context-address-labels $context)
        (cons (context-address $context) $label))))

  (define (context->bytevector $context)
    (lets
      ($bytevector (make-bytevector (context-address $context)))
      (run
        (for-each
          (lambda ($index $byte)
            (bytevector-u8-set! $bytevector $index $byte))
          (indices (context-address $context))
          (reverse (context-bytes $context))))
      ($label-addresses (context-label-addresses $context))
      (run
        (for-each
          (lambda ($address-label)
            (lets
              ($address (car $address-label))
              ($label (cdr $address-label))
              ($value (cdr (assq $label $label-addresses)))
              (run
                (bytevector-u8-set! $bytevector $address (bitwise-and $value #xFF)))
                (bytevector-u8-set! $bytevector (+ $address 1) (bitwise-arithmetic-shift-right $value 8))))
          (context-address-labels $context)))
      (bytevector->immutable-bytevector $bytevector)))

  (define-syntax-rule (context+bytes $context $byte ...)
    (fold-left context+byte $context (list $byte ...)))

  (define (syntax->bytevector $syntax)
    (context->bytevector
      (fold-left
        context+syntax
        (empty-context)
        (syntax->list $syntax))))

  (define (context+syntax $context $syntax)
    (syntax-case $syntax ()
      (label
        (identifier? #`label)
        (context+label $context (syntax->datum #`label)))
      ((data n ...)
        (identifier-named? #`data data)
        (fold-left
          context+byte
          $context
          (map syntax->datum (syntax->list #`(n ...)))))
      ((add a n)
        (and
          (identifier-named? #`add add)
          (identifier-named? #`a a)
          (number? (syntax->datum #`n)))
        (context+bytes $context #xC6 (syntax->datum #`n)))
      ((call nn)
        (and
          (identifier-named? #`call call)
          (or (identifier? #`nn) (number? (syntax->datum #`nn))))
        (lets
          ($context (context+byte $context #xCD))
          (switch (syntax->datum #`nn)
            ((symbol? $symbol) (context+label-ref $context $symbol))
            ((number? $number) (context+word $context $number)))))
      ((ld hl (nn))
        (and
          (identifier-named? #`ld ld)
          (identifier-named? #`hl hl)
          (or (identifier? #`nn) (number? (syntax->datum #`nn))))
        (lets
          ($context (context+byte $context #x2A))
          (switch (syntax->datum #`nn)
            ((symbol? $symbol) (context+label-ref $context $symbol))
            ((number? $number) (context+word $context $number)))))
      ((ret)
        (identifier-named? #`ret ret)
        (context+byte $context #xC9))
      (else (syntax-error $syntax "dupa"))))

)
