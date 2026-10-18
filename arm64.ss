(library (arm64)
  (export
    x0 x1 x2 x3 x29 x30
    x?
    x-code?
    x-code
    add
    ret)
  (import
    (scheme)
    (keyword)
    (syntax)
    (syntaxes)
    (fixnum))

  (define-keywords x0 x1 x2 x3 x29 x30)

  (define-rule-syntax (x? x)
    (and (x-code? x) #t))

  (define-rule-syntax (x-code? x)
    (syntax-case #'x (x0 x1 x2 x3 x29 x30)
      (x0 0)
      (x1 1)
      (x2 2)
      (x3 3)
      (x29 29)
      (x30 30)
      (else #f)))

  (define-rule-syntax (x-code x)
    (or
      (x-code? x)
      (syntax-error #'x "invalid x reg")))

  (define-rules-syntax
    ((add a b c)
      (and (x? a) (x? b) (x? c))
      (fxpack
        (1 1) ; 64-bit
        (1 0) ; add
        (1 0) ; no sign
        (5 #b01011) ; data processing
        (2 0) ; shift amount
        (1 0) ; fixed bit
        (5 (x-code c))
        (6 0) ; shift
        (5 (x-code b))
        (5 (x-code a)))))

  (define-rules-syntax
    ((ret)
      (fxpack
        (11 #b11010110010) ; ret
        (5 #b11111) ; op2
        (6 #b000000) ; op3
        (5 #b11110) ; source register
        (5 #b00000)))) ; padding
)
