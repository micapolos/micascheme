(library (arm64)
  (export
    x x? x-index
    x0 x1 x2 x3 x4 x5 x6 x7
    x8 x9 x10 x11 x12 x13 x14 x15
    x16 x17 x18 x19 x20 x21 x22 x23
    x24 x25 x26 x27 x28 x29 x30

    w w? w-index
    w0 w1 w2 w3 w4 w5 w6 w7
    w8 w9 w10 w11 w12 w13 w14 w15
    w16 w17 w18 w19 w20 w21 w22 w23
    w24 w25 w26 w27 w28 w29 w30

    add
    sub
    ret)
  (import
    (scheme)
    (keyword)
    (syntax)
    (list)
    (procedure)
    (syntaxes)
    (fixnum))

  (define-rule-syntax (define-enum-index (id value ...))
    (define-syntax (id $syntax)
      (syntax-case $syntax ()
        ((_ x)
          (identifier? #'x)
          (literal->syntax
            (or
              (find-index
                (partial free-identifier=? #'x)
                (syntax->list #'(value ...)))
              (syntax-error #'x)))))))

  (define-rule-syntax (define-enum? (id value ...))
    (define-rule-syntax (id x)
      (and
        (keyword? x)
        (and
          (memp
            (partial free-identifier=? #'x)
            (syntax->list #'(value ...)))
          #t))))

  (define-rule-syntax (define-enum (id id? id-index) value ...)
    (begin
      (define-keywords id value ...)
      (define-enum? (id? value ...))
      (define-enum-index (id-index value ...))))

  (define-enum (x x? x-index)
    x0 x1 x2 x3 x4 x5 x6 x7
    x8 x9 x10 x11 x12 x13 x14 x15
    x16 x17 x18 x19 x20 x21 x22 x23
    x24 x25 x26 x27 x28 x29 x30)

  (define-enum (w w? w-index)
    w0 w1 w2 w3 w4 w5 w6 w7
    w8 w9 w10 w11 w12 w13 w14 w15
    w16 w17 w18 w19 w20 w21 w22 w23
    w24 w25 w26 w27 w28 w29 w30)

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
        (5 (x-index c))
        (6 0) ; shift
        (5 (x-index b))
        (5 (x-index a)))))

  (define-rules-syntax
    ((sub a b c)
      (and (x? a) (x? b) (x? c))
      (fxpack
        (1 1) ; 64-bit
        (1 1) ; sub
        (1 0) ; no sign
        (5 #b01011) ; data processing
        (2 0) ; shift amount
        (1 0) ; fixed bit
        (5 (x-index c))
        (6 0) ; shift
        (5 (x-index b))
        (5 (x-index a)))))

  (define-rules-syntax
    ((ret)
      (fxpack
        (11 #b11010110010) ; ret
        (5 #b11111) ; op2
        (6 #b000000) ; op3
        (5 #b11110) ; source register
        (5 #b00000)))) ; padding
)
