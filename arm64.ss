(library (arm64)
  (export
    x x? x-index? x-index
    x0 x1 x2 x3 x4 x5 x6 x7
    x8 x9 x10 x11 x12 x13 x14 x15
    x16 x17 x18 x19 x20 x21 x22 x23
    x24 x25 x26 x27 x28 x29 x30

    w w? w-index? w-index
    w0 w1 w2 w3 w4 w5 w6 w7
    w8 w9 w10 w11 w12 w13 w14 w15
    w16 w17 w18 w19 w20 w21 w22 w23
    w24 w25 w26 w27 w28 w29 w30

    r?
    r-index

    sf

    add adds
    sub subs
    ret)
  (import
    (scheme)
    (keyword)
    (syntax)
    (list)
    (procedure)
    (syntaxes)
    (fixnum))

  (define-rule-syntax (define-enum-index? (id value ...))
    (define-syntax (id $syntax)
      (syntax-case $syntax ()
        ((_ x)
          #'(and
            (identifier? #'x)
            (find-index
              (partial free-identifier=? #'x)
              (syntax->list #'(value ...))))))))

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

  (define-rule-syntax (define-enum (id id-index? id? id-index) value ...)
    (begin
      (define-keywords id value ...)
      (define-enum-index? (id-index? value ...))
      (define-rule-syntax (id? x)
        (and (id-index? x) #t))
      (define-enum-index (id-index value ...))))

  (define-enum (w w-index? w? w-index)
    w0 w1 w2 w3 w4 w5 w6 w7
    w8 w9 w10 w11 w12 w13 w14 w15
    w16 w17 w18 w19 w20 w21 w22 w23
    w24 w25 w26 w27 w28 w29 w30)

  (define-enum (x x-index? x? x-index)
    x0 x1 x2 x3 x4 x5 x6 x7
    x8 x9 x10 x11 x12 x13 x14 x15
    x16 x17 x18 x19 x20 x21 x22 x23
    x24 x25 x26 x27 x28 x29 x30)

  (define-rule-syntax (r? r)
    (or (w? r) (x? r)))

  (define-case-syntax (r-index r)
    (literal->syntax
      (or
        (w-index? r)
        (x-index? r)
        (syntax-error #'r))))

  (define-rules-syntax
    ((sf rd rm)
      (and (w? rd) (w? rm))
      0)
    ((sf rd rm)
      (and (x? rd) (x? rm))
      1)
    ((sf rd rm rn)
      (and (w? rd) (w? rm) (w? rn))
      0)
    ((sf rd rm rn)
      (and (x? rd) (x? rm) (x? rn))
      1))

  (define-rules-syntaxes
    ((add . x)
      (alu add unsigned . x))
    ((adds . x)
      (alu add signed . x))
    ((sub . x)
      (alu sub unsigned . x))
    ((subs . x)
      (alu sub signed . x)))

  (define-enum-index (alu-op-index add sub))

  (define-enum (shift shift-index? shift? shift-index)
    lsl lsr asr ror)

  (define-keywords unsigned signed)
  (define-enum-index (alu-s-index unsigned signed))

  (define-rules-syntax
    ((alu op s rd rm rn)
      (r? rn)
      (fxpack
        (1 (sf rd rm rn))
        (1 (alu-op-index op))
        (1 (alu-s-index s))
        (5 #b01011)
        (2 0) ; shift
        (1 0) ; fixed bit
        (5 (r-index rn))
        (6 0) ; shift
        (5 (r-index rm))
        (5 (r-index rd))))
    ((alu op s rd rm imm12)
      (fxpack
        (1 (sf rd rm))
        (1 (alu-op-index op))
        (1 (alu-s-index s))
        (6 #b100010)
        (1 0) ; shift
        (12 imm12)
        (5 (r-index rm))
        (5 (r-index rd)))))

  (define-rules-syntax
    ((ret)
      (fxpack
        (11 #b11010110010) ; ret
        (5 #b11111) ; op2
        (6 #b000000) ; op3
        (5 #b11110) ; source register
        (5 #b00000)))) ; padding
)
