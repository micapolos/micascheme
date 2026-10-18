(library (arm64)
  (export
    x x? x-index? x-index
    x0 x1 x2 x3 x4 x5 x6 x7
    x8 x9 x10 x11 x12 x13 x14 x15
    x16 x17 x18 x19 x20 x21 x22 x23
    x24 x25 x26 x27 x28 x29 x30 xzr

    w w? w-index? w-index
    w0 w1 w2 w3 w4 w5 w6 w7
    w8 w9 w10 w11 w12 w13 w14 w15
    w16 w17 w18 w19 w20 w21 w22 w23
    w24 w25 w26 w27 w28 w29 w30 wzr

    eq ne cs cc mi pl vs vc hi ls ge lt gt le al nv

    r?
    r-index

    b bl
    br blr

    sf

    pre+ post+

    ldr

    add adds
    sub subs
    ret

    put-arm64
    arm64-bytevector

    check-ops)
  (import
    (except (scheme) condition condition?)
    (keyword)
    (syntax)
    (list)
    (procedure)
    (syntaxes)
    (lets)
    (check)
    (fixnum))

  (define (op->string op)
    (lets
      (str (number->string (fxand op #xffffffff) 2))
      (len (string-length str))
      (str
        (if (< len 32)
          (string-append (make-string (- 32 len) #\0) str)
          str))
      (string-append
        (substring str 0 8) " "
        (substring str 8 16) " "
        (substring str 16 24) " "
        (substring str 24 32))))

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

  (define-keywords imm9)

  (define-enum (w w-index? w? w-index)
    w0 w1 w2 w3 w4 w5 w6 w7
    w8 w9 w10 w11 w12 w13 w14 w15
    w16 w17 w18 w19 w20 w21 w22 w23
    w24 w25 w26 w27 w28 w29 w30 wzr)

  (define-enum (x x-index? x? x-index)
    x0 x1 x2 x3 x4 x5 x6 x7
    x8 x9 x10 x11 x12 x13 x14 x15
    x16 x17 x18 x19 x20 x21 x22 x23
    x24 x25 x26 x27 x28 x29 x30 xzr)

  (define-rule-syntax (r? r)
    (or (w? r) (x? r)))

  (define-case-syntax (r-index r)
    (literal->syntax
      (or
        (w-index? r)
        (x-index? r)
        (syntax-error #'r))))

  ; TODO: allow any number of rs
  (define-rules-syntax
    ((sf rd)
      (w? rd)
      0)
    ((sf rd)
      (x? rd)
      1)
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

  (define-enum (size size-index? size? size-index)
    size-8 size-16 size-32 size-64)

  (define-keywords pre+ post+)

  (define-rules-syntax
    (keywords + pre+ post+)
    ((ldr rt (+ rn rm))
      (r? rm)
      (fxpack
        (1 1)
        (1 (sf rt))
        (9 #b111000011)
        (5 (r-index rm))
        (6 #b010010)
        (5 (r-index rn))
        (5 (r-index rt))))
    ((ldr rt (rn))
      (ldr rt (+ rn 0)))
    ((ldr rt (+ rn offset))
      (fxpack
        (1 1)
        (1 (sf rt))
        (8 #b11100101)
        (12 offset)
        (5 (r-index rn))
        (5 (r-index rt))))
    ((ldr rt (post+ rn offset))
      (fxpack
        (1 1)
        (1 (sf rt))
        (9 #b111000010)
        (9 offset)
        (2 #b01)
        (5 (r-index rn))
        (5 (r-index rt))))
    ((ldr rt (pre+ rn offset))
      (fxpack
        (1 1)
        (1 (sf rt))
        (9 #b111000010)
        (9 offset)
        (2 #b11)
        (5 (r-index rn))
        (5 (r-index rt)))))

  (define-rules-syntax
    ((alu op s rd rm rn)
      (r? rn)
      (alu op s rd rm rn (lsl 0)))
    ((alu op s rd rm rn (shift imm6))
      (r? rn)
      (fxpack
        (1 (sf rd rm rn))
        (1 (alu-op-index op))
        (1 (alu-s-index s))
        (5 #b01011)
        (2 (shift-index shift))
        (1 0) ; fixed bit
        (5 (r-index rn))
        (6 imm6)
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

  (define-enum (condition condition-index? condition? condition-index)
    eq ne cs cc mi pl vs vc hi ls ge lt gt le al nv)

  (define-rules-syntaxes
    ((b offset)
      (fxpack
        (6 #b000101)
        (26 offset)))
    ((bl offset)
      (fxpack
        (6 #b100101)
        (26 offset)))
    ((b condition offset)
      (fxpack
        (8 #b01010100)
        (19 offset)
        (1 0) ; consistency
        (4 (condition-index condition)))))

  (define-rules-syntaxes
    ((br xn)
      (fxpack
        (22 #b1101011000011111000000)
        (5 (x-index xn))
        (5 #b00000)))
    ((blr xn)
      (fxpack
        (22 #b1101011000111111000000)
        (5 (x-index xn))
        (5 #b00000))))

  (define-rules-syntax
    ((ret)
      (ret x30))
    ((ret xn)
      (fxpack
        (11 #b11010110010) ; ret
        (5 #b11111) ; op2
        (6 #b000000) ; op3
        (5 (x-index xn))
        (5 #b00000)))) ; padding

  (define-rule-syntax (put-arm64 $port op ...)
    (begin (put-u32 $port op 'little) ...))

  (define-rule-syntax (arm64-bytevector op ...)
    (with-bytevector-output-port $port
      (put-arm64 $port op ...)))

  (define-rule-syntax (check-ops (op str) ...)
    (begin
      (check
        (equal?
          (op->string op)
          str))
      ...))
)
