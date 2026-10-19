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

    lsl lsr asr ror

    r?
    r-index

    b bl
    br blr

    sf

    pre+ post+

    ldr str
    ldp stp

    add adds
    sub subs

    and eor orr ands
    mov

    ret

    foreign-set-arm64!
    put-arm64
    arm64-bytevector
    check-ops
    check-exec)
  (import
    (rename
      (except (scheme) condition condition?)
      (and %and))
    (keyword)
    (syntax)
    (list)
    (procedure)
    (syntaxes)
    (lets)
    (check)
    (jit)
    (foreign)
    (put)
    (fixnum)
    (arm64 encode))

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
          #'(%and
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
      (define-rule-syntax (id? x) (%and (id-index? x) #t))
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

  (define-rules-syntax
    ((zr r) (w? r) wzr)
    ((zr r) (x? r) xzr))

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
      (%and (w? rd) (w? rm))
      0)
    ((sf rd rm)
      (%and (x? rd) (x? rm))
      1)
    ((sf rd rm rn)
      (%and (w? rd) (w? rm) (w? rn))
      0)
    ((sf rd rm rn)
      (%and (x? rd) (x? rm) (x? rn))
      1))

  (define-enum-index (arithmetic-op-index add sub))

  (define-enum (shift shift-index? shift? shift-index)
    lsl lsr asr ror)

  (define-keywords unsigned signed)
  (define-enum-index (arithmetic-s-index unsigned signed))

  (define-enum (size size-index? size? size-index)
    size-8 size-16 size-32 size-64)

  (define-keywords pre+ post+)

  (define-rules-syntax
    (keywords + post+ pre+)
    ((ldr rt (+ rn rm))
      (r? rm)
      (load/store-register
        (sf rt)
        #b111000011
        (r-index rm)
        #b010010
        (r-index rn)
        (r-index rt)))
    ((ldr rt (rn))
      (ldr rt (+ rn 0)))
    ((ldr rt (+ rn offset))
      (load/store-immediate
        (sf rt)
        #b11100101
        offset
        (r-index rn)
        (r-index rt)))
    ((ldr rt (post+ rn offset))
      (load/store-immediate++
        (sf rt)
        #b111000010
        offset
        #b0
        (r-index rn)
        (r-index rt)))
    ((ldr rt (pre+ rn offset))
      (load/store-immediate++
        (sf rt)
        #b111000010
        offset
        #b1
        (r-index rn)
        (r-index rt))))

  (define-rules-syntax
    (keywords + post+ pre+)
    ((str rt (+ rn rm))
      (r? rm)
      (load/store-register
        (sf rt)
        #b111000001
        (r-index rm)
        #b010010
        (r-index rn)
        (r-index rt)))
    ((str rt (rn))
      (str rt (+ rn 0)))
    ((str rt (+ rn offset))
      (load/store-immediate
        (sf rt)
        #b11100100
        offset
        (r-index rn)
        (r-index rt)))
    ((str rt (post+ rn offset))
      (load/store-immediate++
        (sf rt)
        #b111000000
        offset
        #b0
        (r-index rn)
        (r-index rt)))
    ((str rt (pre+ rn offset))
      (load/store-immediate++
        (sf rt)
        #b111000000
        offset
        #b1
        (r-index rn)
        (r-index rt))))

  (define-rules-syntax
    (keywords + post+ pre+)
    ((ldp rt rt2 (rn))
      (ldp rt rt2 (+ rn 0)))
    ((ldp rt rt2 (+ rn offset))
      (load/store-pair
        (sf rt rt2)
        #b10100101
        offset
        (r-index rt2)
        (x-index rn)
        (r-index rt)))
    ((ldp rt rt2 (post+ rn offset))
      (load/store-pair
        (sf rt rt2)
        #b10100011
        offset
        (r-index rt2)
        (x-index rn)
        (r-index rt)))
    ((ldp rt rt2 (pre+ rn offset))
      (load/store-pair
        (sf rt rt2)
        #b10100111
        offset
        (r-index rt2)
        (x-index rn)
        (r-index rt))))

  (define-rules-syntax
    (keywords + post+ pre+)
    ((stp rt rt2 (rn))
      (stp rt rt2 (+ rn 0)))
    ((stp rt rt2 (+ rn offset))
      (load/store-pair
        (sf rt rt2)
        #b10100100
        offset
        (r-index rt2)
        (x-index rn)
        (r-index rt)))
    ((stp rt rt2 (post+ rn offset))
      (load/store-pair
        (sf rt rt2)
        #b10100010
        offset
        (r-index rt2)
        (x-index rn)
        (r-index rt)))
    ((stp rt rt2 (pre+ rn offset))
      (load/store-pair
        (sf rt rt2)
        #b10100110
        offset
        (r-index rt2)
        (x-index rn)
        (r-index rt))))

  (define-rules-syntax
    ((arithmetic op s rd rm rn)
      (r? rn)
      (arithmetic op s rd rm rn (lsl 0)))
    ((arithmetic op s rd rm rn (shift imm6))
      (r? rn)
      (fxpack
        (1 (sf rd rm rn))
        (1 (arithmetic-op-index op))
        (1 (arithmetic-s-index s))
        (5 #b01011)
        (2 (shift-index shift))
        (1 0) ; fixed bit
        (5 (r-index rn))
        (6 imm6)
        (5 (r-index rm))
        (5 (r-index rd))))
    ((arithmetic op s rd rm imm12)
      (fxpack
        (1 (sf rd rm))
        (1 (arithmetic-op-index op))
        (1 (arithmetic-s-index s))
        (6 #b100010)
        (1 0) ; shift
        (12 imm12)
        (5 (r-index rm))
        (5 (r-index rd)))))

  (define-rules-syntaxes
    ((add . x)
      (arithmetic add unsigned . x))
    ((adds . x)
      (arithmetic add signed . x))
    ((sub . x)
      (arithmetic sub unsigned . x))
    ((subs . x)
      (arithmetic sub signed . x)))

  (define-enum-index (logical-op-index and orr eor ands))

  (define-rules-syntax
    ((logical op rd rm rn)
      (r? rn)
      (logical op rd rm rn (lsl 0)))
    ((logical op rd rm rn (shift imm6))
      (r? rn)
      (fxpack
        (1 (sf rd rm rn))
        (2 (logical-op-index op))
        (5 #b01010)
        (2 (shift-index shift))
        (1 0) ; N
        (5 (r-index rn))
        (6 imm6)
        (5 (r-index rm))
        (5 (r-index rd)))))

  (define-rules-syntaxes
    ((and . x)
      (logical and . x))
    ((eor . x)
      (logical eor . x))
    ((orr . x)
      (logical orr . x))
    ((ands . x)
      (logical ands . x)))

  (define-rules-syntaxes
    ((mov rd rm)
      (fxpack
        (1 (sf rd rm))
        (2 #b01)
        (5 #b01010)
        (2 #b00)
        (1 0) ; N
        (5 (r-index rm))
        (6 #b000000)
        (5 #b11111)
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

  (define-rule-syntax (foreign-set-arm64! address op ...)
    (lets
      ($address address)
      ($offset 0)
      (run
        (begin
          (foreign-set-u32! $address $offset op)
          (set! $offset (+ $offset 4))) ...)))

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

  (define-rule-syntax
    (check-exec (mem-var data ...)
      (block
        (params (param-t param) ...)
        op ...
        (result (ret-t res)))
      ...)
    (when
      (equal? (machine-type) 'tarm64osx)
      (lets
        ($jit (with-jit 1024))
        (run
          (lets
            ($bytevector (locked-object (bytevector! data ...)))
            (mem-var (object->reference-address $bytevector))
            (run
              (jit-edit $jit)
              (foreign-set-arm64! (jit-address $jit) op ... (ret))
              (jit-flush $jit)
              (jit-exec $jit))
            ($proc (foreign-procedure (jit-address $jit) (param-t ...) ret-t))
            (check (equal? ($proc param ...) res)))
          ...))))
)
