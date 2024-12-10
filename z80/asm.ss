(library (z80 asm)
  (export
    op->asm? r)
  (import
    (rename (micascheme)
      (and %and)
      (or %or)
      (xor %xor)
      (not %not)
      (+ %+))
    (z80 keywords))

  (define-rule-syntax (asm-syntax-match? expr (pattern fender item ...) ...)
    (syntax-match? expr
      (pattern (%and fender (non-false-list item ...))) ...))

  (define-rule-syntax (define-asm-pattern-match? (id param ...) (pattern arg ...) ...)
    (define-pattern-match? (id param ...) (expr body)
      (opt-lets
        ($list
          (syntax-match? expr
            (pattern (list arg ...)) ...))
        (let-values (((param ...) (apply values $list)))
          body))))

  (define-asm-pattern-match?
    (r           prefix?      r3     offset?     n?)
    (b           #f           #b000  #f          #f)
    (c           #f           #b001  #f          #f)
    (d           #f           #b010  #f          #f)
    (e           #f           #b011  #f          #f)
    (h           #f           #b100  #f          #f)
    (l           #f           #b101  #f          #f)
    ((hl)        #f           #b110  #f          #f)
    (a           #f           #b111  #f          #f)
    (ixh         (db-8 #xdd)  #b100  #f          #f)
    (ixl         (db-8 #xdd)  #b101  #f          #f)
    (iyh         (db-8 #xfd)  #b100  #f          #f)
    (iyl         (db-8 #xfd)  #b101  #f          #f)
    ((+ ix #'d)  (db-8 #xdd)  #b110  (db-n d)    #f)
    ((+ iy #'d)  (db-8 #xfd)  #b110  (db-n d)    #f)
    (#'n         #f           #b110  #f          (db-n n)))

  (define-pattern-match? (n id) (expr body)
    (lets (id #'(db id))
      body))

  (define-asm-pattern-match? (math math)
    (add #b000)
    (adc #b001)
    (sub #b010)
    (sbc #b011))

  (define-asm-pattern-match? (logic logic)
    (and #b100)
    (or  #b101)
    (xor #b110)
    (cp  #b111))

  (define-asm-pattern-match? (incr incr)
    (inc #b100)
    (dec #b101))

  (define (db-233 $a $b $c)
    #`(db
      #,(fxior
        (fxarithmetic-shift-left $a 6)
        (fxarithmetic-shift-left $b 3)
        $c)))

  (define (db-1133 $a $b $c $d)
    #`(db
      #,(fxior
        (fxarithmetic-shift-left $a 7)
        (fxarithmetic-shift-left $b 6)
        (fxarithmetic-shift-left $c 3)
        $d)))

  (define (db-8 $a)
    #`(db #,$a))

  (define (db-n $n)
    #`(db #,$n))

  (define (op->asm? $op)
    (asm-syntax-match? $op
      (((math m) a (r prefix? r offset? n?)) #t
        prefix?
        (db-1133 1 (if n? 1 0) m r)
        offset?
        n?)
      (((logic l) (r prefix? r offset? n?)) #t
        prefix?
        (db-1133 1 (if n? 1 0) l r)
        offset?
        n?)
      (((incr i) (r prefix? r offset? n?)) (%not n?)
        prefix?
        (db-233 #b00 r i)
        offset?)
      ((nop) #t
        (db-8 0))
      ((halt) #t
        (db-8 #b01110110))
      ((ld (r prefix-1? r-1 offset-1? n-1?) (r prefix-2? r-2 offset-2? n-2?))
        (%and
          (%not (%and (%not n-2?) (= r-1 #b110) (= r-2 #b110)))
          (%not n-1?)
          (%or (%not prefix-1?) (%not prefix-2?) (syntax=? prefix-1? prefix-2?)))
        (%or prefix-1? prefix-2?)
        (db-1133 0 (if n-2? 0 1) r-1 r-2)
        (%or offset-1? offset-2?)
        n-2?)))
)
