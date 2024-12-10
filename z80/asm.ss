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
    (r           prefix?      r3     offset? )
    (b           #f           #b000  #f      )
    (c           #f           #b001  #f      )
    (d           #f           #b010  #f      )
    (e           #f           #b011  #f      )
    (h           #f           #b100  #f      )
    (l           #f           #b101  #f      )
    ((hl)        #f           #b110  #f      )
    (a           #f           #b111  #f      )
    (ixh         (db-8 #xdd)  #b100  #f      )
    (ixl         (db-8 #xdd)  #b101  #f      )
    (iyh         (db-8 #xfd)  #b100  #f      )
    (iyl         (db-8 #xfd)  #b101  #f      )
    ((+ ix #'d)  (db-8 #xdd)  #b110  (db-n d))
    ((+ iy #'d)  (db-8 #xfd)  #b110  (db-n d)))

  (define-pattern-match? (n id) (expr body)
    (lets (id #'(db id))
      body))

  (define-pattern-match? (nm id) (expr body)
    (lets (id #'(dw id))
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

  (define (db-16 $a)
    #`(dw #,$a))

  (define (db-n $n)
    #`(db #,$n))

  (define (op->asm? $op)
    (asm-syntax-match? $op
      (((math m) a (r prefix? r offset?)) #t
        prefix?
        (db-233 #b10 m r)
        offset?)
      (((math m) a (n n)) #t
        (db-233 #b11 m #b110)
        n)
      (((logic l) (r prefix? r offset?)) #t
        prefix?
        (db-233 #b10 l r)
        offset?)
      (((logic l) (n n)) #t
        (db-233 #b11 l #b110)
        n)
      (((incr i) (r prefix? r offset?)) #t
        prefix?
        (db-233 #b00 r i)
        offset?)
      ((nop) #t
        (db-8 0))
      ((halt) #t
        (db-8 #b01110110))
      ((ld a (bc)) #t
        (db-8 #b00001010))
      ((ld a (de)) #t
        (db-8 #b00011010))
      ((ld (bc) a) #t
        (db-8 #b00000010))
      ((ld (de) a) #t
        (db-8 #b00010010))
      ((ld (r prefix-1? r-1 offset-1?) (r prefix-2? r-2 offset-2?))
        (%and
          (%not (%and (= r-1 #b110) (= r-2 #b110)))
          (%or (%not prefix-1?) (%not prefix-2?) (syntax=? prefix-1? prefix-2?)))
        (%or prefix-1? prefix-2?)
        (db-233 #b01 r-1 r-2)
        (%or offset-1? offset-2?))
      ((ld a ((nm nm))) #t
        (db-8 #b00111010)
        nm)
      ((ld ((nm nm)) a) #t
        (db-8 #b00110010)
        nm)
      ((ld (r prefix? r offset?) (n n)) #t
        prefix?
        (db-233 #b00 r #b110)
        offset?
        n)))
)
