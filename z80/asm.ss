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

  (define-rule-syntax (define-asm-pattern-match?-syntax (id param ...) (pattern arg ...) ...)
    (define-pattern-match?-syntax id
      (syntax-rules ()
        ((_ expr (_ param ...) body)
          (opt-lets
            ($list
              (syntax-match? expr
                (pattern (list arg ...)) ...))
            (let-values (((param ...) (apply values $list)))
              body))))))

  (define-asm-pattern-match?-syntax (r prefix? r offset?)
    (b           #f #b000 #f)
    (c           #f #b001 #f)
    (d           #f #b010 #f)
    (e           #f #b011 #f)
    (h           #f #b100 #f)
    (l           #f #b101 #f)
    ((hl)        #f #b110 #f)
    (a           #f #b111 #f)
    (ixh         (db-8 #xdd) #b100 #f)
    (ixl         (db-8 #xdd) #b101 #f)
    (iyh         (db-8 #xfd) #b100 #f)
    (iyl         (db-8 #xfd) #b101 #f)
    ((+ ix #'d)  (db-8 #xdd) #b110 #`(db #,d))
    ((+ iy #'d)  (db-8 #xfd) #b110 #`(db #,d)))

  (define-asm-pattern-match?-syntax (math math)
    (add #b000)
    (adc #b001)
    (sub #b010)
    (sbc #b011))

  (define-asm-pattern-match?-syntax (logic logic)
    (and #b100)
    (or  #b101)
    (xor #b110)
    (cp  #b111))

  (define-asm-pattern-match?-syntax (incr incr)
    (inc #b100)
    (dec #b101))

  (define (op->asm? $op)
    (syntax-match? $op
      (((math m) a (r prefix? r offset?))
        (non-false-list
          prefix?
          (db-233 #b10 m r)
          offset?))
      (((logic l) (r prefix? r offset?))
        (non-false-list
          prefix?
          (db-233 #b10 l r)
          offset?))
      (((incr i) (r prefix? r offset?))
        (non-false-list
          prefix?
          (db-233 #b00 r i)
          offset?))
      ((nop)
        (list (db-8 0)))
      ((halt)
        (list (db-8 #b01110110)))
      ((ld (r prefix-1? r-1 offset-1?) (r prefix-2? r-2 offset-2?))
        (%and
          (%not (%and (= r-1 #b110) (= r-2 #b110)))
          (%or (%not prefix-1?) (%not prefix-2?) (syntax=? prefix-1? prefix-2?))
          (non-false-list
            (%or prefix-1? prefix-2?)
            (db-233 #b01 r-1 r-2)
            (%or offset-1? offset-2?))))
      ((ld (r prefix? r offset?) #'n)
        (non-false-list
          prefix?
          (db-233 #b00 r #b110)
          offset?
          #`(db #,n)))))

  (define (db-233 $a $b $c)
    #`(db
      #,(fxior
        (fxarithmetic-shift-left $a 6)
        (fxarithmetic-shift-left $b 3)
        $c)))

  (define (db-8 $a)
    #`(db #,$a))

  (define-rule-syntax (db-n n)
    #'(db n))
)
