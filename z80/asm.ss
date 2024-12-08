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

  (define-rule-syntax (asm-syntax-case? stx literals (pattern fender body ... ) ...)
    (syntax-case? stx literals (pattern fender (filter-opts (list body ...))) ...))

  (define-syntax-match-clause r
    (lambda ($syntax)
      (syntax-case $syntax ()
        (((_ prefix? r3 offset?) body)
          (with-syntax ((tmp (generate-identifier #'r)))
            (with-syntax
              ((match?
                #`(syntax-match? #'tmp
                  (b           (list #f #b000 #f))
                  (c           (list #f #b001 #f))
                  (d           (list #f #b010 #f))
                  (e           (list #f #b011 #f))
                  (h           (list #f #b100 #f))
                  (l           (list #f #b101 #f))
                  ((hl)        (list #f #b110 #f))
                  (a           (list #f #b111 #f))
                  (ixh         (list #xdd #b100 #f))
                  (ixl         (list #xdd #b101 #f))
                  (iyh         (list #xfd #b100 #f))
                  (iyl         (list #xfd #b101 #f))
                  ((+ ix #'d)  (list #xdd #b110 d))
                  ((+ iy #'d)  (list #xfd #b110 d)))))
                #`(tmp match?
                  (let-values
                    (((prefix? r3 offset?) (apply values match?)))
                    body))))))))

  (define (op->asm? $op)
    (asm-syntax-case? $op (a nop ld hl)
      ((add a r) (%and (add-3? add) (r-3? r))
        (db-prefix? r)
        (db-233 #b10 (add-3? add) (r-3? r))
        (db-offset? r))
      ((and r) (%and (and-3? and) (r-3? r))
        (db-prefix? r)
        (db-233 #b10 (and-3? and) (r-3? r))
        (db-offset? r))
      ((inc r) (%and (inc-3? inc) (r-3? r))
        (db-prefix? r)
        (db-233 #b00 (r-3? r) (inc-3? inc))
        (db-offset? r))
      ((nop) #t
        (db-8 0))
      ((halt) #t
        (db-8 #b01110110))
      ((ld r1 r2)
        (%and
          (r-3? r1)
          (r-3? r2)
          (%not (%and (= (r-3? r1) #b110) (= (r-3? r2) #b110)))
          (%or (%not (db-prefix? r1)) (%not (db-prefix? r2)) (syntax=? (db-prefix? r1) (db-prefix? r2))))
        (%or (db-prefix? r1) (db-prefix? r2))
        (db-233 #b01 (r-3? r1) (r-3? r2))
        (%or (db-offset? r1) (db-offset? r2)))
      ((ld r n) (r-3? r)
        (db-prefix? r)
        (db-233 #b00 (r-3? r) #b110)
        (db-offset? r)
        (db-n n))))

  (define-rule-syntax (r-3? r)
    (syntax-match? #'r
      (b        #b000)
      (c        #b001)
      (d        #b010)
      (e        #b011)
      (h        #b100)
      (ixh      #b100)
      (iyh      #b100)
      (l        #b101)
      (ixl      #b101)
      (iyl      #b101)
      ((hl)     #b110)
      ((+ ix _) #b110)
      ((+ iy _) #b110)
      (a        #b111)))

  (define-rule-syntax (db-prefix? x)
    (syntax-match? #'x
      (ixh      (db-8 #xdd))
      (ixl      (db-8 #xdd))
      ((+ ix _) (db-8 #xdd))
      (iyh      (db-8 #xfd))
      (iyl      (db-8 #xfd))
      ((+ iy _) (db-8 #xfd))))

  (define-rule-syntax (db-offset? x)
    (syntax-match? #'x
      ((+ ix #'d) #'(db d))
      ((+ iy #'d) #'(db d))))

  (define-rule-syntax (add-3? x)
    (syntax-match? #'x
      (add #b000)
      (adc #b001)
      (sub #b010)
      (sbc #b011)))

  (define-rule-syntax (and-3? x)
    (syntax-match? #'x
      (and #b100)
      (or  #b101)
      (xor #b110)
      (cp  #b111)))

  (define-rule-syntax (inc-3? x)
    (syntax-match? #'x
      (inc #b100)
      (dec #b101)))

  (define-rule-syntax (p-3? p)
    (syntax-match? #'p
      (ixh #b100)
      (ixl #b101)))

  (define-rule-syntax (q-3? q)
    (syntax-match? #'q
      (iyh #b100)
      (iyl #b101)))

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
