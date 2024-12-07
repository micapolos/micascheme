(library (z80 asm)
  (export
    op->asm)
  (import
    (rename (micascheme)
      (and %and)
      (not %not))
    (z80 keywords))

  (define-rule-syntax (asm-syntax-case stx literals (pattern fender body ... ) ...)
    (syntax-case stx literals (pattern fender (list->asm body ...)) ...))

  (define (op->asm $op)
    (asm-syntax-case $op (nop ld)
      ((nop) #t
        (db-8 0))
      ((halt) #t
        (db-8 #b01110110))
      ((ld r1 r2) (%and (r? r1) (r? r2))
        (db-233 #b01 (r? r1) (r? r2)))
      ((ld p1 p2) (%and (p? p1) (p? p2))
        (db-8 #xdd) (db-233 #b01 (p? p1) (p? p2)))
      ((ld q1 q2) (%and (q? q1) (q? q2))
        (db-8 #xfd) (db-233 #b01 (q? q1) (q? q2)))
      ((ld r (hl)) (r? r)
        (db-233 #b01 (r? r) #b110))
      ((ld (hl) r) (r? r)
        (db-233 #b01 #b110 (r? r)))
      ((ld r n) (r? r)
        (db-233 #b00 (r? r) #b110)
        (db-n n))
      ((ld (hl) n) #t
        (db-233 #b00 #b110 #b110)
        (db-n n))))

  (define-rule-syntax (r? r)
    (syntax-case #'r (b c d e h l a)
      (b #b000)
      (c #b001)
      (d #b010)
      (e #b011)
      (h #b100)
      (l #b101)
      (a #b111)
      (_ #f)))

  (define-rule-syntax (p? p)
    (syntax-case #'p (ixh ixl)
      (ixh #b100)
      (ixl #b101)
      (_ #f)))

  (define-rule-syntax (q? q)
    (syntax-case #'q (iyh iyl)
      (iyh #b100)
      (iyl #b101)
      (_ #f)))

  (define-rule-syntax (ihl? r)
    (syntax-case #'r (hl)
      ((hl) #b110)
      (_ #f)))

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

  (define (list->asm . $ops)
    #`(asm #,@$ops))
)
