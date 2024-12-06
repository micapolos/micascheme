(library (z80 asm)
  (export
    op->asm)
  (import
    (rename (micascheme)
      (and %and)
      (not %not))
    (z80 keywords))

  (define (op->asm $op)
    (syntax-case $op (nop ld)
      ((nop)
        (list->asm (db-8 0)))
      ((halt)
        (list->asm (db-8 #b01110110)))
      ((ld r1 r2)
        (%and (r? r1) (r? r2))
        (list->asm (db-233 #b01 (r? r1) (r? r2))))
      ((ld r (hl))
        (r? r)
        (list->asm (db-233 #b01 (r? r) #b110)))
      ((ld (hl) r)
        (r? r)
        (list->asm (db-233 #b01 #b110 (r? r))))
      ((ld r n)
        (r? r)
        (list->asm
          (db-233 #b00 (r? r) #b110)
          (db-n n)))
      ((ld (hl) n)
        (list->asm
          (db-233 #b00 #b110 #b110)
          (db-n n)))))

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
