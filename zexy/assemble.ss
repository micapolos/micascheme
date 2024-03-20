(library (zexy assemble)
  (export
    assemble)
  (import
    (micascheme)
    (rename (zexy ops)
      (and %and)
      (or %or)
      (push %push)
      (pop %pop)))

  (define (assemble $op)
    (syntax-case $op (nop ret ld halt a i r bc de hl)
      ((nop) (list (db-8 #x00)))
      ((ret) (list (db-8 #xc9)))
      ((halt) (list (db-8 #b01110110)))

      ((ld $r (hl))
        (r3 #'$r)
        (list (db-233 #b01 (r3 #'$r) #b110)))

      ((ld (hl) $r)
        (r3 #'$r)
        (list (db-233 #b01 #b110 (r3 #'$r))))

      ((ld $r1 $r2)
        (and (r3 #'$r1) (r3 #'$r2))
        (list (db-233 #b01 (r3 #'$r1) (r3 #'$r2))))

      ((ld (hl) $n) (list (db-8 #b00110110) #'(db $n)))

      ((ld a (bc)) (list (db-8 #x0a)))
      ((ld a (de)) (list (db-8 #x1a)))
      ((ld a ($nm)) (list (db-8 #x3a) #'(dw $nm)))

      ((ld (bc) a) (list (db-8 #x02)))
      ((ld (de) a) (list (db-8 #x12)))
      ((ld ($nm) a) (list (db-8 #x32) #'(dw $nm)))

      ((ld a i) (list (db-8 #xed) (db-8 #x57)))
      ((ld a r) (list (db-8 #xed) (db-8 #x5f)))
      ((ld i a) (list (db-8 #xed) (db-8 #x47)))
      ((ld r a) (list (db-8 #xed) (db-8 #x4f)))

      ((ld $r $n)
        (r3 #'$r)
        (list (db-233 #b00 (r3 #'$r) #b110) #'(db $n)))

      ($other (list #'$other))))

  (define (r3 $syntax)
    (syntax-case $syntax (b c d e h l a)
      (b #b000)
      (c #b001)
      (d #b010)
      (e #b011)
      (h #b100)
      (l #b101)
      (a #b111)
      (_ #f)))

  (define (db-8 $value)
    #`(db #,(datum->syntax #'db-8 $value)))

  (define (db-233 $a $b $c)
    (db-8
      (bitwise-ior
        (bitwise-arithmetic-shift-left $a 6)
        (bitwise-arithmetic-shift-left $b 3)
        $c)))
)
