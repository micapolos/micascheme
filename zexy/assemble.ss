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
    (or
      (syntax-case $op (nop ret ld halt a i r bc de hl)
        ((nop)
          (list (u8 #x00)))
        ((ret)
          (list (u8 #xc9)))
        ((halt)
          (list (u8 #b01110110)))

        ((ld $r (hl)) (r3 #'$r)
          (list (u8-233 #b01 (r3 #'$r) #b110)))
        ((ld (hl) $r) (r3 #'$r)
          (list (u8-233 #b01 #b110 (r3 #'$r))))
        ((ld $r1 $r2) (and (r3 #'$r1) (r3 #'$r2))
          (list (u8-233 #b01 (r3 #'$r1) (r3 #'$r2))))
        ((ld (hl) $n)
          (list
            (u8 #b00110110)
            (u8 #'$n)))

        ((ld a (bc)) (list (u8 #x0a)))
        ((ld a (de)) (list (u8 #x1a)))
        ((ld a ($nm)) (list (u8 #x3a) (u16 #'$nm)))

        ((ld (bc) a) (list (u8 #x02)))
        ((ld (de) a) (list (u8 #x12)))
        ((ld ($nm) a) (list (u8 #x32) (u16 #'$nm)))

        ((ld a i) (list (u8 #xed) (u8 #x57)))
        ((ld a r) (list (u8 #xed) (u8 #x5f)))
        ((ld i a) (list (u8 #xed) (u8 #x47)))
        ((ld r a) (list (u8 #xed) (u8 #x4f)))

        ((ld $r $n) (r3 #'$r)
          (list
            (u8-233 #b00 (r3 #'$r) #b110)
            (u8 #'$n)))

        ($other #f))
      (list $op)))

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

  (define (u8 $value)
    #`(db #,$value))

  (define (u16 $value)
    #`(dw #,$value))

  (define (u8-233 $a $b $c)
    (u8
      (bitwise-ior
        (bitwise-arithmetic-shift-left $a 6)
        (bitwise-arithmetic-shift-left $b 3)
        $c)))
)
