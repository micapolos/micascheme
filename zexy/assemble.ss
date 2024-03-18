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

  (define-syntax asm
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $op ...)
          #`(list
            #,@(map
              (lambda ($op)
                (syntax-case $op ()
                  (($op $expr)
                    #'#`($op #,$expr))))
              (syntax->list #'($op ...))))))))

  (define (assemble $op)
    (or
      (syntax-case $op (nop ret ld halt a i r bc de hl)
        ((nop)
          (asm (db #x00)))
        ((ret)
          (asm (db #xc9)))
        ((halt)
          (asm (db #b01110110)))

        ((ld $r (hl)) (r? #'$r)
          (asm (db (bitwise-ior #b01000110 (bitwise-arithmetic-shift-left (r? #'$r) 3)))))
        ((ld (hl) $r) (r? #'$r)
          (asm (db (bitwise-ior #b01110000 (r? #'$r)))))
        ((ld $r1 $r2) (and (r? #'$r1) (r? #'$r2))
          (asm (db (bitwise-ior #b01000000 (bitwise-arithmetic-shift-left (r? #'$r1) 3) (r? #'$r2)))))
        ((ld (hl) $r)
          (asm
            (db (bitwise-ior #b00110110))
            (db #'$r)))

        ((ld a (bc)) (asm (db #x0a)))
        ((ld a (de)) (asm (db #x1a)))
        ((ld a ($nm)) (asm (db #x3a) (dw #'$nm)))

        ((ld (bc) a) (asm (db #x02)))
        ((ld (de) a) (asm (db #x12)))
        ((ld ($nm) a) (asm (db #x32) (dw #'$nm)))

        ((ld a i) (asm (db #xed) (db #x57)))
        ((ld a r) (asm (db #xed) (db #x5f)))
        ((ld i a) (asm (db #xed) (db #x47)))
        ((ld r a) (asm (db #xed) (db #x4f)))

        ((ld $r $n) (r? #'$r)
          (asm
            (db (bitwise-ior #b00000110 (bitwise-arithmetic-shift-left (r? #'$r) 3)))
            (db #'$n)))

        ($other #f))
      (list $op)))

  (define (r? $syntax)
    (syntax-case $syntax (b c d e h l a)
      (b #b000)
      (c #b001)
      (d #b010)
      (e #b011)
      (h #b100)
      (l #b101)
      (a #b111)
      (_ #f)))
)
