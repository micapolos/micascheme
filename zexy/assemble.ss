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

        ((ld $r (hl)) (fxr #'$r)
          (asm (db (fxior #b01000110 (fxsll (fxr #'$r) 3)))))
        ((ld (hl) $r) (fxr #'$r)
          (asm (db (fxior #b01110000 (fxr #'$r)))))
        ((ld $r1 $r2) (and (fxr #'$r1) (fxr #'$r2))
          (asm (db (fxior #b01000000 (fxsll (fxr #'$r1) 3) (fxr #'$r2)))))
        ((ld (hl) $r)
          (asm
            (db (fxior #b00110110))
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

        ((ld $r $n) (fxr #'$r)
          (asm
            (db (fxior #b00000110 (fxsll (fxr #'$r) 3)))
            (db #'$n)))

        ($other #f))
      (list $op)))

  (define (fxr $syntax)
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
