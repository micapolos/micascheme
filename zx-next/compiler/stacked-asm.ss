(library (zx-next compiler stacked-asm)
  (export
    push
    stacked->asm
    check-stacked->asm)
  (import
    (only (micascheme) comment define-keywords define define-rule-syntax syntax-case ... quote syntax check equal? syntax->datum)
    (asm z80))

  (define-keywords push-all)

  (comment
    (stacked (reg ...) (param-size ... return-size preserves-regs? asm) ...)
    (details
      (regs contains registers which hold top elements of the logical stack)
      (remaining values are stored on the physical stack)
      (param-size ... contains sizes in bytes of op parameters)
      (return-size is byte size of the return value from the asm)
      (preserves-regs? specifies whether the op preserves regs or not)
      (asm contains code executed on the registers and the physical stack))
    (valid combinations of regs are () (a) (a l) (a de) (hl) (hl de) (lde) (hlde))
    (it corresponds to sdcc-1 calling conversion))

  (define (stacked->asm $stacked)
    (syntax-case $stacked (a l hl de)
      ((regs x y z ...)
        (syntax-case (stacked->asm #'(regs x)) ()
          ((regs-1 asm-1 ...)
            (syntax-case (stacked->asm #'(regs-1 y z ...)) ()
              ((regs-2 asm-2 ...)
                #'(regs-2 asm-1 ... asm-2 ...))))))

      ((() (size ... #f asm))
        (stacked->asm #'(() (size ... #t asm))))

      (((a l) (size ... #f asm))
        (stacked->asm #'(() (0 #t (ld h a)) (0 #t (push hl)) (size ... #t asm))))

      (((regs ... reg) (size ... #f asm))
        (stacked->asm #'((regs ...) (0 #t (push reg)) (size ... #f asm))))

      ; op 0
      ((() (0 _ asm))
        #'(() asm))

      ((regs (0 _ asm))
        #'(regs asm))

      ; op 1
      ((() (1 _ asm))
        #'((a) asm))

      (((reg ...) (1 #f asm))
        #'((a) (reverse (push reg) ...) asm))

      (((a) (1 _ asm))
        #'((a l) (ld l a) asm))

      (((a r) (1 _ asm))
        #'((a l) (push r) (ld l a) asm))

      (((hl) (1 _ asm))
        #'((a de) (ex de hl) asm))

      (((hl r) (1 _ asm))
        #'((a de) (push r) (ex de hl) asm))

      (((r) (1 _ asm))
        #'((a) (push r) asm))

      ; op 2
      ((() (2 _ asm))
        #'((hl) asm))

      (((reg ...) (2 #f asm))
        #'((hl) (reverse (push reg) ...) asm))

      (((a l) (2 _ asm))
        #'((hl) (ld h a) (push hl) asm))

      (((a de) (2 _ asm))
        #'((hl) (push de) (push a) asm))

      (((hl) (2 _ asm))
        #'((hl de) (ex de hl) asm))

      (((hl de) (2 _ asm))
        #'((hl de) (push de) (ex de hl) asm))

      (((r) (2 _ asm))
        #'((hl) (push r) asm))

      ; op 3
      ((() (3 _ asm))
        #'((lde) asm))

      (((a) (3 _ asm))
        #'((lde) (push a) asm))

      (((a l) (3 _ asm))
        #'((lde) (ld h a) (push hl) asm))

      (((a de) (3 _ asm))
        #'((lde) (push de) (push a) asm))

      (((hl) (3 _ asm))
        #'((lde) (push hl) asm))

      (((hl de) (3 _ asm))
        #'((lde) (push de) (push hl) asm))

      (((lde) (3 _ asm))
        #'((lde) (push lde) asm))

      (((hlde) (3 _ asm))
        #'((hlde) (push hlde) asm))

      ; op 4
      ((() (4 _ asm))
        #'((hlde) asm))

      (((a) (4 _ asm))
        #'((hlde) (push a) asm))

      (((a l) (4 _ asm))
        #'((hlde) (ld h a) (push hl) asm))

      (((a de) (4 _ asm))
        #'((hlde) (push de) (push a) asm))

      (((hl) (4 _ asm))
        #'((hlde) (push hl) asm))

      (((hl de) (4 _ asm))
        #'((hlde) (push de) (push hl) asm))

      (((lde) (4 _ asm))
        #'((hlde) (push lde) asm))

      (((hlde) (4 _ asm))
        #'((hlde) (push hlde) asm))

      ; op 1 0
      ((() (1 0 _ asm))
        #'(() (pop a) asm))

      (((a) (1 0 _ asm))
        #'(() asm))

      (((a l) (1 0 _ asm))
        #'((l) asm))

      (((a de) (1 0 _ asm))
        #'((hl) asm (ex de hl)))

      ; op 2 0
      ((() (2 0 _ asm))
        #'(() (pop hl) asm))

      (((hl) (2 0 _ asm))
        #'(() asm))

      (((hl de) (2 0 _ asm))
        #'((hl) asm (ex de hl)))

      ; op 3 0
      ((() (3 0 _ asm))
        #'(() (pop lde) asm))

      (((lde) (3 0 _ asm))
        #'(() asm))

      ; op 4 0
      ((() (4 0 _ asm))
        #'(() (pop hlde) asm))

      (((hlde) (4 0 _ asm))
        #'(() asm))

      ; op 1 1
      ((() (1 1 _ asm))
        #'((a) (pop a) asm))

      (((a . x) (1 1 _ asm))
        #'((a . x) asm))

      ; op 1 1 1
      ((() (1 1 1 _ asm))
        ; TODO: This one could be optimized not to require two ld's.
        #'((a) (pop hl) (ld a l) (ld l h) asm))

      (((a) (1 1 1 _ asm))
        #'((a) (pop l) asm))

      (((a l) (1 1 1 _ asm))
        #'((a) asm))

      ; op 2 1
      ((() (2 1 _ asm))
        #'((a) (pop hl) asm))

      (((hl . regs) (2 1 _ asm))
        #'((a . regs) asm))))

  ; === test ====
  (define-rule-syntax (check-stacked->asm x out)
    (check (equal? (syntax->datum (stacked->asm #'x)) 'out)))
)
