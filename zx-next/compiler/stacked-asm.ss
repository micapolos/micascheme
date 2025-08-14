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
    (stacked regs (param-size ... return-size preserves-regs? op) ...)
    (regs contains registers which hold top elements of the logical stack)
    (remaining values are stored on the physical stack)
    (param-size ... contains sizes in bytes of op parameters)
    (return-size is byte size of the return value from the asm)
    (preserves-regs? specifies whether the op preserves regs or not)
    (op is abstract operation performed on the stack (for example block of assembler code)))

  (define (stacked->asm $stacked)
    (syntax-case $stacked (a l hl de %op)
      ((regs x y z ...)
        (syntax-case (stacked->asm #'(regs x)) ()
          ((regs-1 op-1 ...)
            (syntax-case (stacked->asm #'(regs-1 y z ...)) ()
              ((regs-2 op-2 ...)
                #'(regs-2 op-1 ... op-2 ...))))))

      ; op 0
      ((() (0 _ op))
        #'(() op))

      ((regs (0 #t op))
        #'(regs op))

      (((reg ...) (0 #f op))
        #'(() (reverse (push reg) ...) op))

      ; op 1
      ((() (1 _ op))
        #'((a) op))

      (((a) (1 _ op))
        #'((a l) (ld l a) op))

      (((a r) (1 _ op))
        #'((a l) (push r) (ld l a) op))

      (((hl) (1 _ op))
        #'((a de) (ex de hl) op))

      (((hl r) (1 _ op))
        #'((a de) (push r) (ex de hl) op))

      (((r) (1 _ op))
        #'((a) (push r) op))

      ; op 2
      ((() (2 _ op))
        #'((hl) op))

      (((a l) (2 _ op))
        #'((hl) (ld h a) (push hl) op))

      (((a de) (2 _ op))
        #'((hl) (push de) (push a) op))

      (((hl) (2 _ op))
        #'((hl de) (ex de hl) op))

      (((hl de) (2 _ op))
        #'((hl de) (push de) (ex de hl) op))

      (((r) (2 _ op))
        #'((hl) (push r) op))

      ; op 3
      ((() (3 _ op))
        #'((lde) op))

      (((a) (3 _ op))
        #'((lde) (push a) op))

      (((a l) (3 _ op))
        #'((lde) (ld h a) (push hl) op))

      (((a de) (3 _ op))
        #'((lde) (push de) (push a) op))

      (((hl) (3 _ op))
        #'((lde) (push hl) op))

      (((hl de) (3 _ op))
        #'((lde) (push de) (push hl) op))

      (((lde) (3 _ op))
        #'((lde) (push lde) op))

      (((hlde) (3 _ op))
        #'((hlde) (push hlde) op))

      ; op 4
      ((() (4 _ op))
        #'((hlde) op))

      (((a) (4 _ op))
        #'((hlde) (push a) op))

      (((a l) (4 _ op))
        #'((hlde) (ld h a) (push hl) op))

      (((a de) (4 _ op))
        #'((hlde) (push de) (push a) op))

      (((hl) (4 _ op))
        #'((hlde) (push hl) op))

      (((hl de) (4 _ op))
        #'((hlde) (push de) (push hl) op))

      (((lde) (4 _ op))
        #'((hlde) (push lde) op))

      (((hlde) (4 _ op))
        #'((hlde) (push hlde) op))

      ; op 1 0
      ((() (1 0 _ op))
        #'(() (pop a) op))

      (((a) (1 0 _ op))
        #'(() op))

      (((a l) (1 0 _ op))
        #'((l) op))

      (((a de) (1 0 _ op))
        #'((hl) op (ex de hl)))

      ; op 2 0
      ((() (2 0 _ op))
        #'(() (pop hl) op))

      (((hl) (2 0 _ op))
        #'(() op))

      (((hl de) (2 0 _ op))
        #'((hl) op (ex de hl)))

      ; op 3 0
      ((() (3 0 _ op))
        #'(() (pop lde) op))

      (((lde) (3 0 _ op))
        #'(() op))

      ; op 4 0
      ((() (4 0 _ op))
        #'(() (pop hlde) op))

      (((hlde) (4 0 _ op))
        #'(() op))

      ; op 1 1
      ((() (1 1 _ op))
        #'((a) (pop a) op))

      (((a . x) (1 1 _ op))
        #'((a . x) op))

      ; op 1 1 1
      ((() (1 1 1 _ op))
        ; TODO: This one could be optimized not to require two ld's.
        #'((a) (pop hl) (ld a l) (ld l h) op))

      (((a) (1 1 1 _ op))
        #'((a) (pop l) op))

      (((a l) (1 1 1 _ op))
        #'((a) op))

      ; op 2 1
      ((() (2 1 _ op))
        #'((a) (pop hl) op))

      (((hl . regs) (2 1 _ op))
        #'((a . regs) op))))

  ; === test ====
  (define-rule-syntax (check-stacked->asm x out)
    (check (equal? (syntax->datum (stacked->asm #'x)) 'out)))
)
