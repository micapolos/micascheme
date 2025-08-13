(library (zx-next compiler stacked-asm)
  (export
    stacked->asm
    check-stacked->asm)
  (import
    (only (micascheme) define define-rule-syntax syntax-case ... quote syntax check equal? syntax->datum)
    (asm z80))

  (define (stacked->asm $stacked)
    (syntax-case $stacked (a l hl de %op)
      ((regs x y z ...)
        (syntax-case (stacked->asm #'(regs x)) ()
          ((regs-1 op-1 ...)
            (syntax-case (stacked->asm #'(regs-1 y z ...)) ()
              ((regs-2 op-2 ...)
                #'(regs-2 op-1 ... op-2 ...))))))

      ; op 0
      ((regs (0 op))
        #'(regs op))

      ; op 1
      ((() (1 op))
        #'((a) op))

      (((a) (1 op))
        #'((a l) (ld l a) op))

      (((a r) (1 op))
        #'((a l) (push r) (ld l a) op))

      (((hl) (1 op))
        #'((a de) (ex de hl) op))

      (((hl r) (1 op))
        #'((a de) (push r) (ex de hl) op))

      (((r) (1 op))
        #'((a) (push r) op))

      ; op 2
      ((() (2 op))
        #'((hl) op))

      (((a l) (2 op))
        #'((hl) (ld h a) (push hl) op))

      (((a de) (2 op))
        #'((hl) (push de) (push a) op))

      (((hl) (2 op))
        #'((hl de) (ex de hl) op))

      (((hl de) (2 op))
        #'((hl de) (push de) (ex de hl) op))

      (((r) (2 op))
        #'((hl) (push r) op))

      ; op 3
      ((() (3 op))
        #'((lde) op))

      (((a) (3 op))
        #'((lde) (push a) op))

      (((a l) (3 op))
        #'((lde) (ld h a) (push hl) op))

      (((a de) (3 op))
        #'((lde) (push de) (push a) op))

      (((hl) (3 op))
        #'((lde) (push hl) op))

      (((hl de) (3 op))
        #'((lde) (push de) (push hl) op))

      (((lde) (3 op))
        #'((lde) (push lde) op))

      (((hlde) (3 op))
        #'((hlde) (push hlde) op))

      ; op 4
      ((() (4 op))
        #'((hlde) op))

      (((a) (4 op))
        #'((hlde) (push a) op))

      (((a l) (4 op))
        #'((hlde) (ld h a) (push hl) op))

      (((a de) (4 op))
        #'((hlde) (push de) (push a) op))

      (((hl) (4 op))
        #'((hlde) (push hl) op))

      (((hl de) (4 op))
        #'((hlde) (push de) (push hl) op))

      (((lde) (4 op))
        #'((hlde) (push lde) op))

      (((hlde) (4 op))
        #'((hlde) (push hlde) op))

      ; op 1 0
      ((() (1 0 op))
        #'(() (pop a) op))

      (((a . x) (1 0 op))
        #'(x op))

      ; op 1 1
      ((() (1 1 op))
        #'((a) (pop a) op))

      (((a . x) (1 1 op))
        #'((a . x) op))

      ; op 1 1 1
      ((() (1 1 1 op))
        ; TODO: This one could be optimized not to require two ld's.
        #'((a) (pop hl) (ld a l) (ld l h) op))

      (((a) (1 1 1 op))
        #'((a) (pop l) op))

      (((a l) (1 1 1 op))
        #'((a) op))

      ; op 2 1
      ((() (2 1 op))
        #'((a) (pop hl) op))

      (((hl . regs) (2 1 op))
        #'((a . regs) op))))

  ; === test ====
  (define-rule-syntax (check-stacked->asm x out)
    (check (equal? (syntax->datum (stacked->asm #'x)) 'out)))
)
