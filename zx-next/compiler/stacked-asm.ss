(library (zx-next compiler stacked-asm)
  (export
    stacked->asm
    check-stacked->asm)
  (import
    (only (micascheme) define define-rule-syntax syntax-case ... quote syntax check equal? syntax->datum)
    (prefix (zx-next compiler stacked) %)
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
      ((regs (%op 0 op))
        #'(regs op))

      ; op 1
      ((() (%op 1 op))
        #'((a) op))

      (((a) (%op 1 op))
        #'((a l) (ld l a) op))

      (((a r) (%op 1 op))
        #'((a l) (push r) (ld l a) op))

      (((hl) (%op 1 op))
        #'((a de) (ex de hl) op))

      (((hl r) (%op 1 op))
        #'((a de) (push r) (ex de hl) op))

      (((r) (%op 1 op))
        #'((a) (push r) op))

      ; op 2
      ((() (%op 2 op))
        #'((hl) op))

      (((a l) (%op 2 op))
        #'((hl) (ld h a) (push hl) op))

      (((a de) (%op 2 op))
        #'((hl) (push de) (push a) op))

      (((hl) (%op 2 op))
        #'((hl de) (ex de hl) op))

      (((hl de) (%op 2 op))
        #'((hl de) (push de) (ex de hl) op))

      (((r) (%op 2 op))
        #'((hl) (push r) op))

      ; op 3
      ((() (%op 3 op))
        #'((lde) op))

      (((a) (%op 3 op))
        #'((lde) (push a) op))

      (((a l) (%op 3 op))
        #'((lde) (ld h a) (push hl) op))

      (((a de) (%op 3 op))
        #'((lde) (push de) (push a) op))

      (((hl) (%op 3 op))
        #'((lde) (push hl) op))

      (((hl de) (%op 3 op))
        #'((lde) (push de) (push hl) op))

      (((lde) (%op 3 op))
        #'((lde) (push lde) op))

      (((hlde) (%op 3 op))
        #'((hlde) (push hlde) op))

      ; op 4
      ((() (%op 4 op))
        #'((hlde) op))

      (((a) (%op 4 op))
        #'((hlde) (push a) op))

      (((a l) (%op 4 op))
        #'((hlde) (ld h a) (push hl) op))

      (((a de) (%op 4 op))
        #'((hlde) (push de) (push a) op))

      (((hl) (%op 4 op))
        #'((hlde) (push hl) op))

      (((hl de) (%op 4 op))
        #'((hlde) (push de) (push hl) op))

      (((lde) (%op 4 op))
        #'((hlde) (push lde) op))

      (((hlde) (%op 4 op))
        #'((hlde) (push hlde) op))

      ; op 1 0
      ((() (%op 1 0 op))
        #'(() (pop a) op))

      (((a . x) (%op 1 0 op))
        #'(x op))

      ; op 1 1
      ((() (%op 1 1 op))
        #'((a) (pop a) op))

      (((a . x) (%op 1 1 op))
        #'((a . x) op))

      ; op 1 1 1
      ((() (%op 1 1 1 op))
        ; TODO: This one could be optimized not to require two ld's.
        #'((a) (pop hl) (ld a l) (ld l h) op))

      (((a) (%op 1 1 1 op))
        #'((a) (pop l) op))

      (((a l) (%op 1 1 1 op))
        #'((a) op))

      ; op 2 1
      ((() (%op 2 1 op))
        #'((a) (pop hl) op))

      (((hl . regs) (%op 2 1 op))
        #'((a . regs) op))))

  ; === test ====
  (define-rule-syntax (check-stacked->asm x out)
    (check (equal? (syntax->datum (stacked->asm #'x)) 'out)))
)
