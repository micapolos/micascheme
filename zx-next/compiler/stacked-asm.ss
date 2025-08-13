(library (zx-next compiler stacked-asm)
  (export
    stacked-asm
    check-stacked-asm)
  (import
    (only (micascheme) logging define-rules-syntaxes syntax-case ... syntax->list quasisyntax unsyntax reverse quote syntax check equal? syntax->datum literals)
    (prefix (zx-next compiler stacked) %)
    (asm z80))

  (define-rules-syntaxes
    (literals a l hl %stacked %op)

    ; terminal
    ((asm . x)
      #`(asm . x))

    ; top-level
    ((stacked-asm x)
      (stacked-asm () x))

    ; append
    ((stacked-asm regs x y z ...)
      (syntax-case (stacked-asm regs x) ()
        ((asm regs-1 op-1 (... ...))
          (syntax-case (stacked-asm regs-1 y z ...) ()
            ((asm regs-2 op-2 (... ...))
              #'(asm regs-2 op-1 (... ...) op-2 (... ...)))))))

    ; op 0
    ((stacked-asm regs (%op 0 op))
      (asm regs op))

    ; op 1
    ((stacked-asm () (%op 1 op))
      (asm (a) op))

    ((stacked-asm (a) (%op 1 op))
      (asm (a l) (ld l a) op))

    ((stacked-asm (a r) (%op 1 op))
      (asm (a l) (push r) (ld l a) op))

    ((stacked-asm (hl) (%op 1 op))
      (asm (a de) (ex de hl) op))

    ((stacked-asm (hl r) (%op 1 op))
      (asm (a de) (push r) (ex de hl) op))

    ((stacked-asm (r) (%op 1 op))
      (asm (a) (push r) op))

    ; op 2
    ((stacked-asm () (%op 2 op))
      (asm (hl) op))

    ((stacked-asm (a l) (%op 2 op))
      (asm (hl) (ld h a) (push hl) op))

    ((stacked-asm (a de) (%op 2 op))
      (asm (hl) (push de) (push a) op))

    ((stacked-asm (hl) (%op 2 op))
      (asm (hl de) (ex de hl) op))

    ((stacked-asm (hl de) (%op 2 op))
      (asm (hl de) (push de) (ex de hl) op))

    ((stacked-asm (r) (%op 2 op))
      (asm (hl) (push r) op))

    ; op 3
    ((stacked-asm () (%op 3 op))
      (asm (lde) op))

    ((stacked-asm (a) (%op 3 op))
      (asm (lde) (push a) op))

    ((stacked-asm (a l) (%op 3 op))
      (asm (lde) (ld h a) (push hl) op))

    ((stacked-asm (a de) (%op 3 op))
      (asm (lde) (push de) (push a) op))

    ((stacked-asm (hl) (%op 3 op))
      (asm (lde) (push hl) op))

    ((stacked-asm (hl de) (%op 3 op))
      (asm (lde) (push de) (push hl) op))

    ((stacked-asm (lde) (%op 3 op))
      (asm (lde) (push lde) op))

    ((stacked-asm (hlde) (%op 3 op))
      (asm (hlde) (push hlde) op))

    ; op 4
    ((stacked-asm () (%op 4 op))
      (asm (hlde) op))

    ((stacked-asm (a) (%op 4 op))
      (asm (hlde) (push a) op))

    ((stacked-asm (a l) (%op 4 op))
      (asm (hlde) (ld h a) (push hl) op))

    ((stacked-asm (a de) (%op 4 op))
      (asm (hlde) (push de) (push a) op))

    ((stacked-asm (hl) (%op 4 op))
      (asm (hlde) (push hl) op))

    ((stacked-asm (hl de) (%op 4 op))
      (asm (hlde) (push de) (push hl) op))

    ((stacked-asm (lde) (%op 4 op))
      (asm (hlde) (push lde) op))

    ((stacked-asm (hlde) (%op 4 op))
      (asm (hlde) (push hlde) op))

    ; op 1 1
    ((stacked-asm () (%op 1 1 op))
      (asm (a) (pop a) op))

    ((stacked-asm (a . x) (%op 1 1 op))
      (asm (a . x) op))

    ; op 1 1 1
    ((stacked-asm () (%op 1 1 1 op))
      ; TODO: This one could be optimized not to require two ld's.
      (asm (a) (pop hl) (ld a l) (ld l h) op))

    ((stacked-asm (a) (%op 1 1 1 op))
      (asm (a) (pop l) op))

    ((stacked-asm (a l) (%op 1 1 1 op))
      (asm (a) op))

    ; op 2 1
    ((stacked-asm () (%op 2 1 op))
      (asm (a) (pop hl) op))

    ((stacked-asm (hl . regs) (%op 2 1 op))
      (asm (a . regs) op))

    ; === test ====
    ((check-stacked-asm (%stacked . x) out)
      (check (equal? (syntax->datum (stacked-asm . x)) 'out))))
)
