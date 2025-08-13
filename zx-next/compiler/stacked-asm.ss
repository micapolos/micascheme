(library (zx-next compiler stacked-asm)
  (export
    stacked-asm
    check-stacked-asm)
  (import
    (only (micascheme) define-rules-syntaxes ... syntax->list quasisyntax unsyntax reverse quote syntax check equal? syntax->datum literals)
    (prefix (zx-next compiler stacked) %)
    (asm z80))

  (define-rules-syntaxes
    (literals a l hl %stacked %op)

    ; terminal
    ((stacked-asm regs () ops)
      #`(asm regs . #,(reverse (syntax->list #'ops))))

    ; op 1
    ((stacked-asm () ((%op 1 op) . xs) ops)
      (stacked-asm (a) xs (op . ops)))

    ((stacked-asm (a) ((%op 1 op) . xs) ops)
      (stacked-asm (a l) xs (op (ld l a) . ops)))

    ((stacked-asm (a r) ((%op 1 op) . xs) ops)
      (stacked-asm (a l) xs (op (ld l a) (push r) . ops)))

    ((stacked-asm (hl) ((%op 1 op) . xs) ops)
      (stacked-asm (a de) xs (op (ex de hl) . ops)))

    ((stacked-asm (hl r) ((%op 1 op) . xs) ops)
      (stacked-asm (a de) xs (op (ex de hl) (push r) . ops)))

    ((stacked-asm (r) ((%op 1 op) . xs) ops)
      (stacked-asm (a) xs (op (push r) . ops)))

    ; op 2
    ((stacked-asm () ((%op 2 op) . xs) ops)
      (stacked-asm (hl) xs (op . ops)))

    ((stacked-asm (a l) ((%op 2 op) . xs) ops)
      (stacked-asm (hl) xs (op (push hl) (ld h a) . ops)))

    ((stacked-asm (a de) ((%op 2 op) . xs) ops)
      (stacked-asm (hl) xs (op (push a) (push de) . ops)))

    ((stacked-asm (hl) ((%op 2 op) . xs) ops)
      (stacked-asm (hl de) xs (op (ex de hl) . ops)))

    ((stacked-asm (hl de) ((%op 2 op) . xs) ops)
      (stacked-asm (hl de) xs (op (ex de hl) (push de) . ops)))

    ((stacked-asm (r) ((%op 2 op) . xs) ops)
      (stacked-asm (hl) xs (op (push r) . ops)))

    ; op 3
    ((stacked-asm () ((%op 3 op) . xs) ops)
      (stacked-asm (lde) xs (op . ops)))

    ((stacked-asm (a) ((%op 3 op) . xs) ops)
      (stacked-asm (lde) xs (op (push a) . ops)))

    ((stacked-asm (a l) ((%op 3 op) . xs) ops)
      (stacked-asm (lde) xs (op (push hl) (ld h a). ops)))

    ((stacked-asm (a de) ((%op 3 op) . xs) ops)
      (stacked-asm (lde) xs (op (push a) (push de). ops)))

    ((stacked-asm (hl) ((%op 3 op) . xs) ops)
      (stacked-asm (lde) xs (op (push hl) . ops)))

    ((stacked-asm (hl de) ((%op 3 op) . xs) ops)
      (stacked-asm (lde) xs (op (push hl) (push de). ops)))

    ((stacked-asm (lde) ((%op 3 op) . xs) ops)
      (stacked-asm (lde) xs (op (push lde). ops)))

    ((stacked-asm (hlde) ((%op 3 op) . xs) ops)
      (stacked-asm (hlde) xs (op (push hlde). ops)))

    ; op 4
    ((stacked-asm () ((%op 4 op) . xs) ops)
      (stacked-asm (hlde) xs (op . ops)))

    ((stacked-asm (a) ((%op 4 op) . xs) ops)
      (stacked-asm (hlde) xs (op (push a) . ops)))

    ((stacked-asm (a l) ((%op 4 op) . xs) ops)
      (stacked-asm (hlde) xs (op (push hl) (ld h a). ops)))

    ((stacked-asm (a de) ((%op 4 op) . xs) ops)
      (stacked-asm (hlde) xs (op (push a) (push de). ops)))

    ((stacked-asm (hl) ((%op 4 op) . xs) ops)
      (stacked-asm (hlde) xs (op (push hl) . ops)))

    ((stacked-asm (hl de) ((%op 4 op) . xs) ops)
      (stacked-asm (hlde) xs (op (push hl) (push de). ops)))

    ((stacked-asm (lde) ((%op 4 op) . xs) ops)
      (stacked-asm (hlde) xs (op (push lde). ops)))

    ((stacked-asm (hlde) ((%op 4 op) . xs) ops)
      (stacked-asm (hlde) xs (op (push hlde). ops)))

    ; op 1 1
    ((stacked-asm () ((%op 1 1 op) . xs) ops)
      (stacked-asm (a) xs (op (pop a) . ops)))

    ((stacked-asm (a . x) ((%op 1 1 op) . xs) ops)
      (stacked-asm (a . x) xs (op . ops)))

    ; op 1 1 1
    ((stacked-asm () ((%op 1 1 1 op) . xs) ops)
      ; TODO: This one could be optimized not to require two ld's.
      (stacked-asm (a) xs (op (ld l h) (ld a l) (pop hl) . ops)))

    ((stacked-asm (a) ((%op 1 1 1 op) . xs) ops)
      (stacked-asm (a) xs (op (pop l) . ops)))

    ((stacked-asm (a l) ((%op 1 1 1 op) . xs) ops)
      (stacked-asm (a) xs (op . ops)))

    ; op 2 1
    ((stacked-asm () ((%op 2 1 op) . xs) ops)
      (stacked-asm (a) xs (op (pop hl) . ops)))

    ((stacked-asm (hl . regs) ((%op 2 1 op) . xs) ops)
      (stacked-asm (a . regs) xs (op . ops)))

    ; === test ====
    ((check-stacked-asm (%stacked regs x) out)
      (check (equal? (syntax->datum (stacked-asm regs (x) ())) 'out)))
  )
)
