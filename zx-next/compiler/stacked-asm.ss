(library (zx-next compiler stacked-asm)
  (export
    stacked-asm
    check-stacked-asm)
  (import
    (only (micascheme) define-rules-syntaxes ... syntax->list quasisyntax unsyntax reverse quote syntax check equal? syntax->datum literals)
    (prefix (zx-next compiler stacked) %)
    (asm z80))

  (define-rules-syntaxes
    (literals a l hl %stacked %op %op1 %op2 %peek
      %const
      %inc %dec
      %add %sub %and %or %xor)

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

    ; const 1
    ((stacked-asm regs ((%const 1 n) . xs) ops)
      (stacked-asm regs ((%op 1 (ld a n)) . xs) ops))

    ; const 2
    ((stacked-asm regs ((%const 2 nn) . xs) ops)
      (stacked-asm regs ((%op 2 (ld hl nn) . xs)) ops))

    ; inc/dec 1
    ((stacked-asm regs ((%inc 1) . xs) ops)
      (stacked-asm regs ((%op 1 1 (inc a)) . xs) ops))

    ((stacked-asm regs ((%dec 1) . xs) ops)
      (stacked-asm regs ((%op 1 1 (dec a)) . xs) ops))

    ; add/sub/and/or/xor 1
    ((stacked-asm regs ((%add 1) . xs) ops)
      (stacked-asm regs ((%op 1 1 1 (add l)) . xs) ops))

    ((stacked-asm regs ((%sub 1) . xs) ops)
      (stacked-asm regs ((%op 1 1 1 (sub l)) . xs) ops))

    ((stacked-asm regs ((%and 1) . xs) ops)
      (stacked-asm regs ((%op 1 1 1 (and l)) . xs) ops))

    ((stacked-asm regs ((%or 1) . xs) ops)
      (stacked-asm regs ((%op 1 1 1 (or l)) . xs) ops))

    ((stacked-asm regs ((%xor 1) . xs) ops)
      (stacked-asm regs ((%op 1 1 1 (xor l)) . xs) ops))

    ; peek
    ((stacked-asm regs ((%peek 1) . xs) ops)
      (stacked-asm regs ((%op 2 1 (ld a (hl))) . xs) ops))

    ; peek offset
    ((stacked-asm regs ((%peek 1 offset) . xs) ops)
      (stacked-asm regs ((%op 1 (ld a (+ ix offset))) . xs) ops))

    ; === test ====
    ((check-stacked-asm (%stacked regs x) out)
      (check (equal? (syntax->datum (stacked-asm regs (x) ())) 'out)))
  )
)
