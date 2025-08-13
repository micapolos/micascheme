(library (zx-next compiler stacked-asm)
  (export
    stacked-asm
    check-stacked-asm)
  (import
    (only (micascheme) define-rules-syntaxes ... syntax->list quasisyntax unsyntax reverse quote syntax check equal? syntax->datum literals)
    (prefix (zx-next compiler stacked) %)
    (asm z80))

  (define-rules-syntaxes
    (literals a hl %stacked %const %op2 %add %sub %and %or %xor)

    ; terminal
    ((stacked-asm regs () ops)
      #`(asm regs . #,(reverse (syntax->list #'ops))))

    ; const 1
    ((stacked-asm () ((%const 1 n) . xs) ops)
      (stacked-asm (a) xs ((ld a n) . ops)))

    ((stacked-asm (a) ((%const 1 n) . xs) ops)
      (stacked-asm (a l) xs ((ld a n) (ld l a) . ops)))

    ((stacked-asm (a r) ((%const 1 n) . xs) ops)
      (stacked-asm (a l) xs ((ld a n) (ld l a) (push r) . ops)))

    ((stacked-asm (hl) ((%const 1 n) . xs) ops)
      (stacked-asm (a de) xs ((ld a n) (ex de hl) . ops)))

    ((stacked-asm (hl r) ((%const 1 n) . xs) ops)
      (stacked-asm (a de) xs ((ld a n) (ex de hl) (push r) . ops)))

    ((stacked-asm (r) ((%const 1 n) . xs) ops)
      (stacked-asm (a) xs ((ld a n) (push r) . ops)))

    ; op2 1
    ((stacked-asm () ((%op2 op 1) . xs) ops)
      (stacked-asm (a) xs ((op h) (ld a l) (pop hl) . ops)))

    ((stacked-asm (a) ((%op2 op 1) . xs) ops)
      (stacked-asm (a) xs ((op l) (pop l) . ops)))

    ((stacked-asm (a l) ((%op2 op 1) . xs) ops)
      (stacked-asm (a) xs ((op l) . ops)))

    ; add/sub/and/or/xor 1
    ((stacked-asm regs ((%add 1) . xs) ops)
      (stacked-asm regs ((%op2 add 1) . xs) ops))

    ((stacked-asm regs ((%sub 1) . xs) ops)
      (stacked-asm regs ((%op2 sub 1) . xs) ops))

    ((stacked-asm regs ((%and 1) . xs) ops)
      (stacked-asm regs ((%op2 and 1) . xs) ops))

    ((stacked-asm regs ((%or 1) . xs) ops)
      (stacked-asm regs ((%op2 or 1) . xs) ops))

    ((stacked-asm regs ((%xor 1) . xs) ops)
      (stacked-asm regs ((%op2 xor 1) . xs) ops))

    ((check-stacked-asm (%stacked regs x) out)
      (check (equal? (syntax->datum (stacked-asm regs (x) ())) 'out)))
  )
)
