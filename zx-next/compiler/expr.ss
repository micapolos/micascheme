(library (zx-next compiler expr)
  (export
    ld-expr
    peek-nn peek peek-offset
    zero? eq? gt?
    with-locals lets local arg)
  (import
    (zx-next core)
    (only (micascheme) -))

  (define-keywords
    peek-nn peek peek-offset
    lets local arg
    zero? eq? gt?)

  (define-ops
    (keywords
      a de hl ehl dehl
      neg cpl
      inc dec
      add sub and or xor
      add-n sub-n and-n or-n xor-n
      mul
      peek-nn peek peek-offset
      lets local arg
      zero? eq? gt?)

    ; Top-level
    ((ld-expr r x) (ld-expr r () () x))

    ; Load
    ((ld-expr r    args locals (1 n))    (ld r n))
    ((ld-expr rr   args locals (2 nn))   (ld rr nn))
    ((ld-expr ehl  args locals (3 nnn))  (ld ehl nnn))
    ((ld-expr dehl args locals (4 nnnn)) (ld dehl nnnn))

    ; Load indirect
    ((ld-expr a args locals (peek-nn 1 nn))
      (ld a (nn)))

    ((ld-expr r args locals (peek-nn 1 nn))
      (ld a (nn))
      (ld r a))

    ((ld-expr r args locals (peek 1 lhs))
      (ld-expr hl args locals lhs)
      (ld r (hl)))

    ((ld-expr r args locals (peek-offset 1 offset))
      (ld r (+ ix offset)))

    ; 8-bit increment/decrement
    ((ld-u8-op1 r args locals op lhs)
      (ld-expr r args locals lhs)
      (op r))

    ((ld-expr r args locals (inc 1 lhs))
      (ld-u8-op1 r args locals inc lhs))

    ((ld-expr r args locals (dec 1 lhs))
      (ld-u8-op1 r args locals dec lhs))

    ; 8-bit math with constant
    ((ld-u8-op2-n a args locals op lhs n)
      (ld-expr a args locals lhs)
      (op n))

    ((ld-u8-op2-n r args locals op lhs n)
      (ld-u8-op2-n a args locals op lhs n)
      (ld r a))

    ((ld-expr r args locals (add-n 1 lhs n))
      (ld-u8-op2-n r args locals add lhs n))
    ((ld-expr r args locals (sub-n 1 lhs n))
      (ld-u8-op2-n r args locals sub lhs n))
    ((ld-expr r args locals (and-n 1 lhs n))
      (ld-u8-op2-n r args locals and lhs n))
    ((ld-expr r args locals (or-n 1 lhs n))
      (ld-u8-op2-n r args locals or lhs n))
    ((ld-expr r args locals (xor-n 1 lhs n))
      (ld-u8-op2-n r args locals xor lhs n))

    ; 8-bit math
    ((ld-expr a args locals (neg 1 lhs))
      (ld-expr a args locals lhs)
      (neg))

    ((ld-expr r args locals (neg 1 lhs))
      (ld-expr a args locals (neg 1 lhs))
      (ld r a))

    ((ld-expr a args locals (cpl 1 lhs))
      (ld-expr a args locals lhs)
      (cpl))

    ((ld-expr r args locals (cpl 1 lhs))
      (ld-expr a args locals (cpl 1 lhs))
      (ld r a))

    ((ld-u8-op2 a args locals op lhs rhs)
      (ld-expr l args locals rhs)
      (push hl)
      (ld-expr a args locals lhs)
      (pop hl)
      (op l))

    ((ld-u8-op2 r args locals op lhs rhs)
      (ld-u8-op2 a args locals op lhs rhs)
      (ld r a))

    ((ld-expr r args locals (add 1 lhs rhs))
      (ld-u8-op2 r args locals add lhs rhs))
    ((ld-expr r args locals (sub 1 lhs rhs))
      (ld-u8-op2 r args locals sub lhs rhs))
    ((ld-expr r args locals (and 1 lhs rhs))
      (ld-u8-op2 r args locals and lhs rhs))
    ((ld-expr r args locals (or 1 lhs rhs))
      (ld-u8-op2 r args locals or lhs rhs))
    ((ld-expr r args locals (xor 1 lhs rhs))
      (ld-u8-op2 r args locals xor lhs rhs))

    ; 8-bit mul
    ((ld-expr de args locals (mul 1 lhs rhs))
      (ld-expr e args locals rhs)
      (push de)
      (ld-expr a args locals lhs)
      (pop de)
      (ld d a)
      (mul d e))

    ((ld-expr rr args locals (mul 1 lhs rhs))
      (ld-expr de args locals (mul 1 lhs rhs))
      (ld rr de))

    ; 16-bit
    ((ld-expr hl args locals (peek-nn 2 nn))
      (ld hl (nn)))

    ((ld-expr rr args locals (peek-nn 2 nn))
      (ld-expr hl args locals (peek-nn 2 nn))
      (ld rr hl))

    ((ld-expr de args locals (peek 2 lhs))
      (ld-expr hl args locals (2 lhs))
      (ld e (hl))
      (inc hl)
      (ld d (hl)))

    ((ld-expr rr args locals (peek 2 lhs))
      (ld-expr de args locals (peek 2 lhs))
      (ld rr de))

    ((ld-expr hl args locals (peek-offset 2 n))
      (ld-expr l args locals (peek-offset 1 n))
      (ld-expr h args locals (peek-offset 1 (+ n 1))))

    ((ld-expr de args locals (peek-offset 2 n))
      (ld-expr e args locals (peek-offset 1 n))
      (ld-expr d args locals (peek-offset 1 (+ n 1))))

    ((ld-expr ehl args locals (peek-offset 3 n))
      (ld-expr l args locals (peek-offset 1 n))
      (ld-expr h args locals (peek-offset 1 (+ n 1)))
      (ld-expr e args locals (peek-offset 1 (+ n 2))))

    ((ld-expr dehl args locals (peek-offset 4 n))
      (ld-expr l args locals (peek-offset 1 n))
      (ld-expr h args locals (peek-offset 1 (+ n 1)))
      (ld-expr e args locals (peek-offset 1 (+ n 2)))
      (ld-expr d args locals (peek-offset 1 (+ n 3))))

    ((ld-u16-op1 rr args locals op lhs)
      (ld-expr rr args locals lhs)
      (op rr))

    ((ld-expr rr args locals (inc 2 lhs))
      (ld-u16-op1 rr args locals inc lhs))

    ((ld-expr rr args locals (dec 2 lhs))
      (ld-u16-op1 rr args locals dec lhs))

    ((ld-expr hl args locals (add 2 lhs rhs))
      (ld-expr rr args locals rhs)
      (push hl)
      (ld-expr rr args locals lhs)
      (pop de)
      (add hl de))

    ((ld-expr rr args locals (add 2 lhs rhs))
      (ld-expr hl args locals (add 2 lhs rhs))
      (ld rr hl))

    ((ld-expr hl args locals (sub 2 lhs rhs))
      (ld-expr rr args locals rhs)
      (push hl)
      (ld-expr rr args locals lhs)
      (pop de)
      (rcf)
      (sbc hl de))

    ((ld-expr rr args locals (sub 2 lhs rhs))
      (ld-expr hl args locals (sub 2 lhs rhs))
      (ld rr hl))

    ((ld-expr hl args locals (peek-nn 2 nn))
      (ld hl (nn)))

    ((ld-expr rr args locals (peek-nn 2 nn))
      (ld hl (nn))
      (ld rr hl))

    ; Conditionals
    ((ld-expr r args locals (if (zero? 1 lhs) then-body else-body))
      (ld-expr a args locals lhs)
      (or a)
      (if z
        (ld-expr r args locals then-body)
        (ld-expr r args locals else-body)))

    ((ld-expr r args locals (if (eq? 1 lhs rhs) then-body else-body))
      (ld-expr l args locals lhs)
      (push hl)
      (ld-expr a args locals rhs)
      (pop hl)
      (xor l)
      (if z
        (ld-expr r args locals then-body)
        (ld-expr r args locals else-body)))

    ((ld-expr r args locals (if (gt? 1 lhs rhs) then-body else-body))
      (ld-expr l args locals lhs)
      (push hl)
      (ld-expr a args locals rhs)
      (pop hl)
      (cp l)
      (if c
        (ld-expr r args locals then-body)
        (ld-expr r args locals else-body)))

    ; Locals
    ((with-locals body ...)
      (preserve (ix)
        (ld ix 0)
        (add ix sp)
        body ...))

    ((ld-expr r args locals (local size n))
      (ld-local r args locals 0 size n))

    ((ld-expr r args locals (arg size n))
      (ld-arg r args locals 0 size n))
  )

  (define-op-syntax ld-local
    (lambda ($syntax)
      (syntax-case $syntax (de gl ehl dehl)
        ((ld-local r args locals offset size 0)
          #`(ld-expr r args locals (peek-offset size #,(- (datum offset) (datum size)))))

        ((ld-local r args (local-size . locals) offset size n)
          #`(ld-local r args locals #,(- (datum offset) (datum local-size)) size #,(- (datum n) 1))))))

  (define-op-syntax ld-arg
    (lambda ($syntax)
      (syntax-case $syntax (de gl ehl dehl)
        ((ld-arg r args locals offset size 0)
          #`(ld-expr r args locals (peek-offset size #,(+ (datum offset) 4))))

        ((ld-arg r (arg-size . args) locals offset size n)
          #`(ld-arg r args locals #,(+ (datum offset) (datum arg-size)) size #,(- (datum n) 1))))))
)
