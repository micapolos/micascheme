(library (zx-next compiler expr)
  (export
    ld-expr
    peek-const peek peek-offset const
    add-const sub-const and-const or-const xor-const
    with-locals lets local arg
    zero? eq? gt?)
  (import
    (zx-next core)
    (only (micascheme) -))

  (define-keywords
    peek-const peek peek-offset
    add-const sub-const and-const or-const xor-const
    lets local arg const
    zero? eq? gt?)

  (define-ops
    (keywords
      a de hl ehl dehl
      const
      neg cpl
      inc dec
      add sub and or xor
      add-const sub-const and-const or-const xor-const
      mul
      peek-const peek peek-offset
      lets local arg
      zero? eq? gt?)

    ; Top-level
    ((ld-expr r size x) (ld-expr r () () size x))

    ; Load
    ((ld-expr r args locals size (const n)) (ld r n))

    ; Load indirect
    ((ld-expr a args locals 1 (peek-const nn))
      (ld a (nn)))

    ((ld-expr r args locals 1 (peek-const nn))
      (ld a (nn))
      (ld r a))

    ((ld-expr r args locals 1 (peek 2 lhs))
      (ld-expr hl args locals 2 lhs)
      (ld r (hl)))

    ((ld-expr r args locals 1 (peek-offset offset))
      (ld r (+ ix offset)))

    ; 8-bit increment/decrement
    ((ld-u8-op1 r args locals op lhs)
      (ld-expr r args locals 1 lhs)
      (op r))

    ((ld-expr r args locals 1 (inc lhs))
      (ld-u8-op1 r args locals inc lhs))

    ((ld-expr r args locals 1 (dec lhs))
      (ld-u8-op1 r args locals dec lhs))

    ; 8-bit math with constant
    ((ld-u8-op2-const a args locals op lhs n)
      (ld-expr a args locals 1 lhs)
      (op n))

    ((ld-u8-op2-const r args locals op lhs n)
      (ld-u8-op2-const a args locals op lhs n)
      (ld r a))

    ((ld-expr r args locals 1 (add-const lhs n))
      (ld-u8-op2-const r args locals add lhs n))
    ((ld-expr r args locals 1 (sub-const lhs n))
      (ld-u8-op2-const r args locals sub lhs n))
    ((ld-expr r args locals 1 (and-const lhs n))
      (ld-u8-op2-const r args locals and lhs n))
    ((ld-expr r args locals 1 (or-const lhs n))
      (ld-u8-op2-const r args locals or lhs n))
    ((ld-expr r args locals 1 (xor-const lhs n))
      (ld-u8-op2-const r args locals xor lhs n))

    ; 8-bit math
    ((ld-expr a args locals 1 (neg lhs))
      (ld-expr a args locals 1 lhs)
      (neg))

    ((ld-expr r args locals 1 (neg lhs))
      (ld-expr a args locals 1 (neg lhs))
      (ld r a))

    ((ld-expr a args locals 1 (cpl lhs))
      (ld-expr a args locals 1 lhs)
      (cpl))

    ((ld-expr r args locals 1 (cpl lhs))
      (ld-expr a args locals 1 (cpl lhs))
      (ld r a))

    ((ld-u8-op2 a args locals op lhs rhs)
      (ld-expr l args locals 1 rhs)
      (push hl)
      (ld-expr a args locals 1 lhs)
      (pop hl)
      (op l))

    ((ld-u8-op2 r args locals op lhs rhs)
      (ld-u8-op2 a args locals op lhs rhs)
      (ld r a))

    ((ld-expr r args locals 1 (add lhs rhs))
      (ld-u8-op2 r args locals add lhs rhs))
    ((ld-expr r args locals 1 (sub lhs rhs))
      (ld-u8-op2 r args locals sub lhs rhs))
    ((ld-expr r args locals 1 (and lhs rhs))
      (ld-u8-op2 r args locals and lhs rhs))
    ((ld-expr r args locals 1 (or lhs rhs))
      (ld-u8-op2 r args locals or lhs rhs))
    ((ld-expr r args locals 1 (xor lhs rhs))
      (ld-u8-op2 r args locals xor lhs rhs))

    ; 8-bit mul
    ((ld-expr de args locals 2 (mul lhs rhs))
      (ld-expr e args locals 1 rhs)
      (push de)
      (ld-expr a args locals 1 lhs)
      (pop de)
      (ld d a)
      (mul d e))

    ((ld-expr rr args locals 1 (mul lhs rhs))
      (ld-expr de args locals 1 (mul lhs rhs))
      (ld rr de))

    ; 16-bit
    ((ld-expr hl args locals 2 (peek-const nn))
      (ld hl (nn)))

    ((ld-expr rr args locals 2 (peek-const nn))
      (ld-expr hl args locals 2 (peek-const nn))
      (ld rr hl))

    ((ld-expr de args locals 2 (peek 2 lhs))
      (ld-expr hl args locals 2 (const lhs))
      (ld e (hl))
      (inc hl)
      (ld d (hl)))

    ((ld-expr rr args locals 2 (peek 2 lhs))
      (ld-expr de args locals 2 (peek 2 lhs))
      (ld rr de))

    ((ld-expr hl args locals 2 (peek-offset n))
      (ld-expr l args locals 1 (peek-offset n))
      (ld-expr h args locals 1 (peek-offset (+ n 1))))

    ((ld-expr de args locals 2 (peek-offset n))
      (ld-expr e args locals 1 (peek-offset n))
      (ld-expr d args locals 1 (peek-offset (+ n 1))))

    ((ld-expr ehl args locals 3 (peek-offset n))
      (ld-expr l args locals 1 (peek-offset n))
      (ld-expr h args locals 1 (peek-offset (+ n 1)))
      (ld-expr e args locals 1 (peek-offset (+ n 2))))

    ((ld-expr dehl args locals 4 (peek-offset n))
      (ld-expr l args locals 1 (peek-offset n))
      (ld-expr h args locals 1 (peek-offset (+ n 1)))
      (ld-expr e args locals 1 (peek-offset (+ n 2)))
      (ld-expr d args locals 1 (peek-offset (+ n 3))))

    ((ld-u16-op1 rr args locals op lhs)
      (ld-expr rr args locals 2 lhs)
      (op rr))

    ((ld-expr rr args locals 2 (inc lhs))
      (ld-u16-op1 rr args locals inc lhs))

    ((ld-expr rr args locals 2 (dec lhs))
      (ld-u16-op1 rr args locals dec lhs))

    ((ld-expr hl args locals 2 (add lhs rhs))
      (ld-expr rr args locals 2 rhs)
      (push hl)
      (ld-expr rr args locals 2 lhs)
      (pop de)
      (add hl de))

    ((ld-expr rr args locals 2 (add lhs rhs))
      (ld-expr hl args locals 2 (add lhs rhs))
      (ld rr hl))

    ((ld-expr hl args locals 2 (sub lhs rhs))
      (ld-expr rr args locals 2 rhs)
      (push hl)
      (ld-expr rr args locals 2 lhs)
      (pop de)
      (rcf)
      (sbc hl de))

    ((ld-expr rr args locals 2 (sub lhs rhs))
      (ld-expr hl args locals 2 (sub lhs rhs))
      (ld rr hl))

    ((ld-expr hl args locals 2 (peek-const nn))
      (ld hl (nn)))

    ((ld-expr rr args locals 2 (peek-const nn))
      (ld hl (nn))
      (ld rr hl))

    ; Conditionals
    ((ld-expr r args locals size (if 1 (zero? lhs) then-body else-body))
      (ld-expr a args locals 1 lhs)
      (or a)
      (if z
        (ld-expr r args locals size then-body)
        (ld-expr r args locals size else-body)))

    ((ld-expr r args locals size (if 1 (eq? lhs rhs) then-body else-body))
      (ld-expr l args locals 1 lhs)
      (push hl)
      (ld-expr a args locals 1 rhs)
      (pop hl)
      (xor l)
      (if z
        (ld-expr r args locals size then-body)
        (ld-expr r args locals size else-body)))

    ((ld-expr r args locals size (if 1 (gt? lhs rhs) then-body else-body))
      (ld-expr l args locals 1 lhs)
      (push hl)
      (ld-expr a args locals 1 rhs)
      (pop hl)
      (cp l)
      (if c
        (ld-expr r args locals size then-body)
        (ld-expr r args locals size else-body)))

    ; Locals
    ((with-locals body ...)
      (preserve (ix)
        (ld ix 0)
        (add ix sp)
        body ...))

    ((ld-expr r args locals size (local n))
      (ld-local r args locals 0 size n))

    ((ld-expr r args locals size (arg n))
      (ld-arg r args locals 0 size n))
  )

  (define-op-syntax ld-local
    (lambda ($syntax)
      (syntax-case $syntax (de gl ehl dehl)
        ((ld-local r args locals offset size 0)
          #`(ld-expr r args locals size (peek-offset #,(- (datum offset) (datum size)))))

        ((ld-local r args (local-size . locals) offset size n)
          #`(ld-local r args locals #,(- (datum offset) (datum local-size)) size #,(- (datum n) 1))))))

  (define-op-syntax ld-arg
    (lambda ($syntax)
      (syntax-case $syntax (de gl ehl dehl)
        ((ld-arg r args locals offset size 0)
          #`(ld-expr r args locals size (peek-offset #,(+ (datum offset) 4))))

        ((ld-arg r (arg-size . args) locals offset size n)
          #`(ld-arg r args locals #,(+ (datum offset) (datum arg-size)) size #,(- (datum n) 1))))))
)
