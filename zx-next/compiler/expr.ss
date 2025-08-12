(library (zx-next compiler expr)
  (export
    u8 u16 u24 u32
    ld-expr
    u8+1 u8-1
    u8-neg u8-not
    u8+n u8-n u8-and-n u8-or-n u8-xor-n
    u8+ u8- u8-and u8-or u8-xor u8-mul
    u8-peek-nn u8-peek-offset u8-peek
    u8-zero? u8=? u8>
    u16+1 u16-1 u16+ u16-
    u16-peek-nn
    with-locals lets local arg)
  (import
    (zx-next core)
    (only (micascheme) -))

  (define-keywords
    u8 u16 u24 u32
    u8-neg u8-not
    u8+1 u8-1 u8+ u8- u8-and u8-or u8-xor u8-mul
    u8+n u8-n u8-and-n u8-or-n u8-xor-n
    u8-peek-nn u8-peek-offset u8-peek
    u8-zero? u8=? u8>
    u16+1 u16-1 u16+ u16-
    u16-peek-nn
    lets local arg)

  (define-ops
    (keywords
      a de hl ehl dehl
      u8 u16 u24 u32
      u8-neg u8-not
      u8+1 u8-1 u8+ u8- u8-and u8-or u8-xor u8-mul
      u8+n u8-n u8-and-n u8-or-n u8-xor-n
      u8-peek-nn u8-peek-offset u8-peek
      u8-zero? u8=? u8>
      u16+1 u16-1 u16+ u16-
      u16-peek-nn
      lets local arg)

    ; Top-level
    ((ld-expr r x) (ld-expr r () () x))

    ; Load
    ((ld-expr r    args locals (u8 n))     (ld r n))
    ((ld-expr rr   args locals (u16 nn))   (ld rr nn))
    ((ld-expr ehl  args locals (u24 nnn))  (ld ehl nnn))
    ((ld-expr dehl args locals (u32 nnnn)) (ld dehl nnnn))

    ; Load indirect
    ((ld-expr a args locals (u8-peek-nn nn))
      (ld a (nn)))

    ((ld-expr r args locals (u8-peek-nn nn))
      (ld a (nn))
      (ld r a))

    ((ld-expr r args locals (u8-peek lhs))
      (ld-expr hl args locals lhs)
      (ld r (hl)))

    ((ld-expr r args locals (u8-peek-offset offset))
      (ld r (+ ix offset)))

    ; 8-bit increment/decrement
    ((ld-u8-op1 r args locals op lhs)
      (ld-expr r args locals lhs)
      (op r))

    ((ld-expr r args locals (u8+1 lhs))
      (ld-u8-op1 r args locals inc lhs))

    ((ld-expr r args locals (u8-1 lhs))
      (ld-u8-op1 r args locals dec lhs))

    ; 8-bit math with constant
    ((ld-u8-op2-n a args locals op lhs n)
      (ld-expr a args locals lhs)
      (op n))

    ((ld-u8-op2-n r args locals op lhs n)
      (ld-u8-op2-n a args locals op lhs n)
      (ld r a))

    ((ld-expr r args locals (u8+n lhs n))
      (ld-u8-op2-n r args locals add lhs n))
    ((ld-expr r args locals (u8-n lhs n))
      (ld-u8-op2-n r args locals sub lhs n))
    ((ld-expr r args locals (u8-and-n lhs n))
      (ld-u8-op2-n r args locals and lhs n))
    ((ld-expr r args locals (u8-or-n lhs n))
      (ld-u8-op2-n r args locals or lhs n))
    ((ld-expr r args locals (u8-xor-n lhs n))
      (ld-u8-op2-n r args locals xor lhs n))

    ; 8-bit math
    ((ld-expr a args locals (u8-neg lhs))
      (ld-expr a args locals lhs)
      (neg))

    ((ld-expr r args locals (u8-neg lhs))
      (ld-expr a args locals (u8-neg lhs))
      (ld r a))

    ((ld-expr a args locals (u8-not lhs))
      (ld-expr a args locals lhs)
      (cpl))

    ((ld-expr r args locals (u8-not lhs))
      (ld-expr a args locals (u8-not lhs))
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

    ((ld-expr r args locals (u8+ lhs rhs))
      (ld-u8-op2 r args locals add lhs rhs))
    ((ld-expr r args locals (u8- lhs rhs))
      (ld-u8-op2 r args locals sub lhs rhs))
    ((ld-expr r args locals (u8-and lhs rhs))
      (ld-u8-op2 r args locals and lhs rhs))
    ((ld-expr r args locals (u8-or lhs rhs))
      (ld-u8-op2 r args locals or lhs rhs))
    ((ld-expr r args locals (u8-xor lhs rhs))
      (ld-u8-op2 r args locals xor lhs rhs))

    ; 8-bit mul
    ((ld-expr de args locals (u8-mul lhs rhs))
      (ld-expr e args locals rhs)
      (push de)
      (ld-expr a args locals lhs)
      (pop de)
      (ld d a)
      (mul d e))

    ((ld-expr rr args locals (u8-mul lhs rhs))
      (ld-expr de args locals (u8-mul lhs rhs))
      (ld rr de))

    ((ld-u16-op1 rr args locals op lhs)
      (ld-expr rr args locals lhs)
      (op rr))

    ((ld-expr rr args locals (u16+1 lhs))
      (ld-u16-op1 rr args locals inc lhs))

    ((ld-expr rr args locals (u16-1 lhs))
      (ld-u16-op1 rr args locals dec lhs))

    ((ld-expr hl args locals (u16+ lhs rhs))
      (ld-expr rr args locals rhs)
      (push hl)
      (ld-expr rr args locals lhs)
      (pop de)
      (add hl de))

    ((ld-expr rr args locals (u16+ lhs rhs))
      (ld-expr hl args locals (u16+ lhs rhs))
      (ld rr hl))

    ((ld-expr hl args locals (u16- lhs rhs))
      (ld-expr rr args locals rhs)
      (push hl)
      (ld-expr rr args locals lhs)
      (pop de)
      (rcf)
      (sbc hl de))

    ((ld-expr rr args locals (u16- lhs rhs))
      (ld-expr hl args locals (u16- lhs rhs))
      (ld rr hl))

    ((ld-expr hl args locals (u16-peek-nn nn))
      (ld hl (nn)))

    ((ld-expr rr args locals (u16-peek-nn nn))
      (ld hl (nn))
      (ld rr hl))

    ; Conditionals
    ((ld-expr r args locals (if (u8-zero? lhs) then-body else-body))
      (ld-expr a args locals lhs)
      (or a)
      (if z
        (ld-expr r args locals then-body)
        (ld-expr r args locals else-body)))

    ((ld-expr r args locals (if (u8=? lhs rhs) then-body else-body))
      (ld-expr l args locals lhs)
      (push hl)
      (ld-expr a args locals rhs)
      (pop hl)
      (xor l)
      (if z
        (ld-expr r args locals then-body)
        (ld-expr r args locals else-body)))

    ((ld-expr r args locals (if (u8> lhs rhs) then-body else-body))
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

    ((ld-expr r args locals (local type n))
      (ld-local r args locals 0 type n))

    ((ld-expr r args locals (arg type n))
      (ld-arg r args locals 0 type n))
  )

  (define-op-syntax ld-local
    (lambda ($syntax)
      (syntax-case $syntax (de gl ehl dehl u8 u16 u24 u32)
        ((ld-local r args locals offset u8 0)
          #`(ld-expr r (u8-peek-offset #,(- (datum offset) 1))))

        ((ld-local de args locals offset u16 0)
          #`(begin
            (ld-expr d (u8-peek-offset #,(- (datum offset) 1)))
            (ld-expr e (u8-peek-offset #,(- (datum offset) 2)))))

        ((ld-local hl args locals offset u16 0)
          #`(begin
            (ld-expr h (u8-peek-offset #,(- (datum offset) 1)))
            (ld-expr l (u8-peek-offset #,(- (datum offset) 2)))))

        ((ld-local ehl args locals offset u24 0)
          #`(begin
            (ld-expr e (u8-peek-offset #,(- (datum offset) 1)))
            (ld-expr h (u8-peek-offset #,(- (datum offset) 2)))
            (ld-expr l (u8-peek-offset #,(- (datum offset) 3)))))

        ((ld-local dehl args locals offset u32 0)
          #`(begin
            (ld-expr d (u8-peek-offset #,(- (datum offset) 1)))
            (ld-expr e (u8-peek-offset #,(- (datum offset) 2)))
            (ld-expr h (u8-peek-offset #,(- (datum offset) 3)))
            (ld-expr l (u8-peek-offset #,(- (datum offset) 4)))))

        ((ld-local r args (u8 . locals) offset type n)
          #`(ld-local r args locals #,(- (datum offset) 1) type #,(- (datum n) 1)))

        ((ld-local r args (u16 . locals) offset type n)
          #`(ld-local r args locals #,(- (datum offset) 2) type #,(- (datum n) 1)))

        ((ld-local r args (u24 . locals) offset type n)
          #`(ld-local r args locals #,(- (datum offset) 3) type #,(- (datum n) 1)))

        ((ld-local r args (u32 . locals) offset type n)
          #`(ld-local r args locals #,(- (datum offset) 4) type #,(- (datum n) 1))))))

  (define-op-syntax ld-arg
    (lambda ($syntax)
      (syntax-case $syntax (de gl ehl dehl u8 u16 u24 u32)
        ((ld-arg r args locals offset u8 0)
          #`(ld-expr r (u8-peek-offset #,(+ (datum offset) 4))))

        ((ld-arg de args locals offset u16 0)
          #`(begin
            (ld-expr d (u8-peek-offset #,(+ (datum offset) 5)))
            (ld-expr e (u8-peek-offset #,(+ (datum offset) 4)))))

        ((ld-arg hl args locals offset u16 0)
          #`(begin
            (ld-expr h (u8-peek-offset #,(+ (datum offset) 5)))
            (ld-expr l (u8-peek-offset #,(+ (datum offset) 4)))))

        ((ld-arg ehl args locals offset u24 0)
          #`(begin
            (ld-expr e (u8-peek-offset #,(+ (datum offset) 6)))
            (ld-expr h (u8-peek-offset #,(+ (datum offset) 5)))
            (ld-expr l (u8-peek-offset #,(+ (datum offset) 4)))))

        ((ld-arg dehl args locals offset u32 0)
          #`(begin
            (ld-expr d (u8-peek-offset #,(+ (datum offset) 7)))
            (ld-expr e (u8-peek-offset #,(+ (datum offset) 6)))
            (ld-expr h (u8-peek-offset #,(+ (datum offset) 5)))
            (ld-expr l (u8-peek-offset #,(+ (datum offset) 4)))))

        ((ld-arg r (u8 . args) locals offset type n)
          #`(ld-arg r args locals #,(+ (datum offset) 1) type #,(- (datum n) 1)))

        ((ld-arg r (u16 . args) locals offset type n)
          #`(ld-arg r args locals #,(+ (datum offset) 2) type #,(- (datum n) 1)))

        ((ld-arg r (u24 . args) locals offset type n)
          #`(ld-arg r args locals #,(+ (datum offset) 3) type #,(- (datum n) 1)))

        ((ld-arg r (u32 . args) locals offset type n)
          #`(ld-arg r args locals #,(+ (datum offset) 4) type #,(- (datum n) 1))))))
)
