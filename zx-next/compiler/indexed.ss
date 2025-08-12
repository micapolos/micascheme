(library (zx-next compiler indexed)
  (export
    ld-indexed
    peek-const peek peek-offset const
    add-const sub-const and-const or-const xor-const
    lets local arg
    drop native
    zero? eq? gt?
    void ignore
    indexed)
  (import
    (zx-next core)
    (only (zx-next call-frame) call-frame)
    (only (micascheme) -))
  (export (import (only (zx-next call-frame) call-frame)))

  (comment
    unsafe (not type checked) language
    with indexed access to arguments and locals
    and support for values of size from 0 to 4
    (TODO
      (extract part of this language which does not use locals to indexed-direct)
      (or rename this one to indexed-call-frame))
    (TODO
      (extract part of this language which implements sdcc-1 calling convention)
      (to a separate language))
    (TODO
      (stack can contain locals and other temporary values)
      (so we need to adjust locals with every native push/pop))
    (TODO
      (optimize access to last local using push/pop)))

  (define-keywords
    peek-const peek peek-offset
    add-const sub-const and-const or-const xor-const
    lets local arg const
    zero? eq? gt?
    drop native
    void ignore)

  (define-ops
    (keywords
      a de hl lde hlde
      begin
      const
      neg cpl
      inc dec
      add sub and or xor
      add-const sub-const and-const or-const xor-const
      mul
      peek-const peek peek-offset
      lets local arg
      call-frame
      push pop drop native
      void ignore
      zero? eq? gt?)

    ; Top-level
    ((indexed x ...) (ld-indexed void 0 x) ...)

    ((ld-indexed r size x) (ld-indexed r () () size x))

    ; Native
    ((ld-indexed r args locals size (native x ...)) (begin x ...))

    ; Push/pop
    ((ld-indexed void args locals 0 (push x)) (ld-indexed void args locals 0 x))
    ((ld-indexed void args locals 1 (push x)) (ld-indexed a    args locals 1 x) (push a))
    ((ld-indexed void args locals 2 (push x)) (ld-indexed hl   args locals 2 x) (push hl))
    ((ld-indexed void args locals 3 (push x)) (ld-indexed lde  args locals 3 x) (push lde))
    ((ld-indexed void args locals 4 (push x)) (ld-indexed hlde args locals 4 x) (push hlde))

    ((ld-indexed void args locals 0 (pop)))
    ((ld-indexed r    args locals 1 (pop))   (pop r))
    ((ld-indexed rr   args locals 2 (pop))   (pop rr))
    ((ld-indexed rrr  args locals 3 (pop))   (pop rrr))
    ((ld-indexed rrrr args locals 4 (pop))   (pop rrrr))

    ((ld-indexed void args locals 0 (drop)))
    ((ld-indexed void args locals 1 (drop))   (inc sp))
    ((ld-indexed void args locals 2 (drop))   (inc sp) (inc sp))
    ((ld-indexed void args locals 3 (drop))   (inc sp) (inc sp) (inc sp))
    ((ld-indexed void args locals 4 (drop))   (inc sp) (inc sp) (inc sp) (inc sp))

    ; Ignore
    ((ld-indexed void args locals 1 (ignore x)) (ld-indexed a args locals 1 x))
    ((ld-indexed void args locals 2 (ignore x)) (ld-indexed hl args locals 2 x))
    ((ld-indexed void args locals 3 (ignore x)) (ld-indexed ehl args locals 3 x))
    ((ld-indexed void args locals 4 (ignore x)) (ld-indexed dehl args locals 4 x))

    ; Load into register
    ((ld-indexed r args locals size (const n)) (ld r n))

    ; Load indirect
    ((ld-indexed a args locals 1 (peek-const nn))
      (ld a (nn)))

    ((ld-indexed r args locals 1 (peek-const nn))
      (ld a (nn))
      (ld r a))

    ((ld-indexed r args locals 1 (peek 2 lhs))
      (ld-indexed hl args locals 2 lhs)
      (ld r (hl)))

    ((ld-indexed r args locals 1 (peek-offset offset))
      (ld r (+ ix offset)))

    ; 8-bit increment/decrement
    ((ld-u8-op1 r args locals op lhs)
      (ld-indexed r args locals 1 lhs)
      (op r))

    ((ld-indexed r args locals 1 (inc lhs))
      (ld-u8-op1 r args locals inc lhs))

    ((ld-indexed r args locals 1 (dec lhs))
      (ld-u8-op1 r args locals dec lhs))

    ; 8-bit math with constant
    ((ld-u8-op2-const a args locals op lhs n)
      (ld-indexed a args locals 1 lhs)
      (op n))

    ((ld-u8-op2-const r args locals op lhs n)
      (ld-u8-op2-const a args locals op lhs n)
      (ld r a))

    ((ld-indexed r args locals 1 (add-const lhs n))
      (ld-u8-op2-const r args locals add lhs n))
    ((ld-indexed r args locals 1 (sub-const lhs n))
      (ld-u8-op2-const r args locals sub lhs n))
    ((ld-indexed r args locals 1 (and-const lhs n))
      (ld-u8-op2-const r args locals and lhs n))
    ((ld-indexed r args locals 1 (or-const lhs n))
      (ld-u8-op2-const r args locals or lhs n))
    ((ld-indexed r args locals 1 (xor-const lhs n))
      (ld-u8-op2-const r args locals xor lhs n))

    ; 8-bit math
    ((ld-indexed a args locals 1 (neg lhs))
      (ld-indexed a args locals 1 lhs)
      (neg))

    ((ld-indexed r args locals 1 (neg lhs))
      (ld-indexed a args locals 1 (neg lhs))
      (ld r a))

    ((ld-indexed a args locals 1 (cpl lhs))
      (ld-indexed a args locals 1 lhs)
      (cpl))

    ((ld-indexed r args locals 1 (cpl lhs))
      (ld-indexed a args locals 1 (cpl lhs))
      (ld r a))

    ((ld-u8-op2 a args locals op lhs rhs)
      (ld-indexed l args locals 1 rhs)
      (push hl)
      (ld-indexed a args locals 1 lhs)
      (pop hl)
      (op l))

    ((ld-u8-op2 r args locals op lhs rhs)
      (ld-u8-op2 a args locals op lhs rhs)
      (ld r a))

    ((ld-indexed r args locals 1 (add lhs rhs))
      (ld-u8-op2 r args locals add lhs rhs))
    ((ld-indexed r args locals 1 (sub lhs rhs))
      (ld-u8-op2 r args locals sub lhs rhs))
    ((ld-indexed r args locals 1 (and lhs rhs))
      (ld-u8-op2 r args locals and lhs rhs))
    ((ld-indexed r args locals 1 (or lhs rhs))
      (ld-u8-op2 r args locals or lhs rhs))
    ((ld-indexed r args locals 1 (xor lhs rhs))
      (ld-u8-op2 r args locals xor lhs rhs))

    ; 8-bit mul
    ((ld-indexed de args locals 2 (mul lhs rhs))
      (ld-indexed e args locals 1 rhs)
      (push de)
      (ld-indexed a args locals 1 lhs)
      (pop de)
      (ld d a)
      (mul d e))

    ((ld-indexed rr args locals 1 (mul lhs rhs))
      (ld-indexed de args locals 1 (mul lhs rhs))
      (ld rr de))

    ; 16-bit
    ((ld-indexed hl args locals 2 (peek-const nn))
      (ld hl (nn)))

    ((ld-indexed rr args locals 2 (peek-const nn))
      (ld-indexed hl args locals 2 (peek-const nn))
      (ld rr hl))

    ((ld-indexed de args locals 2 (peek 2 lhs))
      (ld-indexed hl args locals 2 (const lhs))
      (ld e (hl))
      (inc hl)
      (ld d (hl)))

    ((ld-indexed rr args locals 2 (peek 2 lhs))
      (ld-indexed de args locals 2 (peek 2 lhs))
      (ld rr de))

    ((ld-indexed hl args locals 2 (peek-offset n))
      (ld-indexed l args locals 1 (peek-offset n))
      (ld-indexed h args locals 1 (peek-offset (+ n 1))))

    ((ld-indexed de args locals 2 (peek-offset n))
      (ld-indexed e args locals 1 (peek-offset n))
      (ld-indexed d args locals 1 (peek-offset (+ n 1))))

    ((ld-indexed ehl args locals 3 (peek-offset n))
      (ld-indexed hl args locals 2 (peek-offset n))
      (ld-indexed e args locals 1 (peek-offset (+ n 2))))

    ((ld-indexed dehl args locals 4 (peek-offset n))
      (ld-indexed hl args locals 1 (peek-offset n))
      (ld-indexed de args locals 1 (peek-offset (+ n 2))))

    ((ld-u16-op1 rr args locals op lhs)
      (ld-indexed rr args locals 2 lhs)
      (op rr))

    ((ld-indexed rr args locals 2 (inc lhs))
      (ld-u16-op1 rr args locals inc lhs))

    ((ld-indexed rr args locals 2 (dec lhs))
      (ld-u16-op1 rr args locals dec lhs))

    ((ld-indexed hl args locals 2 (add lhs rhs))
      (ld-indexed rr args locals 2 rhs)
      (push hl)
      (ld-indexed rr args locals 2 lhs)
      (pop de)
      (add hl de))

    ((ld-indexed rr args locals 2 (add lhs rhs))
      (ld-indexed hl args locals 2 (add lhs rhs))
      (ld rr hl))

    ((ld-indexed hl args locals 2 (sub lhs rhs))
      (ld-indexed rr args locals 2 rhs)
      (push hl)
      (ld-indexed rr args locals 2 lhs)
      (pop de)
      (rcf)
      (sbc hl de))

    ((ld-indexed rr args locals 2 (sub lhs rhs))
      (ld-indexed hl args locals 2 (sub lhs rhs))
      (ld rr hl))

    ((ld-indexed hl args locals 2 (peek-const nn))
      (ld hl (nn)))

    ((ld-indexed rr args locals 2 (peek-const nn))
      (ld hl (nn))
      (ld rr hl))

    ; Conditionals
    ((ld-indexed r args locals size (if 1 (zero? lhs) then-body else-body))
      (ld-indexed a args locals 1 lhs)
      (or a)
      (if z
        (ld-indexed r args locals size then-body)
        (ld-indexed r args locals size else-body)))

    ((ld-indexed r args locals size (if 1 (eq? lhs rhs) then-body else-body))
      (ld-indexed l args locals 1 lhs)
      (push hl)
      (ld-indexed a args locals 1 rhs)
      (pop hl)
      (xor l)
      (if z
        (ld-indexed r args locals size then-body)
        (ld-indexed r args locals size else-body)))

    ((ld-indexed r args locals size (if 1 (gt? lhs rhs) then-body else-body))
      (ld-indexed l args locals 1 lhs)
      (push hl)
      (ld-indexed a args locals 1 rhs)
      (pop hl)
      (cp l)
      (if c
        (ld-indexed r args locals size then-body)
        (ld-indexed r args locals size else-body)))

    ; Locals
    ((ld-indexed r args locals size (call-frame body ...))
      (call-frame
        (ld-indexed r args locals size (begin body ...))))

    ; Lets
    ((ld-indexed r args locals size (lets x))
      (ld-indexed r args locals size x))

    ((ld-indexed r args (loc ...) size (lets (arg-size arg-indexed) . x))
      (begin
        (ld-indexed void args locals arg-size (push arg-indexed))
        (ld-indexed r args (loc ... arg-size) size (lets . x))
        (ld-indexed void args locals arg-size (drop))))

    ; Local access
    ((ld-indexed r args locals size (local n))
      (ld-local r args locals 0 size n))

    ; Argument access
    ((ld-indexed r args locals size (arg n))
      (ld-arg r args locals 0 size n))

    ; Begin block
    ((ld-indexed r args locals size (begin stmt ... indexed))
      (ld-indexed void args locals 0 stmt) ...
      (ld-indexed r args locals size indexed))

    ; Everything else is a call, where all arguments ar pushed on the stack
    ; Use SDCC-1 calling convention.
    ((ld-indexed r args locals size (addr (4 x) xs ...))
      (ld-indexed hlde args locals 4 x)
      (ld-indexed r args locals size
        (lets xs ... (native (call addr)))))

    ((ld-indexed r args locals size (addr (3 x) xs ...))
      (ld-indexed lde args locals 3 x)
      (ld-indexed r args locals size
        (lets xs ... (native (call addr)))))

    ((ld-indexed r args locals size (addr (2 x) (2 y) xs ...))
      (ld-indexed de args locals 2 y)
      (preserve (de) (ld-indexed hl args locals 2 x))
      (ld-indexed r args locals size
        (lets xs ... (native (call addr)))))

    ((ld-indexed r args locals size (addr (2 x) xs ...))
      (ld-indexed hl args locals 2 x)
      (ld-indexed r args locals size
        (lets xs ... (native (call addr)))))

    ((ld-indexed r args locals size (addr (1 x) (2 y) xs ...))
      (ld-indexed hl args locals 2 y)
      (preserve (hl) (ld-indexed a args locals 1 x))
      (ld-indexed r args locals size
        (lets xs ... (native (call addr)))))

    ((ld-indexed r args locals size (addr (1 x) (1 y) xs ...))
      (ld-indexed l args locals 1 y)
      (preserve (hl) (ld-indexed a args locals 1 x))
      (ld-indexed r args locals size
        (lets xs ... (native (call addr)))))

    ((ld-indexed r args locals size (addr (1 x) xs ...))
      (ld-indexed a args locals 1 x)
      (ld-indexed r args locals size
        (lets xs ... (native (call addr)))))

    ((ld-indexed r args locals size (addr xs ...))
      (ld-indexed r args locals size
        (lets xs ... (native (call addr)))))
  )

  (define-op-syntax ld-local
    (lambda ($syntax)
      (syntax-case $syntax (de gl ehl dehl)
        ((ld-local r args locals offset size 0)
          #`(ld-indexed r args locals size (peek-offset #,(- (datum offset) (datum size)))))

        ((ld-local r args (local-size . locals) offset size n)
          #`(ld-local r args locals #,(- (datum offset) (datum local-size)) size #,(- (datum n) 1))))))

  (define-op-syntax ld-arg
    (lambda ($syntax)
      (syntax-case $syntax (de gl ehl dehl)
        ((ld-arg r args locals offset size 0)
          #`(ld-indexed r args locals size (peek-offset #,(+ (datum offset) 4))))

        ((ld-arg r (arg-size . args) locals offset size n)
          #`(ld-arg r args locals #,(+ (datum offset) (datum arg-size)) size #,(- (datum n) 1))))))
)
