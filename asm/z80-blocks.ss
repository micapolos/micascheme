(library (asm z80-blocks)
  (export
    input output
    loop loop-djnz
    preserve
    if then else
    while
    when unless
    ld-inc dec-ld
    loop-byte loop-word)
  (import
    (asm lang) (asm z80))

  (define-keywords then else while)

  (define-ops (keywords then else while c nc z nz m p po pe)
    ((input body ...))
    ((output body ...))
    ((loop body ... (while cond))
      (with-labels (label)
        label
        body ...
        (jp cond label)))
    ((loop body ...)
      (with-labels (label)
        label
        body ...
        (jp label)))
    ((loop-djnz body ...)
      (with-labels (label)
        label
        body ...
        (djnz label)))
    ((loop-byte n body ...)
      (ld b n)
      (loop-djnz (preserve (bc) body ...)))
    ((loop-word nn body ...)
      (ld bc nn)
      (loop
        (preserve (af)
          (preserve (bc) body ...)
          (ld a b)
          (or c))
        (while nz)))
    ((preserve (reg ...) body ...)
      (push reg) ...
      body ...
      (reverse (pop reg) ...))
    ((unless flag body ...)
      (with-labels (end)
        (jp flag end)
        body ...
        end))
    ((if flag (then then-body ...) (else else-body ...))
      (with-labels (label-then label-end)
        (jp flag label-then)
        else-body ...
        (jp label-end)
        label-then
        then-body ...
        label-end)))

  (define-ops (keywords c nc z nz m p po pe)
    ((when nc body ...) (unless c body ...))
    ((when c body ...) (unless nc body ...))
    ((when z body ...) (unless nz body ...))
    ((when nz body ...) (unless z body ...))
    ((when m body ...) (unless p body ...))
    ((when p body ...) (unless m body ...))
    ((when po body ...) (unless pe body ...))
    ((when pe body ...) (unless po body ...)))

  (define-ops (keywords a b c d e bc de hl)
    ((ld-inc a (hl))   (ld a (hl)) (inc hl))
    ((ld-inc b (hl))   (ld b (hl)) (inc hl))
    ((ld-inc c (hl))   (ld c (hl)) (inc hl))
    ((ld-inc d (hl))   (ld d (hl)) (inc hl))
    ((ld-inc e (hl))   (ld e (hl)) (inc hl))

    ((ld-inc (hl) a)   (ld (hl) a) (inc hl))
    ((ld-inc (hl) b)   (ld (hl) b) (inc hl))
    ((ld-inc (hl) c)   (ld (hl) c) (inc hl))
    ((ld-inc (hl) d)   (ld (hl) d) (inc hl))
    ((ld-inc (hl) e)   (ld (hl) e) (inc hl))

    ((ld-inc bc (hl))  (ld-inc c (hl)) (ld-inc b (hl)))
    ((ld-inc de (hl))  (ld-inc e (hl)) (ld-inc d (hl)))

    ((ld-inc (hl) bc)  (ld-inc (hl) c) (ld-inc (hl) b))
    ((ld-inc (hl) de)  (ld-inc (hl) e) (ld-inc (hl) d))

    ((dec-ld a (hl))   (dec hl) (ld a (hl)))
    ((dec-ld b (hl))   (dec hl) (ld b (hl)))
    ((dec-ld c (hl))   (dec hl) (ld c (hl)))
    ((dec-ld d (hl))   (dec hl) (ld d (hl)))
    ((dec-ld e (hl))   (dec hl) (ld e (hl)))

    ((dec-ld (hl) a)   (dec hl) (ld (hl) a))
    ((dec-ld (hl) b)   (dec hl) (ld (hl) b))
    ((dec-ld (hl) c)   (dec hl) (ld (hl) c))
    ((dec-ld (hl) d)   (dec hl) (ld (hl) d))
    ((dec-ld (hl) e)   (dec hl) (ld (hl) e))

    ((dec-ld bc (hl))  (dec-ld b (hl)) (dec-ld c (hl)))
    ((dec-ld de (hl))  (dec-ld d (hl)) (dec-ld e (hl)))

    ((dec-ld (hl) bc)  (dec-ld (hl) b) (dec-ld (hl) c))
    ((dec-ld (hl) de)  (dec-ld (hl) d) (dec-ld (hl) e)))
)
