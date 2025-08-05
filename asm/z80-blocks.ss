(library (asm z80-blocks)
  (export
    input output
    loop loop-djnz
    preserve
    if then else
    while
    when unless
    ld-inc dec-ld
    loop-byte loop-word
    define-proc define-procs
    ret-c ret-nc
    with
    pop-regs push-regs preserve-regs)
  (import
    (asm lang)
    (asm z80)
    (except (micascheme) with and or xor pop push break exit data define define-values reverse else unless if when))

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
        (preserve (af bc) body ...)
        (dec bc)
        (ld a b)
        (or c)
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

  (define-ops (keywords c nc)
    ((ret-c) (scf) (ret))
    ((ret-nc) (rcf) (ret)))

  (define-ops (keywords ex exx af de hl)
    ((with (exx) body ...)
      (exx)
      body ...
      (exx))
    ((with (ex af) body ...)
      (ex af)
      body ...
      (ex af))
    ((with (ex de hl) body ...)
      (ex de hl)
      body ...
      (ex de hl)))

  (define-ops
    ((pop-regs)
      (pop af)
      (pop hl)
      (pop bc)
      (pop de)
      (exx) (ex af)
      (pop af)
      (pop hl)
      (pop bc)
      (pop de)
      (exx) (ex af)
      (pop ix)
      (pop iy))

    ((push-regs)
      (push iy)
      (push ix)
      (exx) (ex af)
      (push de)
      (push bc)
      (push hl)
      (push af)
      (exx) (ex af)
      (push de)
      (push bc)
      (push hl)
      (push af)))

  (define-op (preserve-regs body ...)
    (push-regs)
    body ...
    (pop-regs))

  (define-syntax (define-proc $syntax)
    (syntax-case $syntax ()
      ((_ (id reg ...) body ...)
        (for-all identifier? #'(id reg ...))
        (lets
          ($proc (identifier-append #'id #'id #'- #'proc))
          ($tc (identifier-append #'id #'id #'- #'tc))
          ($call #`(call #,$proc))
          ($jp #`(jp #,$proc))
          ($tmps (generate-temporaries #'(reg ...)))
          ($lds (map-with ($reg #'(reg ...)) ($tmp $tmps) #`(ld #,$reg #,$tmp)))
          #`(begin
            (define-fragment #,$proc body ...)
            (define-ops (keywords reg ...)
              ((id reg ...) #,$call)
              ((#,$tc reg ...) #,$jp)
              ((id #,@$tmps) #,@$lds #,$call)
              ((#,$tc #,@$tmps) #,@$lds #,$jp)))))))

  (define-rule-syntax (define-procs (id . x) ...)
    (begin (define-proc id . x) ...))
)
