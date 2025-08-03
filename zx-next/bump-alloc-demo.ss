(import
  (zx-next demo)
  (zx-next bump-alloc))

(demo
  (ld hl #x2000)
  (ld bc #x0124)
  (ld e #b11100000)
  (preserve (bc de hl) (writeln "alloc " hl ", " bc ", " e))
  (call bump-alloc)
  (if c
    (then (writeln-error "out of memory " hl))
    (else (writeln-ok "allocated " hl ", " de)))
  (writeln)

  (ld hl #x2000)
  (ld bc #x1ffe)
  (ld e #b11100000)
  (preserve (bc de hl) (writeln "alloc " hl ", " bc ", " e))
  (call bump-alloc)
  (if c
    (then (writeln-error "out of memory " hl))
    (else (writeln-ok "allocated " hl ", " de)))
  (writeln)

  (ld hl #x2000)
  (ld bc #x1fff)
  (ld e #b11100000)
  (preserve (bc de hl) (writeln "alloc " hl ", " bc ", " e))
  (call bump-alloc)
  (if c
    (then (writeln-ok "out of memory " hl))
    (else (writeln-error "allocated " hl ", " de))))
