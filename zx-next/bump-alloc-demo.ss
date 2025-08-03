(import
  (zx-next demo)
  (zx-next bump-alloc))

(demo
  (ld hl #xe000)
  (ld bc #x00fe)
  (ld d #b10100000) ; tag
  (ld e #b11100000) ; bank
  (preserve (bc de hl) (writeln "alloc " hl #\space bc #\space d #\space e))
  (call bump-alloc)
  (preserve (hl)
    (if c
      (then (writeln-error "out of memory " hl))
      (else (writeln-ok "allocated " hl #\space de)))
    (writeln))

  (ld bc #x00fe)
  (ld d #b10100000) ; tag
  (ld e #b11100000) ; bank
  (preserve (bc de hl) (writeln "alloc " hl #\space bc #\space d #\space e))
  (call bump-alloc)
  (preserve (hl)
    (if c
      (then (writeln-error "out of memory " hl))
      (else (writeln-ok "allocated " hl #\space de)))
    (writeln))

  (ld bc #x1dff)
  (ld d #b10100000) ; tag
  (ld e #b11100000) ; bank
  (preserve (bc de hl) (writeln "alloc " hl #\space bc #\space d #\space e))
  (call bump-alloc)
  (preserve (hl)
    (if c
      (then (writeln-ok "out of memory " hl))
      (else (writeln-error "allocated " hl #\space de)))
    (writeln))

  (ld bc #x1dfe)
  (ld d #b10100000) ; tag
  (ld e #b11100000) ; bank
  (preserve (bc de hl) (writeln "alloc " hl #\space bc #\space d #\space e))
  (call bump-alloc)
  (preserve (hl)
    (if c
      (then (writeln-error "out of memory " hl))
      (else (writeln-ok "allocated " hl #\space de)))))

