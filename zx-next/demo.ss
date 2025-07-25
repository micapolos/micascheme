(import
  (zx-next core)
  (zx-next debug)
  (zx-next mem))

(run
  (ld de #x4000)
  (ld bc #x1800)
  (ld a #b10101010)
  (call mem-fill)

  (ld de #x1800)
  (ld bc #x0300)
  (ld a #b00111000)
  (call mem-fill)

  (call loop-bars))
