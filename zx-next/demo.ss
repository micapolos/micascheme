(import
  (zx-next core)
  (zx-next debug)
  (zx-next mem))

(run
  (ld de #x4000)
  (ld bc #x1800)
  (ld a #b10010101)
  (call mem-fill)

  (ld bc #x0100)
  (ld a #b00111000)
  (call mem-fill)

  (ld bc #x0100)
  (ld a #b00111011)
  (call mem-fill)

  (ld bc #x0100)
  (ld a #b00110101)
  (call mem-fill)

  (call loop-bars))
