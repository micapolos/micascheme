(import
  (zx-next core)
  (zx-next debug)
  (zx-next mem)
  (zx-next scheme value)
  (zx-next scheme pair))

(run
  (ld de #x4000)
  (ld bc #x1800)
  (ld a #b11110000)
  (call mem-fill)

  (ld bc #x0100)
  (ld a #b01111000)
  (call mem-fill)

  (ld bc #x0100)
  (ld a #b11111011)
  (call mem-fill)

  (ld bc #x0100)
  (ld a #b01110101)
  (call mem-fill)

  (call loop-bars)
  ;(call ref)
  ;(call car)
  ;(call cdr)
  )
