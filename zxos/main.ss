(import (zxos lang))

(zxos
  (ld de #x4000)
  (ld bc #x1800)
  (ld a #b11001100)
  (call mem-fill)

  (ld de #x5800)
  (ld bc #x100)
  (ld a #b01010111)
  (call mem-fill)

  (ld de #x5900)
  (ld bc #x100)
  (ld a #b11001111)
  (call mem-fill)

  (ld de #x5a00)
  (ld bc #x100)
  (ld a #b00100111)
  (call mem-fill)

  (jp debug-bars)

  (import
    (mem fill)
    (debug bars)))
