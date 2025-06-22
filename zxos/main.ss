(import (zxos))

(zxos
  (ld de #x4000)
  (ld bc #x1800)
  (ld a #b11001100)
  (call mem-fill)

  (ld de #x5800)
  (ld bc #x100)
  (ld a #b01111010)
  (call mem-fill)

  (ld de #x5900)
  (ld bc #x100)
  (ld a #b11001111)
  (call mem-fill)

  (ld de #x5a00)
  (ld bc #x100)
  (ld a #b00100111)
  (call mem-fill)

  (mmu 0 #xff)
  (mmu 1 #xff)

  (ld hl hi-string)
  (ld de #x4000)
  (call ula-blit-string)

  (jp debug-bars)

  (data hi-string
    (db #\H #\i #\! 0))

  (import
    (mem fill)
    (debug bars)
    (ula blit string)))
