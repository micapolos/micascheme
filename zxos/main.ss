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

  (ld hl string-1)
  (ld de #x4000)
  (call ula-blit-string-c)
  (ld hl string-2)
  (ld de #x4800)
  (call ula-blit-string-c)

  (jp debug-bars)

  (data string-1 (string-c "Hi there!!!"))
  (data string-2 (string-c "This is ZX Spectrum Next."))

  (import
    (mem fill)
    (debug bars)
    (mem bank alloc)
    (ula blit string-c)))
