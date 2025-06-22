; de - dst
(proc mem-clear
  (ld a 0))

; a - value
; de - dst
(proc mem-fill
  (ld h d)
  (ld l e)
  (ld (de) a)
  (inc de)
  (dec bc)
  (ldir)
  (ret))
