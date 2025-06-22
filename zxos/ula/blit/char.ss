(proc ula-blit-char
  (input
    (a char)
    (de dst))
  (sub #x20)
  (ld h 0)
  (ld l a)
  (add hl hl)
  (add hl hl)
  (add hl hl)
  (add hl #x3d00)
  (ld b 8)
  (loop-djnz
    (ld a (hl))
    (inc hl)
    (ld (de) a)
    (inc d))
  (ret))
