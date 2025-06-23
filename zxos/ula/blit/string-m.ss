(import
  (ula blit char))

(proc ula-blit-string-m
  (input
    (hl string-addr)
    (de screen-addr))
  (output
    (de advanced)
    (hl advanced)
    (a 0))
  (loop
    (ld a (hl))
    (inc hl)
    (or a)
    (ret m)
    (preserve (de hl) (call ula-blit-char))
    (inc e)))
