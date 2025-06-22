(proc mem-clear
  (ld a 0))

(proc mem-fill
  (ld h d)
  (ld l e)
  (ld (de) a)
  (inc de)
  (dec bc)
  (ldir)
  (ret))
