(proc mem-clear
  (input (de address) (bc size))
  (ld a 0))

(proc mem-fill
  (input (a value) (de address) (bc size))
  (ld h d)
  (ld l e)
  (ld (de) a)
  (inc de)
  (dec bc)
  (ldir)
  (ret))
