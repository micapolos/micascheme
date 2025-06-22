(proc step
  (out (#xfe) a)
  (xor #b111)
  (loop-djnz
    (nop)
    (nop)
    (nop)
    (nop))
  (ret))
