(proc bit-mask
  (input (a bit-number))
  (output (a bit-mask))
  (ld hl bit-mask-table)
  (add hl a)
  (ld a (hl))
  (ret))

(data bit-mask-table
  (db #b00000001)
  (db #b00000010)
  (db #b00000100)
  (db #b00001000)
  (db #b00010000)
  (db #b00100000)
  (db #b01000000)
  (db #b10000000))
