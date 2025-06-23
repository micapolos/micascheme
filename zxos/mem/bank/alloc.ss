(import (bit mask))

(proc mem-bank-a-free?
  (input (a bank-number))
  (output (fz free?))

  (call mem-bank-a->hl-addr/b-mask)
  (ld a (hl))
  (and b)
  (ret))

(proc mem-bank-alloc-a
  (output (fc error) (a bank-number))

  (ld hl mem-bank-index)
  (ld a (hl))
  (ld b 0)
  (loop-djnz
    (preserve (af bc)
      (call mem-bank-a->hl-addr/b-mask)
      (ld a (hl))
      (and b)
      (jp nz try-next-bank)

      (label found-free-bank)
      (or b)
      (ld (hl) a)
      (ld hl mem-bank-index)
      (ld (hl) a)
      (ld b 1) ; end the loop
      (rcf)
      (ret)

      (label try-next-bank))
    (inc a))
  (label no-free-banks)
  (scf)
  (ret))

(proc mem-bank-a->hl-addr/b-mask
  (input (a bank-number))
  (output (hl addr) (b mask))

  ; b = bit-mask
  (preserve (af)
    (and #b111)
    (call bit-mask)
    (ld b a))

  ; a = table offset
  (rrca)
  (rrca)
  (rrca)
  (and #b11111)

  ; a = entry
  (ld hl mem-bank-table)
  (add hl a)
  (ret))

(data mem-bank-index (db 0))
(data mem-bank-table (db #b11101111) (ds 31 0))
