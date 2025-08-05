(import (zx-next test) (zx-next bank-table))

(test
  (case init-bank-cursor
    (init-bank-cursor)
    (assert de #x8000)
    (assert hl bank-table))

  (case inc-bank-cursor-no-overflow
    (ld de #x1012)
    (ld hl bank-table)
    (inc-bank-cursor de hl)
    (assert de #x0813)
    (assert hl bank-table))

  (case inc-bank-cursor-mask-overflow
    (ld de #x0112)
    (ld hl bank-table)
    (inc-bank-cursor de hl)
    (assert de #x8013)
    (assert hl (+ bank-table 1)))

  (case inc-bank-cursor-address-overflow
    (ld de #x0112)
    (ld hl (+ bank-table #x1f))
    (inc-bank-cursor de hl)
    (assert de #x8013)
    (assert hl bank-table))

  (case count-free-banks
    (count-free-banks)
    (assert a #xdc))

  (case bank-reserve
    (bank-reserve #x40)
    (bank-reserved? #x40)
    (assert nz)
    (count-free-banks)
    (assert a #xdb))

  (case bank-free
    (bank-free #x40)
    (bank-reserved? #x40)
    (assert z)
    (count-free-banks)
    (assert a #xdc)))
