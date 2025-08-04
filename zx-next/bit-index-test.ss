(import (zx-next test) (zx-next bit-index))

(test
  (case lbi-de-n
    (lbi d #x11)
    (assert de #x0240))

  (case lbi-de-b
    (ld e #x11)
    (lbi de)
    (assert de #x0240))

  (case bii-no-carry
    (ld de #x1208)
    (bii de)
    (assert de #x1204))

  (case bii-carry
    (ld de #x1201)
    (bii de)
    (assert de #x1380))

  (case bid-no-carry
    (ld de #x1210)
    (bid de)
    (assert de #x1220))

  (case bid-carry
    (ld de #x1280)
    (bid de)
    (assert de #x1101))

  (case load-bit-index
    (load-bit-index #x11)
    (assert de #x0240))

  (case load-bit-index
    (ld b #x11)
    (load-bit-index b)
    (assert de #x0240)))
