(import (zx-next test) (zx-next bit-index))

(test
  (case lbi-de-n
    (lbi de #x75)
    (assert de #xc020))

  (case lbi-de-b
    (ld b #x75)
    (lbi de b)
    (assert de #xc020))

  (case bii-no-carry
    (ld de #x1208)
    (bii de)
    (assert de #x1210))

  (case bii-carry
    (ld de #x1280)
    (bii de)
    (assert de #x1301))

  (case bid-no-carry
    (ld de #x1210)
    (bid de)
    (assert de #x1208))

  (case bid-carry
    (ld de #x1201)
    (bid de)
    (assert de #x1180))

  (case load-bit-index
    (load-bit-index #x75)
    (assert de #xc020))

  (case load-bit-index
    (ld b #x75)
    (load-bit-index b)
    (assert de #xc020)))
