(import
  (zx-next test)
  (zx-next scheme prims))

(test
  (case cons
    ; Load cdr
    (ld de #x0123)
    (ld hl #x4567)

    ; Push cdr
    (push de)
    (push hl)

    ; Load car
    (ld de #x890a)
    (ld hl #xbcde)

    ; Make cons
    (cons)

    (assert e #x01)
    (assert hl #xe003)
    (assert-byte (#xe003) #xde)
    (assert-word (#xe004) #x0abc)
    (assert-byte (#xe006) #x67)
    (assert-word (#xe007) #x2345)))

