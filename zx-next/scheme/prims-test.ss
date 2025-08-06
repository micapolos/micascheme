(import
  (zx-next test)
  (zx-next scheme prims))

(define-fragment test-box
  (value-data (value #x56 #x1234)))

(define-fragment test-pair
  (pair-data
    (value #x56 #x1234)
    (value #xbc #x789a)))

(test
  (case unbox
    (ld de #x0400)
    (ld hl test-box)
    (unbox)
    (assert de #x0456)
    (assert hl #x3412))

  (case car
    (ld de #x0400)
    (ld hl test-pair)
    (car)
    (assert de #x0456)
    (assert hl #x3412))

  (case cdr
    (ld de #x0400)
    (ld hl test-pair)
    (cdr)
    (assert de #x04bc)
    (assert hl #x9a78))

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

