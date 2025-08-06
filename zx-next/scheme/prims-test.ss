(import
  (zx-next scheme test)
  (zx-next scheme prims)
  (zx-next tagged)
  (zx-next scheme tag))

(define-values
  (offset-1 #xa1)
  (offset-2 #xa2)
  (value-1 (value offset-1 #x56 #x1234))
  (value-2 (value offset-2 #xbc #x789a))
  (pair-1 (pair-value offset-1 pair-data-1)))

(define-fragments
  (test-box (value-data (value offset-1 #x56 #x1234)))
  (value-data-1 (value-data value-1))
  (value-data-2 (value-data value-2))
  (pair-data-1 (pair-data value-1 value-2)))

(test
  (case throw
    (assert-throws (throw)))

  (case null?
    (load-value (null-value offset-1))
    (null?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (tagged-word constant-tag true-word)))

  (case not-null?
    (load-value (byte-value offset-1 #x12))
    (null?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (tagged-word constant-tag false-word)))

  (case byte?
    (load-value (byte-value offset-1 #x12))
    (byte?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (tagged-word constant-tag true-word)))

  (case not-byte?
    (load-value (word-value offset-1 #x1234))
    (byte?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (tagged-word constant-tag false-word)))

  (case word?
    (load-value (word-value offset-1 #x1234))
    (word?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (tagged-word constant-tag true-word)))

  (case not-word?
    (load-value (byte-value offset-1 #x12))
    (word?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (tagged-word constant-tag false-word)))

  (case pair?
    (load-value (pair-value offset-1 pair-data-1))
    (pair?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (tagged-word constant-tag true-word)))

  (case not-pair?
    (load-value (byte-value offset-1 #x12))
    (pair?)
    (assert de (offset/byte offset-1 #x00))
    (assert hl (tagged-word constant-tag false-word)))

  (case unsafe-unbox
    (ld de (offset/byte #x04 #x00))
    (ld hl test-box)
    (unsafe-unbox)
    (assert de #x0456)
    (assert hl #x1234))

  (case unsafe-car
    (ld de (offset/byte #x04 #x00))
    (ld hl pair-data-1)
    (unsafe-car)
    (assert de #x0456)
    (assert hl #x1234))

  (case unsafe-cdr
    (ld de (offset/byte #x04 #x00))
    (ld hl pair-data-1)
    (unsafe-cdr)
    (assert de #x04bc)
    (assert hl #x789a))

  (case car
    (load-value (pair-value offset-1 pair-data-1))
    (car)
    (assert de (offset/byte #xa1 #x56))
    (assert hl #x1234))

  (case cdr
    (load-value (pair-value offset-1 pair-data-1))
    (cdr)
    (assert de (offset/byte #xa1 #xbc))
    (assert hl #x789a))

  (case car/throws
    (load-value (byte-value offset-1 #x12))
    (assert-throws (car)))

  (case cdr/throws
    (load-value (byte-value offset-1 #x12))
    (assert-throws (cdr)))

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
    (assert-word (#xe007) #x2345))

)
