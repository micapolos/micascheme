(import (zx-next test) (zx-next compiler expr))

(define-fragment test-data (dw #x1234))

(test
  (case byte-add
    (ld-expr a (byte-add (byte #x11) (byte #x22)))
    (assert a #x33))

  (case byte-add/transitive
    (ld-expr a
      (byte-add
        (byte-add (byte #x01) (byte #x02))
        (byte-add (byte #x03) (byte #x04))))
    (assert a #x0a))

  (case byte-peek-nn
    (ld-expr a (byte-peek-nn test-data))
    (assert a #x34))

  (case byte-peek-nn
    (ld-expr a (byte-peek-nn (+ test-data 1)))
    (assert a #x12))

  (case byte-peek
    (ld-expr a (byte-peek (word test-data)))
    (assert a #x34))

  (case byte-peek
    (ld-expr a (byte-peek (word-inc (word test-data))))
    (assert a #x12))

  (case byte-mul
    (ld-expr de (byte-mul (byte #x02) (byte #x03)))
    (assert de #x0006))
)
