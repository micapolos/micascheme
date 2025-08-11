(import (zx-next test) (zx-next compiler expr))

(define-fragment test-data (dw #x1234))

(test
  (case u8-add
    (ld-expr a (u8-add (u8 #x11) (u8 #x22)))
    (assert a #x33))

  (case u8-add/transitive
    (ld-expr a
      (u8-add
        (u8-add (u8 #x01) (u8 #x02))
        (u8-add (u8 #x03) (u8 #x04))))
    (assert a #x0a))

  (case u8-peek-nn
    (ld-expr a (u8-peek-nn test-data))
    (assert a #x34))

  (case u8-peek-nn
    (ld-expr a (u8-peek-nn (+ test-data 1)))
    (assert a #x12))

  (case u8-peek
    (ld-expr a (u8-peek (u16 test-data)))
    (assert a #x34))

  (case u8-peek
    (ld-expr a (u8-peek (u16-inc (u16 test-data))))
    (assert a #x12))

  (case u8-mul
    (ld-expr de (u8-mul (u8 #x02) (u8 #x03)))
    (assert de #x0006))

  (case if-u8-zero?-positive
    (ld-expr a (if (u8-zero? (u8 #x00)) (u8 #x34) (u8 #x56)))
    (assert a #x34))

  (case if-u8-zero?-negative
    (ld-expr a (if (u8-zero? (u8 #x12)) (u8 #x34) (u8 #x56)))
    (assert a #x56))

  (case if-u8=?-positive
    (ld-expr a (if (u8=? (u8 #x01) (u8 #x01)) (u8 #x34) (u8 #x56)))
    (assert a #x34))

  (case if-u8=?-negative
    (ld-expr a (if (u8=? (u8 #x01) (u8 #x02)) (u8 #x34) (u8 #x56)))
    (assert a #x56))

  (case if-u8<?-positive
    (ld-expr a (if (u8<? (u8 #x01) (u8 #x02)) (u8 #x34) (u8 #x56)))
    (assert a #x34))

  (case if-u8<?-negative
    (ld-expr a (if (u8<? (u8 #x02) (u8 #x02)) (u8 #x34) (u8 #x56)))
    (assert a #x56))
)
