(import (zx-next test) (zx-next compiler expr))

(define-fragment test-data (dw #x1234))

(test
  (case u8
    (ld-expr a (u8 #x22))
    (assert a #x22))

  (case u8-inc
    (ld-expr a (u8-inc (u8 #x11)))
    (assert a #x12))

  (case u8-dec
    (ld-expr a (u8-dec (u8 #x11)))
    (assert a #x10))

  (case u8-add-n
    (ld-expr a (u8-add-n (u8 #x11) #x22))
    (assert a #x33))

  (case u8-add
    (ld-expr a (u8-add (u8 #x11) (u8 #x22)))
    (assert a #x33))

  (case u8-sub-n
    (ld-expr a (u8-sub-n (u8 #x33) #x22))
    (assert a #x11))

  (case u8-sub
    (ld-expr a (u8-sub (u8 #x33) (u8 #x22)))
    (assert a #x11))

  (case u8-and-n
    (ld-expr a (u8-and-n (u8 #x33) #x0f))
    (assert a #x03))

  (case u8-and
    (ld-expr a (u8-and (u8 #x33) (u8 #x0f)))
    (assert a #x03))

  (case u8-mul
    (ld-expr de (u8-mul (u8 #x02) (u8 #x03)))
    (assert de #x0006))

  (case u8-and-n
    (ld-expr a (u8-or-n (u8 #x33) #x0f))
    (assert a #x3f))

  (case u8-and
    (ld-expr a (u8-or (u8 #x33) (u8 #x0f)))
    (assert a #x3f))

  (case u8-and-n
    (ld-expr a (u8-xor-n (u8 #x33) #x0f))
    (assert a #x3c))

  (case u8-and
    (ld-expr a (u8-xor (u8 #x33) (u8 #x0f)))
    (assert a #x3c))

  (case u8-peek-nn
    (ld-expr a (u8-peek-nn test-data))
    (assert a #x34))

  (case u8-peek-nn+1
    (ld-expr a (u8-peek-nn (+ test-data 1)))
    (assert a #x12))

  (case u8-peek
    (ld-expr a (u8-peek (u16 test-data)))
    (assert a #x34))

  (case u8-peek+1
    (ld-expr a (u8-peek (u16-inc (u16 test-data))))
    (assert a #x12))

  (case u8-peek-local
    (preserve (ix)
      (ld ix test-data)
      (ld-expr a (u8-peek-local 0))
      (assert a #x34)))

  (case u8-peek-local+1
    (preserve (ix)
      (ld ix test-data)
      (ld-expr a (u8-peek-local 1))
      (assert a #x12)))

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

  (case if-u8<-positive
    (ld-expr a (if (u8> (u8 #x03) (u8 #x02)) (u8 #x34) (u8 #x56)))
    (assert a #x34))

  (case if-u8<-negative
    (ld-expr a (if (u8> (u8 #x02) (u8 #x02)) (u8 #x34) (u8 #x56)))
    (assert a #x56))
)
