(import (zx-next test) (zx-next compiler expr))

(define-fragment test-data (dw #x1234))

(test
  (case const-1
    (ld-expr h (1 #x22))
    (assert h #x22))

  (case const-2
    (ld-expr de (2 #x1234))
    (assert de #x1234))

  (case const-3
    (ld-expr ehl (3 #x123456))
    (assert e #x12)
    (assert hl #x3456))

  (case const-4
    (ld-expr dehl (4 #x12345678))
    (assert de #x1234)
    (assert hl #x5678))

  (case inc-1
    (ld-expr a (inc 1 (1 #x11)))
    (assert a #x12))

  (case dec-1
    (ld-expr a (dec 1 (1 #x11)))
    (assert a #x10))

  (case u8-neg
    (ld-expr a (u8-neg (1 #x22)))
    (assert a #xde))

  (case u8-not
    (ld-expr a (u8-not (1 #x22)))
    (assert a #xdd))

  (case add-n-1
    (ld-expr a (add-n 1 (1 #x11) #x22))
    (assert a #x33))

  (case add-1
    (ld-expr a (add 1 (1 #x11) (1 #x22)))
    (assert a #x33))

  (case sub-n-1
    (ld-expr a (sub-n 1 (1 #x33) #x22))
    (assert a #x11))

  (case sub-1
    (ld-expr a (sub 1 (1 #x33) (1 #x22)))
    (assert a #x11))

  (case and-n-1
    (ld-expr a (and-n 1 (1 #x33) #x0f))
    (assert a #x03))

  (case and-1
    (ld-expr a (and 1 (1 #x33) (1 #x0f)))
    (assert a #x03))

  (case or-n-1
    (ld-expr a (or-n 1 (1 #x33) #x0f))
    (assert a #x3f))

  (case or-1
    (ld-expr a (or 1 (1 #x33) (1 #x0f)))
    (assert a #x3f))

  (case xor-n-1
    (ld-expr a (xor-n 1 (1 #x33) #x0f))
    (assert a #x3c))

  (case xor-1
    (ld-expr a (xor 1 (1 #x33) (1 #x0f)))
    (assert a #x3c))

  (case u8-mul
    (ld-expr de (mul 1 (1 #x02) (1 #x03)))
    (assert de #x0006))

  (case peek-nn
    (ld-expr a (peek-nn 1 test-data))
    (assert a #x34))

  (case peek-nn+1
    (ld-expr a (peek-nn 1 (+ test-data 1)))
    (assert a #x12))

  (case peek
    (ld-expr a (peek 1 (2 test-data)))
    (assert a #x34))

  (case peek+1
    (ld-expr a (peek 1 (u16+1 (2 test-data))))
    (assert a #x12))

  (case peek-offset
    (preserve (ix)
      (ld ix test-data)
      (ld-expr a (peek-offset 1 0))
      (assert a #x34)))

  (case peek-offset+1
    (preserve (ix)
      (ld ix test-data)
      (ld-expr a (peek-offset 1 1))
      (assert a #x12)))

  (case if-u8-zero?-positive
    (ld-expr a (if (u8-zero? (1 #x00)) (1 #x34) (1 #x56)))
    (assert a #x34))

  (case if-u8-zero?-negative
    (ld-expr a (if (u8-zero? (1 #x12)) (1 #x34) (1 #x56)))
    (assert a #x56))

  (case if-u8=?-positive
    (ld-expr a (if (u8=? (1 #x01) (1 #x01)) (1 #x34) (1 #x56)))
    (assert a #x34))

  (case if-u8=?-negative
    (ld-expr a (if (u8=? (1 #x01) (1 #x02)) (1 #x34) (1 #x56)))
    (assert a #x56))

  (case if-u8<-positive
    (ld-expr a (if (u8> (1 #x03) (1 #x02)) (1 #x34) (1 #x56)))
    (assert a #x34))

  (case if-u8<-negative
    (ld-expr a (if (u8> (1 #x02) (1 #x02)) (1 #x34) (1 #x56)))
    (assert a #x56))

  (case local-2-1
    (with-locals
      (push #x3412)
      (push #x7856)
      (ld-expr hl () () (local 2 0)))
    (assert hl #x1234))

  (case local-1-1
    (with-locals
      (push #x3412)
      (push #x7856)
      (ld-expr a () (2) (local 1 1)))
    (assert a #x56))

  (case local-1-2
    (with-locals
      (push #x3412)
      (push #x7856)
      (ld-expr a () (2 1) (local 1 2)))
    (assert a #x78))

  (case arg-2-0
    (push #x3412)
    (push #x7856)
    (preserve (af) ; fake return address
      (with-locals
        (ld-expr hl () () (arg 2 0)))
      (assert hl #x5678)))

  (case arg-1-1
    (push #x3412)
    (push #x7856)
    (preserve (af) ; fake return address
      (with-locals
        (ld-expr a (2) () (arg 1 1)))
      (assert a #x34)))

  (case arg-1-2
    (push #x3412)
    (push #x7856)
    (preserve (af) ; fake return address
      (with-locals
        (ld-expr a (2 1) () (arg 1 2)))
      (assert a #x12)))
)
