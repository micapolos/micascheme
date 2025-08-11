(import (zx-next test) (zx-next compiler expr))

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

  (case byte-mul
    (ld-expr de (byte-mul (byte #x02) (byte #x03)))
    (assert de #x0006))
)
