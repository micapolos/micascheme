(import (micascheme) (z80 blob))

(check-equal?
  (blob->bytevector
    (blob 3
      (lambda ($port)
        (put-u8 $port 1)
        (put-u8 $port 2)
        (put-u8 $port 3))))
  (bytevector 1 2 3))

(define-rule-syntax (check-blob blob byte ...)
  (check-equal?
    (blob->bytevector blob)
    (bytevector byte ...)))

(check-blob
  (u8-blob 123)
  123)

(check-blob
  (blob-append (u8-blob 1) (u8-blob 2) (u8-blob 3))
  1 2 3)

(check-blob
  (list->blob (list (u8-blob 1) (u8-blob 2) (u8-blob 3)))
  1 2 3)

(check-blob
  (list->blob (list (u8-blob 1) (u8-blob 2) (u8-blob 3)))
  1 2 3)
