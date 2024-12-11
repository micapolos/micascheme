(import (scheme) (check) (blob) (syntax) (lets) (procedure))

(check (blob? (blob 0 (lambda ($port) (void)))))

(check-equal?
  (blob->bytevector
    (blob 3
      (lambda ($port)
        (put-u8 $port 1)
        (put-u8 $port 2)
        (put-u8 $port 3))))
  (bytevector 1 2 3))

(define-rule-syntax (check-blob blob byte ...)
  (lets
    ($blob blob)
    ($bytevector (blob->bytevector $blob))
    ($size (bytevector-length $bytevector))
    (run
      (check-equal? (blob-size $blob) $size)
      (check-equal? $bytevector (bytevector byte ...)))))

(check-blob (empty-blob))

(check-blob
  (u8-blob 123)
  123)

(check-blob
  (blob-append (u8-blob 1) (u8-blob 2) (u8-blob 3))
  1 2 3)

(check-blob
  (list->blob (list (u8-blob 1) (u8-blob 2) (u8-blob 3)))
  1 2 3)
