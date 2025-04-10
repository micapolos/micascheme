(import (scheme) (check) (blob) (syntax) (lets) (procedure))

(check (blob? (blob 0 (lambda ($port) (void)))))

(lets
  ($put-proc (lambda ($port) (void)))
  (check-equal? (blob-put-proc (blob 0 $put-proc)) $put-proc))

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

(check-blob (u8-blob))
(check-blob (u8-blob 123) 123)
(check-blob (u8-blob 1 2 3 4) 1 2 3 4)

(check-blob
  (blob-append (u8-blob 1) (u8-blob 2) (u8-blob 3))
  1 2 3)

(check-blob
  (list->blob (list (u8-blob 1) (u8-blob 2) (u8-blob 3)))
  1 2 3)

(check-blob
  (bytevector->blob (bytevector 1 2 3))
  1 2 3)

(check-blob
  (utf8->blob "foo")
  102 111 111)

(check-datum=?
  (blob->syntax (u8-blob 1 2 3))
  '(bytevector->blob (($primitive 3 bytevector) 1 2 3)))
