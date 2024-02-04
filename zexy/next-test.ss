(import
  (check)
  (zexy next))

(check (equal? (bytevector-length (next-mmus (make-next))) 8))
(check (equal? (vector-length (next-banks (make-next))) #x100))

(next-println (make-next) "Hello, world!")
