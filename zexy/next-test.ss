(import
  (check)
  (zexy base)
  (zexy next))

(check (equal? (bytevector-length (next-mmus (make-next))) 8))
(check (equal? (vector-length (next-banks (make-next))) #x100))

(lets
  ($next (make-next))
  (run
    (next-wr $next #x0000 #x12)
    (next-wr $next #x0001 #x34)
    (check (equal? (next-rd $next #x0000) #x12))
    (check (equal? (next-rd $next #x0001) #x34))

    (next-println $next "Hello, world!")
    (check (equal? (next-string $next) "Hello, world!\n"))))
