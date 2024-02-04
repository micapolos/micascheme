(import
  (check)
  (zexy base)
  (zexy next))

(check (equal? (bytevector-length (next-mmus (make-next))) 8))
(check (equal? (vector-length (next-banks (make-next))) #x100))

(define $next (make-next))
(check (equal? (next-mmu $next 0) 0))

(next-wr $next #x0000 #x12)
(next-wr $next #x0001 #x34)

(check (equal? (next-rd $next #x0000) #x12))
(check (equal? (next-rd $next #x0001) #x34))
(check (equal? (next-rd $next #x0002) #x00))

(next-mmu! $next 0 3)
(next-wr $next #x0000 #x56)
(next-wr $next #x0001 #x78)
(check (equal? (next-rd $next #x0000) #x56))
(check (equal? (next-rd $next #x0001) #x78))
(check (equal? (next-rd $next #x0002) #x00))

(next-mmu! $next 0 0)
(check (equal? (next-rd $next #x0000) #x12))
(check (equal? (next-rd $next #x0001) #x34))
(check (equal? (next-rd $next #x0002) #x00))

(next-println $next "Hello, world!")
(check (equal? (next-string $next) "Hello, world!\n"))
