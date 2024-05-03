(import (scheme) (check) (emu rom))

(let ()
  (define-rom (rom 4 write)
    (write 0 10)
    (write 2 20))

  (check (equal? (rom 0) 10))
  (check (equal? (rom 1) 0))
  (check (equal? (rom 2) 20))
  (check (equal? (rom 3) 0))

  (rom 0 30)
  (rom 1 30)
  (rom 2 30)
  (rom 3 30)

  (check (equal? (rom 0) 10))
  (check (equal? (rom 1) 0))
  (check (equal? (rom 2) 20))
  (check (equal? (rom 3) 0)))
