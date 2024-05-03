(import (scheme) (check) (emu mem))

(let ()
  (define-mem m 4)

  (check (true? m))

  (m 0 10)
  (mem-write m 2 20)

  (check (equal? (m 0) 10))
  (check (equal? (m 1) 0))
  (check (equal? (mem-read m 2) 20))
  (check (equal? (mem-read m 3) 0))

  (m 0 30)
  (mem-write m 3 40)

  (check (equal? (m 0) 30))
  (check (equal? (m 1) 0))
  (check (equal? (mem-read m 2) 20))
  (check (equal? (mem-read m 3) 40)))
