(import (scheme) (check) (procedure) (emu mmu))

(let ()
  ; 13-bit memory banks, 3-bit slots
  (define-mmu-8 mmu 13 3)

  (check (equal? (mmu-8-bank mmu 0) 0))
  (check (equal? (mmu-8-bank mmu 1) 0))
  (check (equal? (mmu-8-bank mmu 2) 0))
  (check (equal? (mmu-8-bank mmu 3) 0))
  (check (equal? (mmu-8-bank mmu 4) 0))
  (check (equal? (mmu-8-bank mmu 5) 0))
  (check (equal? (mmu-8-bank mmu 6) 0))
  (check (equal? (mmu-8-bank mmu 7) 0))

  (mmu-8-bank mmu 0 1)
  (mmu-8-bank mmu 2 5)
  (mmu-8-bank mmu 7 11)

  (check (equal? (mmu-8-bank mmu 0) 1))
  (check (equal? (mmu-8-bank mmu 1) 0))
  (check (equal? (mmu-8-bank mmu 2) 5))
  (check (equal? (mmu-8-bank mmu 3) 0))
  (check (equal? (mmu-8-bank mmu 4) 0))
  (check (equal? (mmu-8-bank mmu 5) 0))
  (check (equal? (mmu-8-bank mmu 6) 0))
  (check (equal? (mmu-8-bank mmu 7) 11))

  (check (equal? (mmu-8-addr mmu #x0234) #x2234))
  (check (equal? (mmu-8-addr mmu #x2234) #x0234))
  (check (equal? (mmu-8-addr mmu #x4234) #xa234))
  (check (equal? (mmu-8-addr mmu #x6234) #x0234))
  (check (equal? (mmu-8-addr mmu #x8234) #x0234))
  (check (equal? (mmu-8-addr mmu #xa234) #x0234))
  (check (equal? (mmu-8-addr mmu #xc234) #x0234))
  (check (equal? (mmu-8-addr mmu #xe234) #x16234)))