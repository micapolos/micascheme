(import (scheme) (check) (emu unit))

(let ()
  (define-unit (mem size)
    (bytevector (make-bytevector size 0)))
  (define-mem mem-1 #x10)
  (define-mem mem-2 #x20)
  (check (equal? (mem-size mem-1) #x10))
  (check (equal? (mem-size mem-2) #x20))
  (check (equal? (mem-bytevector mem-1) (make-bytevector #x10 0)))
  (check (equal? (mem-bytevector mem-2) (make-bytevector #x20 0))))