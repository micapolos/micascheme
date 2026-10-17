(import (scheme) (check) (fixnum))

(check (equal? (fx+1/wraparound (most-positive-fixnum)) (most-negative-fixnum)))
(check (equal? (fx-1/wraparound (most-negative-fixnum)) (most-positive-fixnum)))

(check (fx= (fxmask 0) #x0))
(check (fx= (fxmask 4) #xf))
(check (fx= (fxmask 8) #xff))

(check (fx= (fxbits 0 #x1234) #x0))
(check (fx= (fxbits 4 #x1234) #x4))
(check (fx= (fxbits 8 #x1234) #x34))

(check (fx= (fxpack) 0))
(check (fx= (fxpack (4 #x1234)) #x4))
(check (fx= (fxpack (4 #x12) (4 #x34) (4 #x56)) #x246))
