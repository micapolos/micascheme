(import (only (scheme) equal? number->string) (check) (arm64) (boolean))

(check (equal? (x? x0) #t))
(check (equal? (x? x30) #t))
(check (equal? (x? x31) #f))

(check (equal? (x-index x0) 0))
(check (equal? (x-index x30) 30))

(check
  (equal?
    (number->string (add x0 x0 x1) 2)
    "10001011000000010000000000000000"))

(check
  (equal?
    (number->string (sub x0 x0 x1) 2)
    "11001011000000010000000000000000"))

(check
  (equal?
    (number->string (ret) 2)
    "11010110010111110000001111000000"))
