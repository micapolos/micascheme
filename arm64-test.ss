(import (only (scheme) environment expand quote equal? number->string) (check) (arm64) (boolean))

(check (equal? (w-index? w0) 0))
(check (equal? (w-index? w30) 30))
(check (equal? (w-index? w31) #f))

(check (equal? (x-index? x0) 0))
(check (equal? (x-index? x30) 30))
(check (equal? (x-index? x31) #f))

(check (equal? (r-index x12) 12))
(check (equal? (r-index w12) 12))

(check (equal? (expand '(r-index x12) (environment '(arm64))) 12))
(check (raises (expand '(r-index x31) (environment '(arm64)))))

(check (equal? (sf w0 w3 w12) 0))
(check (equal? (sf x0 x3 x12) 1))

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
