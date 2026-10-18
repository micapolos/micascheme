(import (only (scheme) environment expand quote equal? number->string) (check) (arm64) (boolean))

(check (equal? (w-index? w0) 0))
(check (equal? (w-index? w30) 30))
(check (equal? (w-index? wzr) 31))
(check (equal? (w-index? w31) #f))

(check (equal? (x-index? x0) 0))
(check (equal? (x-index? x30) 30))
(check (equal? (x-index? xzr) 31))
(check (equal? (x-index? x31) #f))

(check (equal? (r-index x12) 12))
(check (equal? (r-index w12) 12))

(check (equal? (expand '(r-index x12) (environment '(arm64))) 12))
(check (raises (expand '(r-index x31) (environment '(arm64)))))

(check (equal? (sf w0 w3 w12) 0))
(check (equal? (sf x0 x3 x12) 1))

(check
  (equal?
    (op->string (add x0 x0 x1))
    "10001011 00000001 00000000 00000000"))

(check
  (equal?
    (op->string (sub x0 x0 x1))
    "11001011 00000001 00000000 00000000"))

(check
  (equal?
    (op->string (adds x0 x0 x1))
    "10101011 00000001 00000000 00000000"))

(check
  (equal?
    (op->string (subs x0 x0 x1))
    "11101011 00000001 00000000 00000000"))

(check
  (equal?
    (op->string (add x0 x0 #b111111))
    "10010001 00000000 11111100 00000000"))

(check
  (equal?
    (op->string (sub x0 x0 #b111111))
    "11010001 00000000 11111100 00000000"))

(check
  (equal?
    (op->string (adds x0 x0 #b111111))
    "10110001 00000000 11111100 00000000"))

(check
  (equal?
    (op->string (subs x0 x0 #b111111))
    "11110001 00000000 11111100 00000000"))

(check
  (equal?
    (op->string (ret))
    "11010110 01011111 00000011 11000000"))

(check
  (equal?
    (op->string (b #xff))
    "00010100 00000000 00000000 11111111"))

(check
  (equal?
    (op->string (b -4))
    "00010111 11111111 11111111 11111100"))

(check
  (equal?
    (op->string (b cs #xff))
    "01010100 00000000 00011111 11100010"))

(check
  (equal?
    (op->string (b cs -4))
    "01010100 11111111 11111111 10000010"))
