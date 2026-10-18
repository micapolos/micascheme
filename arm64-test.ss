(import
  (scheme)
  (check)
  (arm64)
  (lets)
  (jit)
  (foreign)
  (boolean))

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

(check-ops
  ((add x0 x0 x1) "10001011 00000001 00000000 00000000")
  ((sub x0 x0 x1) "11001011 00000001 00000000 00000000")

  ((adds x0 x0 x1) "10101011 00000001 00000000 00000000")
  ((subs x0 x0 x1) "11101011 00000001 00000000 00000000")

  ((add x0 x0 #b111111) "10010001 00000000 11111100 00000000")
  ((sub x0 x0 #b111111) "11010001 00000000 11111100 00000000")

  ((adds x0 x0 #b111111) "10110001 00000000 11111100 00000000")
  ((subs x0 x0 #b111111) "11110001 00000000 11111100 00000000")

  ((ret) "11010110 01011111 00000011 11000000")
  ((ret x7) "11010110 01011111 00000000 11100000")

  ((b #xff) "00010100 00000000 00000000 11111111")
  ((b -4) "00010111 11111111 11111111 11111100")
  ((bl #xff) "10010100 00000000 00000000 11111111")
  ((bl -4) "10010111 11111111 11111111 11111100")

  ((b cs #xff) "01010100 00000000 00011111 11100010")
  ((b cs -4) "01010100 11111111 11111111 10000010")
  ((br x7) "11010110 00011111 00000000 11100000")
  ((blr x7) "11010110 00111111 00000000 11100000")

  ((ldr x3 (+ x7 x15)) "11111000 01101111 01001000 11100011")
  ((ldr x3 (x7)) "11111001 01000000 00000000 11100011")
  ((ldr x3 (+ x7 #xff)) "11111001 01000011 11111100 11100011")
  ((ldr x3 (pre+ x7 #xff)) "11111000 01001111 11111100 11100011")
  ((ldr x3 (post+ x7 #xff)) "11111000 01001111 11110100 11100011")
)

(check-exec
  (test
    (params (int 30) (int 20))
    (add x0 x0 x1)
    (result (int 50)))

  (test
    (params (int 30) (int 20))
    (sub x0 x0 x1)
    (result (int 10)))

  (test
    (params (int 30))
    (add x0 x0 1)
    (result (int 31)))

  (test
    (params (int 30))
    (sub x0 x0 1)
    (result (int 29)))

  (test
    (params (int 10000) (int 100) (int 30))
    (add x0 x0 x1)
    (subs x2 x2 1)
    (b ne -2)
    (result (int 13000)))
)
