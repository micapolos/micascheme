(import (chezscheme) (base))

; === associ ===

(check (equal? (associ (list) 100 `a) #f))
(check (equal? (associ (list (cons `a `foo) (cons `b `bar)) 100 `a) (cons 100 `foo)))
(check (equal? (associ (list (cons `a `foo) (cons `b `bar)) 100 `b) (cons 101 `bar)))
(check (equal? (associ (list (cons `a `foo) (cons `b `bar)) 100 `c) #f))

; === map-find-indexed ===

(check
  (obj=?
    (map-find-indexed
      (lambda (s) (and (> (string-length s) 3) (string-append s "!")))
      (list "ala" "ma" "kota" "i" "psa"))
    (indexed "kota!" 2)))

(check
  (obj=?
    (map-find-indexed
      (lambda (s) (and (> (string-length s) 5) (string-append s "!")))
      (list "ala" "ma" "kota" "i" "psa"))
    #f))
