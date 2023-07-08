(import (chezscheme) (base))

; === bind ===

(check (equal? (lets 1) 1))
(check (equal? (lets (x 1) (y (+ x 2)) y) 3))

; === associ ===

(check (equal? (associ (list) 100 `a) #f))
(check (equal? (associ (list (cons `a `foo) (cons `b `bar)) 100 `a) (cons 100 `foo)))
(check (equal? (associ (list (cons `a `foo) (cons `b `bar)) 100 `b) (cons 101 `bar)))
(check (equal? (associ (list (cons `a `foo) (cons `b `bar)) 100 `c) #f))

; === map-find-indexed ===

(let ((fn (lambda (s) (and (> (string-length s) 3) (string-append s "!")))))
  (check (obj=? (map-find-indexed fn (list "ala" "ma" "kocicę" "Lunę")) (indexed "kocicę!" 2)))
  (check (obj=? (map-find-indexed fn (list "ala" "ma" "ul")) #f)))

; === curry ===

(check (equal? ((partial string-append "a" "b") "c" "d") "abcd"))

; === indices ===

(check (equal? (indices 3) (list 0 1 2)))

; === indexed ===

(check (equal? (indexed-value (indexed "a" 1)) "a"))
(check (equal? (indexed-index (indexed "a" 1)) 1))

; === map-indexed ===

(check (equal? (map-indexed cons (list "a" "b" "c")) (list (cons 0 "a") (cons 1 "b") (cons 2 "c"))))

; === list-indexed ===

(check (obj=? (list-indexed (list "a" "b" "c")) (list (indexed "a" 0) (indexed "b" 1) (indexed "c" 2))))

; === iterate ===

(let (($fn (lambda (s) (string-append s "!"))))
  (check (equal? (iterate $fn "Hello" 0) "Hello"))
  (check (equal? (iterate $fn "Hello" 3) "Hello!!!")))