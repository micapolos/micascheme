(import
  (rename (micascheme) (+ %+) (- %-))
  (sjasm mapping))

(define-sjasm-mappings
  (+ %+)
  (- %-))

(check (equal? (+ 1 2) 3))
(check (equal? (- 3 2) 1))
(check (equal? (+ 1 (- 3 2)) 2))
(check (equal? (+ 1 'a) '(+ 1 a)))
