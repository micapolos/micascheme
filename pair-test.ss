(import (scheme) (check) (pair) (procedure))

; === null/pair? ===

(check-equal? (null/pair? `()) #t)
(check-equal? (null/pair? (cons 1 2)) #t)
(check-equal? (null/pair? 123) #f)

; === singleton-list? ===

(check (singleton-list? '(foo)))
(check (not (singleton-list? '())))
(check (not (singleton-list? '(foo bar))))
(check (not (singleton-list? '(foo . bar))))

; === pair-map ===

(check
  (equal?
    (pair-map number->string (cons 1 2))
    (cons "1" "2")))

; === car-lets ===

(check-equal?
  (map-car (partial + 1) (cons 10 20))
  (cons 11 20))

(check-equal?
  (map-cdr (partial + 1) (cons 10 20))
  (cons 10 21))

; === with-car / with-cdr ===

(check-equal?
  (with-car
    ($car (cons "foo" "bar"))
    (string-append $car "!"))
  (cons "foo!" "bar"))

(check-equal?
  (with-cdr
    ($car (cons "foo" "bar"))
    (string-append $car "!"))
  (cons "foo" "bar!"))

(check-equal? (cons/identity "foo" '()) "foo")
(check-equal? (cons/identity "foo" "bar") (cons "foo" "bar"))
