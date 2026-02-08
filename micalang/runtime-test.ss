(import
  (only (micascheme) check equal? quote procedure?)
  (micalang runtime)
  (prefix (micalang term) %))

(check (equal? (prim +) (prim +)))
(check (equal? (app (app (prim + a b) 2) 3) 5))

(check (equal? (app zero? (literal 0)) #t))

(check (equal? (app (app = (literal 2)) (literal 2)) (literal #t)))
(check (equal? (app (app = (literal 2)) (literal 3)) (literal #f)))
(check (equal? (app (app + (literal 2)) (literal 3)) (literal 5)))
(check (equal? (app (app - (literal 5)) (literal 3)) (literal 2)))
(check (equal? (app (app < (literal 2)) (literal 3)) (literal #t)))

(check (procedure? (lambda x x)))
(check (equal? ((lambda x x) (literal 10)) 10))

(check (procedure? (pi number boolean)))
(check (equal? ((pi number boolean) (literal 10)) boolean))

(check (procedure? (pi (x number) (app (app + x) (literal 1)))))
(check (equal? ((pi (x number) (app (app + x) (literal 1))) (literal 10)) 11))

(check (procedure? (pi (x type) x)))
(check (equal? ((pi (x type) x) number) number))

(check
  (equal?
    (let
      (x (literal 10))
      (y (literal 20))
      ((app + x) y))
    (literal 30)))

(check
  (equal?
    (if (literal #t) (literal 10) (literal 20))
    (literal 10)))

(check
  (equal?
    (if (literal #f) (literal 10) (literal 20))
    (literal 20)))
