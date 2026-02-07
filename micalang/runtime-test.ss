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

(check (equal? ((lambda x x) (literal 10)) 10))

(check (equal? (%pi-symbol? (pi number boolean)) #f))
(check (equal? (%pi-param (pi number boolean)) number))
(check (equal? ((%pi-procedure (pi number boolean)) #f) boolean))

(check (equal? (%pi-symbol? (pi (x type) x)) 'x))
(check (equal? (%pi-param (pi (x type) x)) type))
(check (equal? ((%pi-procedure (pi (x type) x)) number) number))

(check
  (equal?
    (let
      (x (literal 10))
      (y (literal 20))
      ((app + x) y))
    (literal 30)))

(check (equal? (first-index 16) 0))
(check (equal? (last-index 16) 15))

(check
  (equal?
    (if (literal #t) (literal 10) (literal 20))
    (literal 10)))

(check
  (equal?
    (if (literal #f) (literal 10) (literal 20))
    (literal 20)))
