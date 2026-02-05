(import
  (only (micascheme) check equal? quote procedure?)
  (micalang comptime))

(check (equal? type (literal 'type)))
(check (equal? bool (literal 'bool)))
(check (equal? int (literal 'int)))

(check (equal? (app zero? (literal 0)) (literal #t)))
(check (equal? (app inc (literal 2)) (literal 3)))
(check (equal? (app dec (literal 2)) (literal 1)))

(check (equal? (app (app = (literal 2)) (literal 2)) (literal #t)))
(check (equal? (app (app = (literal 2)) (literal 3)) (literal #f)))
(check (equal? (app (app + (literal 2)) (literal 3)) (literal 5)))
(check (equal? (app (app - (literal 5)) (literal 3)) (literal 2)))
(check (equal? (app (app < (literal 2)) (literal 3)) (literal #t)))

(check (equal? (app < (literal 1) (literal 3)) (literal #t)))

(check (equal? (pi-param (pi int bool)) int))
(check (equal? (app (pi int bool) (literal '())) bool))

(check (equal? (pi-param (pi (x type) x)) type))
(check (equal? (app (pi (x type) x) int) int))

(check (equal? (pi-param (pi (x type) (list x))) type))
(check (equal? (app (pi (x type) (app list x)) int) (app list int)))

(check
  (equal?
    (let
      (x (literal 10))
      (y (literal 20))
      (app + x y))
    (literal 30)))
