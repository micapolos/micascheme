(import
  (only (micascheme) check equal? quote procedure?)
  (micalang comptime))

(check (equal? (prim fx+) (prim fx+)))

(check
  (equal?
    (app (app (prim + a b) (literal 2)) (literal 3))
    (literal 5)))

(check (equal? type (literal 'type)))
(check (equal? bool (literal 'bool)))
(check (equal? int (literal 'int)))
(check (equal? symbol (literal 'symbol)))
(check (equal? string (literal 'string)))

(check (equal? (app zero? (literal 0)) (literal #t)))
(check (equal? (app inc (literal 2)) (literal 3)))
(check (equal? (app dec (literal 2)) (literal 1)))

(check (equal? (app (app = (literal 2)) (literal 2)) (literal #t)))
(check (equal? (app (app = (literal 2)) (literal 3)) (literal #f)))
(check (equal? (app (app + (literal 2)) (literal 3)) (literal 5)))
(check (equal? (app (app - (literal 5)) (literal 3)) (literal 2)))
(check (equal? (app (app < (literal 2)) (literal 3)) (literal #t)))

(check (equal? (pi-param (pi int bool)) int))
(check (equal? (app (pi int bool) (literal '())) bool))

(check (equal? (pi-param (pi (x type) x)) type))
(check (equal? (app (pi (x type) x) int) int))

(check (equal? (pi-param (pi (x type) (list x))) type))
(check (equal? (app (pi (x type) (app list x)) int) (app list int)))

(check
  (equal?
    (if (literal #t) (literal 10) (literal 20))
    (literal 10)))

(check
  (equal?
    (if (literal #f) (literal 10) (literal 20))
    (literal 20)))

(check
  (equal?
    (let (x (literal 10))
      (let (y (literal 20))
        (app (app + x) y)))
    (literal 30)))

; index type
(check
  (equal?
    (app index (literal 16))
    (application (native index) (native 16))))

; array type
(check
  (equal?
    (app array (literal 16))
    (application (native array) (native 16))))

; start index
(check
  (equal?
    (app first-index (literal 16))
    (native 0)))

; end index
(check
  (equal?
    (app last-index (literal 16))
    (native 15)))
