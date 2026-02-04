(import
  (only (micascheme) check equal? quote procedure? let)
  (micalang runtime-term))

(check (equal? bool (literal 'bool)))
(check (equal? int (literal 'int)))

(check (equal? type (native 'type)))
(check (equal? bool (native 'bool)))
(check (equal? int (native 'int)))

(check (equal? (app zero? (literal 0)) (literal #t)))
(check (equal? (app inc (literal 2)) (literal 3)))
(check (equal? (app dec (literal 2)) (literal 1)))

(check (equal? (app (app = (literal 2)) (literal 2)) (literal #t)))
(check (equal? (app (app = (literal 2)) (literal 3)) (literal #f)))
(check (equal? (app (app + (literal 2)) (literal 3)) (literal 5)))
(check (equal? (app (app - (literal 5)) (literal 3)) (literal 2)))
(check (equal? (app (app < (literal 2)) (literal 3)) (literal #t)))

(check (equal? (pi-param (pi (_ int) bool)) int))
(check (equal? (app (pi (_ int) bool) (literal '())) bool))

(check (equal? (pi-param (pi (x type) x)) type))
(check (equal? (app (pi (x type) x) int) int))

(check (equal? (pi-param (pi (x type) (list x))) type))
(check (equal? (app (pi (x type) (app list x)) int) (app list int)))
