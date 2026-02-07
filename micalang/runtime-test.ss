(import (only (micascheme) check equal? quote procedure?) (micalang runtime) (prefix (micalang term) %))

(check (equal? ($primitive 3 fx+) ($primitive 3 fx+)))
(check (equal? (app (app (prim a b ($primitive 3 +)) 2) 3) 5))

(check (equal? (app zero? (literal 0)) #t))
(check (equal? (app inc (literal 2)) (literal 3)))
(check (equal? (app dec (literal 2)) (literal 1)))

(check (equal? (app (app = (literal 2)) (literal 2)) (literal #t)))
(check (equal? (app (app = (literal 2)) (literal 3)) (literal #f)))
(check (equal? (app (app + (literal 2)) (literal 3)) (literal 5)))
(check (equal? (app (app - (literal 5)) (literal 3)) (literal 2)))
(check (equal? (app (app < (literal 2)) (literal 3)) (literal #t)))

(check
  (equal?
    (let
      (x (literal 10))
      (y (literal 20))
      ((app + x) y))
    (literal 30)))

(check (equal? (first-index 16) 0))
(check (equal? (last-index 16) 15))
