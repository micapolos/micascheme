(import (micascheme))

(lets
  ((list a b . c) (list "a" "b" "c" "d"))
  (run
    (check (equal? a "a"))
    (check (equal? b "b"))
    (check (equal? c (list "c" "d")))))
