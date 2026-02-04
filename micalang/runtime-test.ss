(import (only (micascheme) check equal? quote) (micalang runtime))

(check (equal? bool (native 'bool)))
(check (equal? int (native 'int)))

(check (equal? (zero? 0) #t))
(check (equal? (inc 2) 3))
(check (equal? (dec 2) 1))

(check (equal? ((+ 2) 3) 5))
(check (equal? ((- 5) 3) 2))
(check (equal? ((< 2) 3) #t))
