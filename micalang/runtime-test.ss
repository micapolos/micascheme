(import (only (micascheme) check equal? quote procedure?) (micalang runtime) (prefix (micalang term) %))

(check (equal? type (native 'type)))
(check (equal? bool (native 'bool)))
(check (equal? int (native 'int)))

(check (equal? (zero? 0) #t))
(check (equal? (inc 2) 3))
(check (equal? (dec 2) 1))

(check (equal? ((+ 2) 3) 5))
(check (equal? ((- 5) 3) 2))
(check (equal? ((< 2) 3) #t))

(check (procedure? list))
(check (equal? (list int) (%application list int)))

(check (procedure? (pi bool int)))
(check (equal? ((pi bool int) '()) int))

(check (procedure? (pi (x : type) x)))
(check (equal? ((pi (x : type) x) int) int))
(check (equal? ((pi (x : type) (list x)) int) (list int)))
