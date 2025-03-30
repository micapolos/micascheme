(import (micascheme) (datum))

(check (equal? (pretty-datum 123) 123))
(check (equal? (pretty-datum "foo") "foo"))
(check (equal? (pretty-datum 'foo) 'foo))
(check (equal? (pretty-datum (gensym "v0")) 'v0))
(check (equal? (pretty-datum (cons (gensym "a") (gensym "b"))) '(a . b)))
