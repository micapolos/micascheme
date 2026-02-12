(import (micalang base) (micalang term) (micalang term-eval))

(define (string-exclamate s) (string-append s "!"))

(check
  (term-equal?
    (term-eval (application (native string-exclamate) (native "foo")))
    (native "foo!")))

(check
  (term-equal?
    (term-eval (application (native string-exclamate) (variable 'x)))
    (application (native string-exclamate) (variable 'x))))

(check
  (term-equal?
    (term-eval (application (variable 'x) (variable 'y)))
    (application (variable 'x) (variable 'y))))
