(import (check) (syntaxes) (procedure))

(run-void
  (define-case-syntaxes ()
    ((foo $a) #'(string-append "foo" $a))
    ((foo $a $b) #'(string-append $a $b))
    ((bar $a) #'(string-append "bar" $a)))

  (check (equal? (foo "bar") "foobar"))
  (check (equal? (foo "foo" "bar") "foobar"))
  (check (equal? (bar "foo") "barfoo"))
)

(run-void
  (define-rules-syntaxes ()
    ((foo $a) (string-append "foo" $a))
    ((foo $a $b) (string-append $a $b))
    ((bar $a) (string-append "bar" $a)))

  (check (equal? (foo "bar") "foobar"))
  (check (equal? (foo "foo" "bar") "foobar"))
  (check (equal? (bar "foo") "barfoo"))
)
