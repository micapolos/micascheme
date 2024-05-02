(import (scheme) (check) (syntaxes) (procedure))

(run-void
  (define-rules-syntax (literals +)
    ((foo $a) (string-append "foo" $a))
    ((foo $a $b) (string-append $a $b))
    ((foo $a + $b) (string-append $a "+" $b))
    ((foo $a $op $b) (string? (datum $op)) (string-append $a $op $b)))

  (check (equal? (foo "bar") "foobar"))
  (check (equal? (foo "foo" "bar") "foobar"))
  (check (equal? (foo "foo" + "bar") "foo+bar"))
  (check (equal? (foo "foo" "-" "bar") "foo-bar"))
)

(run-void
  (define-rules-syntaxes (literals +)
    ((foo $a) (string-append "foo" $a))
    ((foo $a $b) (string-append $a $b))
    ((foo $a + $b) (string-append $a "+" $b))
    ((foo $a $op $b) (string? (datum $op)) (string-append $a $op $b))
    ((bar $a) (string-append "bar" $a)))

  (check (equal? (foo "bar") "foobar"))
  (check (equal? (foo "foo" "bar") "foobar"))
  (check (equal? (foo "foo" + "bar") "foo+bar"))
  (check (equal? (foo "foo" "-" "bar") "foo-bar"))
  (check (equal? (bar "foo") "barfoo"))
)
