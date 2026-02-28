(import (scheme) (check) (syntaxes) (procedure))

(run-void
  (define-case-syntaxes
    ((foo $a) #`(string-append "foo" $a))
    ((foo $a $b) #`(string-append $a $b))
    ((bar $a) #`(string-append "bar" $a)))

  (check (equal? (foo "bar") "foobar"))
  (check (equal? (foo "foo" "bar") "foobar"))
  (check (equal? (bar "foo") "barfoo")))

(run-void
  (define-case-syntaxes (keywords +)
    ((foo $a) #`(string-append "foo" $a))
    ((foo $a $b) #`(string-append $a $b))
    ((foo $a + $b) #`(string-append $a "+" $b))
    ((foo $a $op $b) (string? (datum $op)) #`(string-append $a $op $b))
    ((bar $a) #`(string-append "bar" $a)))

  (check (equal? (foo "bar") "foobar"))
  (check (equal? (foo "foo" "bar") "foobar"))
  (check (equal? (foo "foo" + "bar") "foo+bar"))
  (check (equal? (foo "foo" "-" "bar") "foo-bar"))
  (check (equal? (bar "foo") "barfoo")))

(run-void
  (define-rules-syntaxes
    ((foo $a) (string-append "foo" $a))
    ((foo $a $b) (string-append $a $b))
    ((bar $a) (string-append "bar" $a)))

  (check (equal? (foo "bar") "foobar"))
  (check (equal? (foo "foo" "bar") "foobar"))
  (check (equal? (bar "foo") "barfoo")))

(run-void
  (define-rules-syntaxes (keywords +)
    ((foo $a) (string-append "foo" $a))
    ((foo $a $b) (string-append $a $b))
    ((foo $a + $b) (string-append $a "+" $b))
    ((foo $a $op $b) (string? (datum $op)) (string-append $a $op $b))
    ((bar $a) (string-append "bar" $a)))

  (check (equal? (foo "bar") "foobar"))
  (check (equal? (foo "foo" "bar") "foobar"))
  (check (equal? (foo "foo" + "bar") "foo+bar"))
  (check (equal? (foo "foo" "-" "bar") "foo-bar"))
  (check (equal? (bar "foo") "barfoo")))

(run-void
  (define-rules-syntax
    ((foo $a) (string-append "foo" $a))
    ((foo $a $b) (string-append $a $b)))

  (check (equal? (foo "bar") "foobar"))
  (check (equal? (foo "foo" "bar") "foobar")))

(run-void
  (define-rules-syntax (keywords +)
    ((foo $a) (string-append "foo" $a))
    ((foo $a $b) (string-append $a $b))
    ((foo $a + $b) (string-append $a "+" $b))
    ((foo $a $op $b) (string? (datum $op)) (string-append $a $op $b)))

  (check (equal? (foo "bar") "foobar"))
  (check (equal? (foo "foo" "bar") "foobar"))
  (check (equal? (foo "foo" + "bar") "foo+bar"))
  (check (equal? (foo "foo" "-" "bar") "foo-bar")))
