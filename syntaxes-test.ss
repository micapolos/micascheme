(import (scheme) (check) (syntaxes) (procedure))

(run-void
  (with-ellipsis ___
    (define-syntax foo
      (syntax-rules ()
        ((_ $id ___ $last) (list $id ___ $last))))
    (define-syntax bar
      (syntax-rules ()
        ((_ $id ___ $last) (list $id ___ $last)))))
  (check (equal? (foo 1 2 3 4 5) (list 1 2 3 4 5)))
  (check (equal? (bar 1 2 3 4 5) (list 1 2 3 4 5))))

(run-void
  (define-syntax outer
    (syntax-rules ()
      ((_ $id)
        (with-ellipsis ___
          (define-syntax $id
            (syntax-rules ()
              (($id $x ___)
                (list $x ___))))))))
  (outer inner)
  (check (equal? (inner 1 2 3) (list 1 2 3))))

(run-void
  (define-rules-syntax
    ((foo $a) (string-append "foo" $a))
    ((foo $a $b) (string-append $a $b)))

  (check (equal? (foo "bar") "foobar"))
  (check (equal? (foo "foo" "bar") "foobar")))

(run-void
  (define-rules-syntax (literals +)
    ((foo $a) (string-append "foo" $a))
    ((foo $a $b) (string-append $a $b))
    ((foo $a + $b) (string-append $a "+" $b))
    ((foo $a $op $b) (string? (datum $op)) (string-append $a $op $b)))

  (check (equal? (foo "bar") "foobar"))
  (check (equal? (foo "foo" "bar") "foobar"))
  (check (equal? (foo "foo" + "bar") "foo+bar"))
  (check (equal? (foo "foo" "-" "bar") "foo-bar")))

(run-void
  (define-rules-syntaxes
    ((foo $a) (string-append "foo" $a))
    ((foo $a $b) (string-append $a $b))
    ((bar $a) (string-append "bar" $a)))

  (check (equal? (foo "bar") "foobar"))
  (check (equal? (foo "foo" "bar") "foobar"))
  (check (equal? (bar "foo") "barfoo")))

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
  (check (equal? (bar "foo") "barfoo")))
