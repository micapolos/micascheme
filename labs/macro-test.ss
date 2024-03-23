(import (check) (labs macro) (labs syntax-match))

(let ()
  (define-macro-literal? +)
  (define-macro-literal? -)

  (define-macro dawaj
    ((_ $a + $b) (string-append $a " + " $b))
    ((_ $a - $b) (string-append $a " - " $b))
    ((_ $a $op $b) (string-append $a " " $op " " $b))
    (_ "dawaj"))

  (check (equal? (dawaj "foo" + "bar") "foo + bar"))
  (check (equal? (dawaj "foo" - "bar") "foo - bar"))
  (check (equal? (dawaj "foo" "*" "bar") "foo * bar"))
  (check (equal? dawaj "dawaj"))
)

(let ()
  (define-macro minus
    ((_ a b) (- a b))
    ((_ a) (- a)))

  (check (equal? (minus 3 2) 1))
  (check (equal? (minus 3) -3))
)

(let ()
  ; TODO: Define custom syntax for macro-matcher
  (define-macro-matcher string
    (lambda ($lookup $syntax $pattern)
      (syntax-case $pattern ()
        ((_ $string)
          (and (identifier? #'$string))
          (and
            (string? (syntax->datum $syntax))
            (match-put null-match #'$string $syntax))))))

  (define-macros
    ((plus (string a) (string b)) (string-append a b))
    ((plus a b) (+ a b))
    ((minus a b) (- a b))
    ((minus a) (- a)))

  (check (equal? (plus "foo" "bar") "foobar"))
  (check (equal? (plus 3 2) 5))
  (check (equal? (minus 3 2) 1))
  (check (equal? (minus 3) -3))
)

