(import (check) (labs macro) (labs syntax-match))

(lets
  ($lookup
    (lambda ($key $id)
      (cond
        ((free-identifier=? $id #'syntax-literal?)
          (or
            (free-identifier=? $key #'+)
            (free-identifier=? $key #'-)))
        ((free-identifier=? $id #'syntax-matcher)
          #f))))
  (run
    (check
      (equal?
        (syntax->datum
          (macro-case $lookup #'(+ "foo" "bar")
            ((+ a b) (a plus b))
            ((- a b) (a minus b))
            ((op a b) (a op b))))
        '("foo" plus "bar")))

    (check
      (equal?
        (syntax->datum
          (macro-case $lookup #'(- "foo" "bar")
            ((+ a b) (a plus b))
            ((- a b) (a minus b))
            ((op a b) (a op b))))
        '("foo" minus "bar")))

    (check
      (equal?
        (syntax->datum
          (macro-case $lookup #'(* "foo" "bar")
            ((+ a b) (a plus b))
            ((- a b) (a minus b))
            ((op a b) (a op b))))
        '("foo" * "bar")))
  )
)

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
            (match ($string $syntax)))))))

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

