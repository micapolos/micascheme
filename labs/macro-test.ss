(import (check) (labs macro) (labs syntax-match) (micascheme))

(run-void
  (define $lookup
    (lambda ($key $id)
      (cond
        ((free-identifier=? $id #'syntax-literal?)
          (or
            (free-identifier=? $key #'+)
            (free-identifier=? $key #'-)))
        ((free-identifier=? $id #'syntax-matcher)
          #f))))

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
          (identifier? #'$string)
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

; Just check that it parses correctly.
(check
  (procedure?
    (pattern-rules
      ((r $code)
        (b #b000)
        (c #b001)
        (d #b010)
        (e #b011)
        (h #b100)
        (l #b101)
        (a #b111))
      ((r $prefix $code)
        (ixh #xdd #b100)
        (ixl #xdd #b101)
        (iyh #xfd #b100)
        (iyl #xfd #b101))
      ((r $prefix $code $offset)
        ((+ ix $d) #xdd #b110 $d)
        ((- ix $d) #xdd #b110 $d)
        ((+ ix $d) #xdd #b110 $d)
        ((- iy $d) #xfd #b110 $d)))))

(check
  (equal?
    (syntax->datum
      (macro-case-2 #'("foo" "bar")
        (($x $y) #'(match $x $y))
        (($x) #'(match $x))))
    '(match "foo" "bar")))

(check
  (equal?
    (syntax->datum
      (macro-case-2 #'("foo")
        (($x $y) #'(match $x $y))
        (($x) #'(match $x))))
    '(match "foo")))

(define-macro-2 dupa
  ((_ $x $y) (string-append $x $y))
  ((_ $x) (string-append $x "!")))

(check (equal? (dupa "foo") "foo!"))
(check (equal? (dupa "foo" "bar") "foobar"))

(define-macros-2
  ((dupa-2 $x $y) (string-append $x $y))
  ((dupa-1 $x) (string-append $x "!")))

(check (equal? (dupa-1 "foo") "foo!"))
(check (equal? (dupa-2 "foo" "bar") "foobar"))

