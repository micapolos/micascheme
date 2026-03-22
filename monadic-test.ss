(import
  (scheme)
  (check)
  (monadic)
  (syntax)
  (lets)
  (list))

(define (counter $value)
  (lambda ($count)
    (values $count $value)))

(define (counter-run $counter)
  (counter-run-from $counter 0))

(define (counter-run-from $counter $count)
  ($counter $count))

(define (counter-bind $counter $fn)
  (lambda ($count)
    (lets
      ((values $count $value) (counter-run-from $counter $count))
      (counter-run-from ($fn $value) (+ $count 1)))))

(define-rule-syntax (check-counter counter out)
  (check
    (equal?
      (values->list (counter-run counter))
      'out)))

(define-monadic counter)

(check-counter
  (counter-map (counter 123) number->string)
  (1 "123"))

(check-counter
  (counter-lets (counter "foo"))
  (0 "foo"))

(check-counter
  (counter-lets
    ($foo (counter "foo"))
    ($bar (counter "bar"))
    ($foobar (counter (string-append $foo $bar)))
    (counter $foobar))
  (3 "foobar"))

(check-counter
  (counter-lets?
    ($foo (counter "foo"))
    ($bar (counter #f))
    ($foobar (counter (string-append $foo $bar)))
    (counter $foobar))
  (2 #f))

(check-counter
  (list->counter (list (counter "foo") (counter "bar") (counter "gar")))
  (6 ("foo" "bar" "gar")))

(check-counter
  (append-counter (counter "foo") (counter "bar") (counter "gar"))
  (6 ("foo" "bar" "gar")))

(check-counter
  (replace-counter (counter "foo") "bar")
  (1 "bar"))

(check-counter
  (counter-switch (counter 12)
    ((number? $number) (counter (+ $number 1)))
    ((string? $string) (counter (string-append $string "!")))
    ((else $other) (counter (list $other))))
  (1 13))

(check-counter
  (counter-switch (counter "foo")
    ((number? $number) (counter (+ $number 1)))
    ((string? $string) (counter (string-append $string "!")))
    ((else $other) (counter (list $other))))
  (1 "foo!"))

(check-counter
  (counter-switch (counter #\space)
    ((number? $number) (counter (+ $number 1)))
    ((string? $string) (counter (string-append $string "!")))
    ((else $other) (counter (list $other))))
  (1 (#\space)))

(check-counter
  (counter-or)
  (0 #f))

(check-counter
  (counter-or
    (counter "foo")
    (counter "bar")
    (counter "goo"))
  (1 "foo"))

(check-counter
  (counter-or
    (counter #f)
    (counter #f)
    (counter "foo")
    (counter "bar"))
  (3 "foo"))
