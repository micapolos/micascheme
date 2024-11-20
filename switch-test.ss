(import (scheme) (check) (switch) (procedure) (binder) (syntax) (lets))

; === switch

(check
  (equal?
    (switch (string-append "foo" "bar")
      ((string? $string) (string-append $string "!"))
      ((number? $number) (number->string $number))
      ((else $other) (format "~s?" $other)))
    "foobar!"))

(check
  (equal?
    (switch (+ 1 2)
      ((string? $string) (string-append $string "!"))
      ((number? $number) (number->string $number))
      ((else $other) (format "~s?" $other)))
    "3"))

(check
  (equal?
    (switch #f
      ((string? $string) (string-append $string "!"))
      ((number? $number) (number->string $number))
      ((else $other) (format "~s?" $other)))
    "#f?"))

(check
  (equal?
    (switch #f
      ((string? $string) (string-append $string "!"))
      ((number? $number) (number->string $number)))
    (void)))

(run
  (define-aux-keyword number-with-one)
  (define-aux-keyword string-with-excl)

  (define-bind number-with-one
    (syntax-rules ()
      ((_ ($var $number $one) $body)
        (lets
          ($number $var)
          ($one 1)
          $body))))

  (define-bind string-with-excl
    (syntax-rules ()
      ((_ ($var $string $excl) $body)
        (lets
          ($string $var)
          ($excl "!")
          $body))))

  (check
    (equal?
      (switch "foo"
        ((string? (string-with-excl $s1 $s2)) (string-append $s1 $s2))
        ((number? (number-with-one $n1 $n2)) (+ $n1 $n2)))
      "foo!"))

  (check
    (equal?
      (switch 128
        ((string? (string-with-excl $s1 $s2)) (string-append $s1 $s2))
        ((else (number-with-one $n1 $n2)) (+ $n1 $n2)))
      129)))

; === switch-opt

(check
  (equal?
    (switch-opt (string-append "foo" "bar")
      ((string? $string) (string-append $string "!"))
      ((number? $number) (number->string $number)))
    "foobar!"))

(check
  (equal?
    (switch-opt (+ 1 2)
      ((string? $string) (string-append $string "!"))
      ((number? $number) (number->string $number)))
    "3"))

(check
  (equal?
    (switch-opt #\a
      ((string? $string) (string-append $string "!"))
      ((number? $number) (number->string $number)))
    #f))

; === switch-exclusive

(check
  (equal?
    (switch-exclusive (string-append "foo" "bar")
      ((string? $string) (string-append $string "!"))
      ((number? $number) (number->string $number)))
    "foobar!"))

(check
  (equal?
    (switch-exclusive (+ 1 2)
      ((string? $string) (string-append $string "!"))
      ((number? $number) (number->string $number)))
    "3"))

(check
  (raises
    (switch-exclusive #\a
      ((string? $string) (string-append $string "!"))
      ((number? $number) (number->string $number)))))

; === index-switch

(check (equal? (index-switch 0 "zero" "one" "two") "zero"))
(check (equal? (index-switch 1 "zero" "one" "two") "one"))
(check (equal? (index-switch 2 "zero" "one" "two") "two"))
(check (equal? (index-switch 3 "zero" "one" "two") "two"))
