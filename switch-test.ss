(import (check) (switch))

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
  (raises?
    (lambda ()
      (switch-exclusive #\a
        ((string? $string) (string-append $string "!"))
        ((number? $number) (number->string $number))))))
