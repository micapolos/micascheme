(import (check) (labs slick) (micascheme))

(check
  (equal?
    (slick "foo")
    "foo"))

(check
  (equal?
    (slick
      "foo"
      (string-append "bar"))
    "foobar"))

(check
  (equal?
    (slick
      "foo"
      "bar"
      (string-append))
    "foobar"))

(check
  (equal?
    (slick
      (string-append "foo" "bar"))
    "foobar"))

; the keyword
(check
  (equal?
    (slick
      (the "foo" (string-append "bar"))
      (the "!")
      (string-append))
    "foobar!"))

; expression context
(check
  (equal?
    (slick
      (the
        $foo (is "foo")
        $bar (is "bar")
        $foo (string-append $bar)))
    "foobar"))

; top-level context
(check
  (equal?
    (run
      (slick
        $foo (is "foo")
        $bar (is "bar")
        $foo (string-append $bar)))
    "foobar"))

; lambda
(check
  (procedure?
    (slick
      $x $y
      (doing $x (string-append $y)))))

; application
(check
  (equal?
    (slick
      $x $y
      (doing $x (string-append $y))
      (app "foo" "bar"))
    "foobar"))
