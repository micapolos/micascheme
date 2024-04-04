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

(check
  (equal?
    (slick
      (the "foo" (string-append "bar"))
      (the "!")
      (string-append))
    "foobar!"))

(check
  (equal?
    (call-with-values (lambda () (slick 10 20 30)) list)
    (list 10 20 30)))
