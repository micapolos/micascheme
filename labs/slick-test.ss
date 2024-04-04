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
      (values "foo" "bar" (string-append))
      (then "!" "!" "!" (string-append))
      (string-append))
    "foobar!!!"))

(check
  (equal?
    (call-with-values (lambda () (slick 10 20 30 (values))) list)
    (list 10 20 30)))

(check
  (equal?
    (slick
      ($a $b)
      (lambda (string-append $a $b))
      (app "foo" "bar"))
    "foobar"))

(check
  (equal?
    (let ()
      (slick
        (plusik $a $b)
        (define (string-append $a $b))
        (then "foo" "bar" (plusik))))
    "foobar"))
