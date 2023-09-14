(import (micascheme) (leoscheme))

(check (equal? (leo string-append) string-append))
(check (equal? (leo (string-append)) ""))

(check (equal? (leo "foo") "foo"))
(check (equal? (leo "foo" (string-length)) 3))
(check (equal? (leo "foo" (string-append "bar")) "foobar"))

(check (equal? (leo "foo" "bar" (string-append)) "foobar"))
(check (equal? (leo (string-append "foo" "bar")) "foobar"))

(check (equal? (leo (with "foo")) "foo"))
(check (equal? (leo (with "foo") (with "bar") (string-append)) "foobar"))
(check (equal? (leo (string-append (with "foo") (with "bar"))) "foobar"))
(check (equal? (leo (string-append: "foo" "bar")) "foobar"))

(check (equal? ((leo (doing "foo"))) "foo"))
(check (equal? ((leo $x (doing $x (string-length))) "foo") 3))
(check (equal? ((leo $a (with $b) (doing $a (string-append $b))) "foo" "bar") "foobar"))

(check
  (equal?
    (leo
      "foo"
      (get string-length))
    3))

(check
  (equal?
    (leo
      "foo"
      (do string-length))
    3))

(check
  (equal?
    (leo
      (doing "foo")
      (apply))
    "foo"))

(check
  (equal?
    (leo
      $string
      (doing $string (string-append "!"))
      (apply "foo"))
    "foo!"))

(check
  (equal?
    (leo
      $string1 $string2
      (doing $string1 (string-append $string2))
      (apply "foo" "bar"))
    "foobar"))

(check
  (equal?
    (leo
      (splice "1" "2")
      (string-append (splice "3" "4")))
    "1234"))

(check
  (equal?
    (leo
      (splice
        ("1" (string-append "2"))
        ("3" (string-append "4")))
      (string-append
        (splice
          ("5" (string-append "6"))
          ("7" (string-append "8")))))
    "12345678"))

(check
  (equal?
    (leo
      (splice
        ("1" (string-append "2"))
        ("3" (string-append "4")))
      (string-append:
        ("5" (string-append "6"))
        ("7" (string-append "8"))))
    "12345678"))

(check
  (equal?
    (leo
      $x (is "foo")
      $y (is "bar")
      $z (is $x (string-append $y))
      $z
      (string-append "!"))
    "foobar!"))

(check
  (equal?
    (leo
      $x
      (add $y)
      (does $x (+ $y))
      "foo"
      (string-append "bar"))
    "foobar"))

(leo
  pi
  (is 3.14)

  (void))
