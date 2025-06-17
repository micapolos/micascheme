(import (micascheme) (simplang lang))

(check (equal? (simplang (typed string (string-append "foo" "bar"))) "foobar"))

(check (equal? (simplang (= 1 1)) #t))
(check (equal? (simplang (= 1 2)) #f))

(check (equal? (simplang (= #t #t)) #t))
(check (equal? (simplang (= #t #f)) #f))

(check (equal? (simplang (= #\a #\a)) #t))
(check (equal? (simplang (= #\a #\b)) #f))

(check (equal? (simplang (= "foo" "foo")) #t))
(check (equal? (simplang (= "foo" "bar")) #f))

(check (equal? (simplang (+ 1 2)) 3))
(check (equal? (simplang (+ "foo" "bar")) "foobar"))

(check (equal? (simplang (- 3 2)) 1))

(check (equal? (simplang (length "foo")) 3))

(check (equal? (simplang (if #t "foo" "bar")) "foo"))
(check (equal? (simplang (if #f "foo" "bar")) "bar"))

(check (equal? (simplang (let ((x 10) (y 20)) (+ x y))) 30))

(check
  (equal?
    (simplang ((lambda ((string a) (string b)) (+ a b)) "foo" "bar"))
    "foobar"))

(check
  (equal?
    (simplang
      (let
        (
          (hello "Hello")
          (exclamate
            (macro ($scope $syntax)
              (syntax-case $syntax ()
                ((_ s) `(+ ,#'s "!")))))
          (world "world"))
        (exclamate (+ hello ", " world))))
    "Hello, world!"))
