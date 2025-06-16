(import (micascheme) (simplang expander) (simplang core))

(define scope
  (append
    (list
      (cons 'b 'boolean)
      (cons 'b1 'boolean)
      (cons 'b2 'boolean)
      (cons 'i 'integer)
      (cons 'i1 'integer)
      (cons 'i2 'integer)
      (cons 'ch 'char)
      (cons 's 'string))
    core-scope))

(check (equal? (typed scope #f) '(boolean . #f)))
(check (equal? (typed scope #\a) '(char . #\a)))
(check (equal? (typed scope 123) '(integer . 123)))
(check (equal? (typed scope "foo") '(string . "foo")))
(check (equal? (typed scope '(typed integer x)) '(integer . x)))

(check (equal? (typed scope 'b) '(boolean . b)))
(check (equal? (typed scope 'i) '(integer . i)))
(check (equal? (typed scope 'ch) '(char . ch)))
(check (equal? (typed scope 's) '(string . s)))
(check (raises (typed scope 'x)))

(check
  (equal?
    (typed scope '(let ((x 10) (y 20)) x))
    '(integer . (let ((x 10) (y 20)) x))))

(check
  (equal?
    (typed scope '(let ((x 10) (y 20)) (+ x y)))
    '(integer . (let ((x 10) (y 20)) (+ x y)))))

(check (equal? (typed scope '(= #t #f)) '(boolean . (boolean=? #t #f))))
(check (equal? (typed scope '(= 10 20)) '(boolean . (= 10 20))))
(check (equal? (typed scope '(= #\a #\b)) '(boolean . (char=? #\a #\b))))
(check (equal? (typed scope '(= "a" "b")) '(boolean . (string=? "a" "b"))))
(check (raises (typed scope '(=))))
(check (raises (typed scope '(= 10))))
(check (raises (typed scope '(= 10 "foo"))))
(check (raises (typed scope '(= 10 20 30))))

(check (raises (typed scope '(+))))
(check (raises (typed scope '(+ #\a))))
(check (raises (typed scope '(+ #f))))

(check (equal? (typed scope '(+ 1)) '(integer . (+ 1))))
(check (equal? (typed scope '(+ 1 2)) '(integer . (+ 1 2))))
(check (equal? (typed scope '(+ 1 2 3)) '(integer . (+ 1 2 3))))

(check (equal? (typed scope '(+ "a")) '(string . (string-append "a"))))
(check (equal? (typed scope '(+ "a" "b")) '(string . (string-append "a" "b"))))
(check (equal? (typed scope '(+ "a" "b" "c")) '(string . (string-append "a" "b" "c"))))

(check (raises (typed scope '(+ 1 "foo"))))
(check (raises (typed scope '(+ "foo" 1))))

(check (raises (typed scope '(-))))
(check (raises (typed scope '(- #\a))))
(check (raises (typed scope '(- "a"))))
(check (raises (typed scope '(- #f))))

(check (equal? (typed scope '(- 1)) '(integer . (- 1))))
(check (equal? (typed scope '(- 1 2)) '(integer . (- 1 2))))
(check (equal? (typed scope '(- 1 2 3)) '(integer . (- 1 2 3))))

(check (raises (typed scope '(length 1))))
(check (equal? (typed scope '(length "foo")) '(integer . (string-length "foo"))))

(check (equal? (typed scope '(if #t 10 20)) '(integer . (if #t 10 20))))
(check (raises (typed scope '(if 10 20 30))))
(check (raises (typed scope '(if #t 10 "foo"))))
(check (raises (typed scope '(if #t 10))))

(check
  (equal?
    (typed scope '(cond (b1 i1) (b2 i2) (else i)))
    '(integer . (if b1 i1 (if b2 i2 i)))))

(check
  (equal?
    (typed scope
      `(let
        (
          (hello "Hello")
          (exclamate
            (macro ($scope $syntax)
              (syntax-case $syntax ()
                ((_ s) `(+ ,#'s "!")))))
          (world "world"))
        (exclamate (+ hello ", " world))))
    '(string .
      (let ((hello "Hello") (world "world"))
        (string-append (string-append hello ", " world) "!")))))
