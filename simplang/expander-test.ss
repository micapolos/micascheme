(import (micascheme) (simplang expander))

(define scope
  '(
    (b . boolean)
    (i . integer)
    (ch . char)
    (s . string)))

(check (equal? (typed scope #f) '(boolean . #f)))
(check (equal? (typed scope #\a) '(char . #\a)))
(check (equal? (typed scope 123) '(integer . 123)))
(check (equal? (typed scope "foo") '(string . "foo")))
(check (equal? (typed scope '(: integer x)) '(integer . x)))

(check (equal? (typed scope 'b) '(boolean . b)))
(check (equal? (typed scope 'i) '(integer . i)))
(check (equal? (typed scope 'ch) '(char . ch)))
(check (equal? (typed scope 's) '(string . s)))
(check (raises (typed scope 'x)))

(check
  (equal?
    (typed scope '(let ((x 10) (y 20)) x))
    '(integer . (let ((x 10) (y 20)) x))))

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
