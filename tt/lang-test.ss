(import
  (tt lang)
  (prefix (scheme) %)
  (prefix (check) %)
  (prefix (tt hoas) %)
  (prefix (tt hoas-compiler) %)
  (prefix (tt primitive) %))

(define-type point)
(define-type (list 1))
(define-type (pair 2))

(check #f (typed boolean #f))
(check 10 (typed number 10))
(check #\a (typed char #\a))
(check "foo" (typed string "foo"))

(check
  (typed number (%+ 1 2))
  (typed number 3))

(define my-boolean #t)
(define my-number 10)
(define my-char #\a)
(define my-string "foo")

(check my-boolean (typed boolean #t))
(check my-number (typed number 10))
(check my-char (typed char #\a))
(check my-string (typed string "foo"))

(define my-point (typed point #f))
(check my-point (typed point #f))

(check
  ((lambda (x number) (y string) x) 10 "foo")
  (typed number 10))

(check
  ((lambda (x number) (y number)
    (typed number (%+ x y))) 10 20)
  (typed number 30))

(define +
  (lambda (x number) (y number)
    (typed number (%+ x y))))

(check
  (+ my-number 10)
  (typed number 20))

(define cons
  (typed
    (lambda car cdr (pi car cdr (pair car cdr)))
    cons))

;(check (cons 1 2) 12)
