(import (check) (lets))

(check (equal? (lets 1) 1))

(check (equal? (lets (x 1) x) 1))

(check (equal? (lets ((values x) 1) x) 1))

(check (equal? (lets ((values) (values)) 1) 1))
(check (equal? (lets ((values x) (values 1)) x) 1))
(check (equal? (lets ((values x y) (values 3 2)) (- x y)) 1))

(check
  (equal?
    (lets
      (x 1)
      (y (+ x 2))
      y)
    3))

(check
  (equal?
    (lets
      (x (box 1))
      (_ (set-box! x 2))
      (unbox x))
    2))

(check
  (equal?
    (lets
      (fib (rec (lambda (n) (if (< n 2) n (+ (fib (- n 2)) (fib (- n 1)))))))
      (fib 10))
    55))

; --- binder

(define-binder opt
  (lambda ($opt $fn)
    (and $opt ($fn $opt))))

(check
  (equal?
    (lets
      ((opt a) "foo")
      ((opt b) "bar")
      (string-append a b))
    "foobar"))

(check
  (equal?
    (lets
      ((opt a) #f)
      ((opt b) "bar")
      (string-append a b))
    #f))

(check
  (equal?
    (lets
      ((opt a) "foo")
      ((opt b) #f)
      (string-append a b))
    #f))
