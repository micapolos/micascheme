(import (scheme) (check) (lets) (binder) (list) (syntax) (procedure))

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

(define-bind opt
  (syntax-rules ()
    ((_ ($opt $value) $body)
      (lets ($value $opt)
        (and $value $body)))))

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

(check
  (equal?
    (lets
      (lambda (+ $a $b) (string-append $a $b))
      (+ "foo" "bar"))
    "foobar"))

(check
  (equal?
    (lets
      (lambda (+ $a $b . $cs) (apply string-append (cons* $a $b $cs)))
      (+ "foo" "bar" "goo" "zar"))
    "foobargoozar"))

(define-bind cons
  (syntax-rules ()
    ((_ ($cons $car $cdr) $body ...)
      (lets
        ($car (car $cons))
        ($cdr (cdr $cons))
        $body ...))))

(check
  (equal?
    (lets
      ((cons $a $b) (cons "a" "b"))
      (string-append $a $b))
    "ab"))

(check
  (equal?
    (lets
      ($string "a")
      (run
        (set! $string (string-append $string "b"))
        (set! $string (string-append $string "c")))
      $string)
    "abc"))
