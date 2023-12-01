(import (check) (lets))

(check (equal? (lets 1) 1))
(check (equal? (lets (run 1)) 1))

(check (equal? (lets (x 1) x) 1))
(check (equal? (lets (x 1) (run x)) 1))

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

; --- accessors

(define-accessors (string-data string->number string-length string->list))

(lets
  ((string-data $number $length $chars) "123")
  (run
    (check (equal? $number 123))
    (check (equal? $length 3))
    (check (equal? $chars (list #\1 #\2 #\3)))))

(define-accessors
  (custom-accessors
    (lambda ($string) (string-append $string "!"))
    (lambda ($string) (string-append $string "?"))))

(lets
  ((custom-accessors $exclamated $questioned) "Hello")
  (run
    (check (equal? $exclamated "Hello!"))
    (check (equal? $questioned "Hello?"))))

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

; --- in

(define (linear $value)
  (lambda (_) $value))

(define (linear-bind $linear $fn)
  (lambda ($x)
    (app ($fn (app $linear $x)) $x)))

(check
  (equal?
    (app
      (lets
        (in linear
          ($sin sin)
          ($cos cos)
          ($const (linear 10))
          (_ (linear "ignored"))
          (run (linear "ignored"))
          (linear (+ $sin $cos $const))))
      128)
    (+ (sin 128) (cos 128) 10)))
