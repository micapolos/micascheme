(import
  (tt lang)
  (prefix (scheme) %)
  (prefix (data) %)
  (prefix (check) %)
  (prefix (tt hoas) %)
  (prefix (tt hoas-compiler) %)
  (prefix (tt primitive) %))

(define-type fx)
(define-type (point))
(define-type (list _))
(define-type (pair _ _))

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
  ((=> (x number) (y string) x) 10 "foo")
  (typed number 10))

(check
  ((=> (x number) (y number)
    (typed number (%+ x y))) 10 20)
  (typed number 30))

(define (+ number number number) %+)

(check
  (+ my-number 10)
  (typed number 20))

; --- identity

(define identity
  (typed
    (forall x (-> x x))
    (%lambda (x) x)))

(check
  (identity 123)
  (typed number 123))

; --- pairs

(define cons
  (typed
    (forall a b (-> a b (pair a b)))
    %cons))

(define car
  (typed
    (forall a b (-> (pair a b) a))
    %car))

(define cdr
  (typed
    (forall a b (-> (pair a b) b))
    %cdr))

(check
  (cons 10 "foo")
  (typed
    (pair number string)
    (10 . "foo")))

(check
  (car (cons 10 "foo"))
  (typed number 10))

(check
  (cdr (cons 10 "foo"))
  (typed string "foo"))

; --- lists

(define null
  (typed
    (forall x (list x))
    (%quote ())))

(check
  null
  (typed
    (forall $0 (list $0))
    ()))

(define link
  (typed
    (forall x (-> x (list x) (list x)))
    %cons))

(check
  (link "foo" null)
  (typed
    (list string)
    ("foo")))

(check
  (link "bar" (link "foo" null))
  (typed
    (list string)
    ("bar" "foo")))

; --- point

(%data (%point x y))

(define (make-point number number point) %point)
(define (point-x point number) %point-x)
(define (point-y point number) %point-y)

(check
  (point-x (make-point 10 20))
  (typed number 10))

(check
  (point-y (make-point 10 20))
  (typed number 20))
