(import
  (tt lang)
  (prefix (scheme) %)
  (prefix (tt lang-macros) %)
  (prefix (tt hoas) %%)
  (prefix (tt primitive) %%)
  (prefix (tt type) %%))

; --- primitive types

(check #f (typed boolean #f))
(check 10 (typed number 10))
(check #\a (typed char #\a))
(check "foo" (typed string "foo"))

(check '"foo" (typed datum "foo"))
(check '(+ 1 2) (typed datum (+ 1 2)))

(check
  (unchecked number (%+ 1 2))
  (typed number 3))

(define my-boolean #t)
(define my-number 10)
(define my-char #\a)
(define my-string "foo")

(check my-boolean (typed boolean #t))
(check my-number (typed number 10))
(check my-char (typed char #\a))
(check my-string (typed string "foo"))

(check
  ((lambda ((x number) (y string)) x) 10 "foo")
  (typed number 10))

(check
  ((lambda ((x number) (y number))
    (unchecked number (%+ x y))) 10 20)
  (typed number 30))

; --- identity

(define identity
  (unchecked
    (forall (x) (pi (x) x))
    (%lambda (x) x)))

(check
  (identity 123)
  (typed number 123))

; --- classes

(%define (%point=? $lhs $rhs)
  (%and
    (%= (%car $lhs) (%car $rhs))
    (%= (%cdr $lhs) (%cdr $rhs))))

(%define (%point->datum $point)
  (%quasiquote
    (point
      (%unquote (%car $point))
      (%unquote (%cdr $point)))))

(%define (%pair=? $car=? $cdr=? $lhs $rhs)
  (%and
    ($cdr=? (%car $lhs) (%car $rhs))
    ($cdr=? (%cdr $lhs) (%cdr $rhs))))

(%define (%pair->datum $car->datum $cdr->datum $pair)
  (%quasiquote
    (
      (%unquote ($car->datum (%car $pair)))
      (%unquote ($cdr->datum (%cdr $pair))))))

(define-class fx)
(define-class (point))
(define-class (list _))
(define-class (pair _ _))

; --- pairs

(define cons
  (unchecked
    (forall (a b) (pi (a b) (pair a b)))
    %cons))

(define car
  (unchecked
    (forall (a b) (pi ((pair a b)) a))
    %car))

(define cdr
  (unchecked
    (forall (a b) (pi ((pair a b)) b))
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
  (unchecked
    (forall (x) (list x))
    (%quote ())))

(check
  null
  (typed
    (forall ($0) (list $0))
    ()))

(define link
  (unchecked
    (forall (x) (pi (x (list x)) (list x)))
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

; --- booleans

(define boolean=? (unchecked (pi (boolean boolean) boolean) %boolean=?))

(define-macro and %compile-and)
(define-macro or %compile-or)

(check (boolean=? #t #t) (typed boolean #t))
(check (boolean=? #t #f) (typed boolean #f))

(check (and #t #t) (typed boolean #t))

; --- math

(define = (unchecked (pi (number number) boolean) %=))

(check (boolean=? (= 2 2) #t))
(check (boolean=? (= 2 3) #f))

(define + (unchecked (pi (number number) number) %+))

(check
  (+ my-number 10)
  (typed number 20))

; --- point

(define make-point (unchecked (pi (number number) point) %cons))
(define point-x (unchecked (pi (point) number) %car))
(define point-y (unchecked (pi (point) number) %cdr))

(check
  (point-x (make-point 10 20))
  (typed number 10))

(check
  (point-y (make-point 10 20))
  (typed number 20))

(define (point=? (p1 point) (p2 point))
  (and
    (= (point-x p1) (point-x p2))
    (= (point-y p1) (point-y p2))))

(check
  (point=?
    (make-point 10 (+ 10 10))
    (make-point 10 20))
  (typed boolean #t))

(check
  (point=?
    (make-point 10 20)
    (make-point 10 30))
  (typed boolean #f))

; --- equality

(define equal? (unchecked (forall (t) (pi (t t) boolean)) %equal?))

(check (equal? 10 10) (typed boolean #t))
(check (equal? 10 11) (typed boolean #f))
