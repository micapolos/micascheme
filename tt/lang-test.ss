(import
  (tt lang)
  (prefix (scheme) %)
  (prefix (boolean) %)
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

(define false? (unchecked (pi (boolean) boolean) %false?))
(define true? (unchecked (pi (boolean) boolean) %not-false?))

(check (boolean=? #t #t) (typed boolean #t))
(check (boolean=? #t #f) (typed boolean #f))

(check (and #t #t) (typed boolean #t))

; --- number

(define = (unchecked (pi (number number) boolean) %=))
(define < (unchecked (pi (number number) boolean) %<))
(define + (unchecked (pi (number number) number) %+))
(define - (unchecked (pi (number number) number) %-))
(define increment (unchecked (pi (number) number) (%lambda (x) (%+ x 1))))

(check (= 2 2))
(check (not (= 2 3)))
(check (< 10 20))
(check (not (< 10 10)))
(check (= (+ 1 2) 3))
(check (= (- 3 2) 1))
(check (= (increment 10) 11))

; --- char

(define char=? (unchecked (pi (char char) boolean) %char=?))
(define char->number (unchecked (pi (char) number) %char->integer))

(check (char=? #\a #\a))
(check (not (char=? #\a #\b)))
(check (= (char->number #\space) #x20))

; --- string

(define string=? (unchecked (pi (string string) boolean) %string=?))
(define string (unchecked (pi (char ...) string) %string))
(define string-append (unchecked (pi (string ...) string) %string-append))
(define string-length (unchecked (pi (string) number) %string-length))

(check (string=? "foo" "foo"))
(check (not (string=? "foo" "bar")))
(check (string=? (string) ""))
(check (string=? (string #\a #\b #\c) "abc"))
(check (string=? (string-append) ""))
(check (string=? (string-append "a" "b" "c") "abc"))
(check (= (string-length "foo") 3))

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

; --- tuple

(check (= ((tuple-accessor 1 0) ((tuple-constructor 1) 10)) 10))

(check (= ((tuple-accessor 2 0) ((tuple-constructor 2) 10 "foo")) 10))
(check (string=? ((tuple-accessor 2 1) ((tuple-constructor 2) 10 "foo")) "foo"))

(check (= ((tuple-accessor 3 0) ((tuple-constructor 3) 10 "foo" #\a)) 10))
(check (string=? ((tuple-accessor 3 1) ((tuple-constructor 3) 10 "foo" #\a)) "foo"))
(check (char=? ((tuple-accessor 3 2) ((tuple-constructor 3) 10 "foo" #\a)) #\a))

; --- choice

(check
  (=
    ((choice-matcher 1)
      ((choice-constructor 1 0) 10)
      increment)
    11))

(check
  (=
    ((choice-matcher 2)
      ((choice-constructor 2 0) 10)
      increment string-length)
    11))

(check
  (=
    ((choice-matcher 2)
      ((choice-constructor 2 1) "foo")
      increment string-length)
    3))

(check
  (=
    ((choice-matcher 3)
      ((choice-constructor 3 0) 10)
      increment string-length char->number)
    11))

(check
  (=
    ((choice-matcher 3)
      ((choice-constructor 3 1) "foo")
      increment string-length char->number)
    3))

(check
  (=
    ((choice-matcher 3)
      ((choice-constructor 3 2) #\space)
      increment string-length char->number)
    #x20))

; --- is? typeof

(check (true? (is? number 10)))
(check (false? (is? number "foo")))

(check (true? (is? (typeof (+ 1 2)) 2)))
(check (false? (is? (typeof (+ 1 2)) "foo")))

; --- let

(check (= (let ((x 10) (y 20)) (+ x y)) 30))
(check (fails (let ((x 10) (y x)) (+ x y))))

(check (= (lets (x 10) (y 20) (+ x y)) 30))
(check (= (lets (x 10) (y (+ x 10)) (+ x y)) 30))

; --- fibonacci

(check
  (=
    (lets
      (fib
        (lambda (fib number) ((n number))
          (if (< n 2)
            n
            (+ (fib (- n 1)) (fib (- n 2))))))
      (fib 10))
    55))
