(import
  (scheme)
  (check)
  (switch)
  (syntax)
  (tt hoas-compiler)
  (tt lookup)
  (tt hoas)
  (tt primitive)
  (prefix (tt keywords) %))

(define point-declaration (generate-declaration "point" 0))
(define list-declaration (generate-declaration "list" 1))
(define pair-declaration (generate-declaration "pair" 2))

(define test-lookup
  (lookup
    (point point-declaration)
    (list list-declaration)
    (pair pair-declaration)))

(check
  (raises
    (compile-type test-lookup #'dupa)))

(check
  (type=?
    (compile-type test-lookup #'%number)
    number-type))

(check
  (type=?
    (compile-type test-lookup #'(%number))
    number-type))

(check
  (type=?
    (compile-type test-lookup #'(%tuple %number %string))
    (tuple (list number-type string-type))))

(check
  (type=?
    (compile-type test-lookup #'(%union %number %string))
    (union (list number-type string-type))))

(check
  (type=?
    (compile-type test-lookup #'point)
    (class point-declaration (list))))

(check
  (raises
    (compile-type test-lookup #'list)))

(check
  (type=?
    (compile-type test-lookup #'(list %number))
    (class list-declaration (list number-type))))

(check
  (type=?
    (compile-type test-lookup #'(pair %number %string))
    (class pair-declaration (list number-type string-type))))

(check
  (type=?
    (compile-type test-lookup #'(%forall () %number))
    number-type))

(check
  (type=?
    (compile-type test-lookup #'(%forall (x) (list x)))
    (abstraction
      (lambda ($arg)
        (class list-declaration (list $arg))))))

(check
  (type=?
    (compile-type test-lookup #'(%forall (x y) (pair x y)))
    (abstraction
      (lambda ($0)
        (abstraction
          (lambda ($1)
            (class pair-declaration (list $0 $1))))))))

(check
  (type=?
    (compile-type test-lookup #'(%forall (x) (pair x x)))
    (abstraction
      (lambda ($0)
        (class pair-declaration (list $0 $0))))))

(check
  (type=?
    (compile-type test-lookup #'(%pi () %boolean))
    (arrow (list) #f boolean-type)))

(check
  (type=?
    (compile-type test-lookup #'(%pi (%number) %boolean))
    (arrow (list number-type) #f boolean-type)))

(check
  (type=?
    (compile-type test-lookup #'(%pi (%number %string) %boolean))
    (arrow (list number-type string-type) #f boolean-type)))

(check
  (type=?
    (compile-type test-lookup #'(%pi (%number %string %...) %boolean))
    (arrow (list number-type) string-type boolean-type)))

(check
  (type=?
    (compile-type test-lookup #'(%forall (x) x))
    (abstraction
      (lambda ($0)
        $0))))

; --- compile-typed

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'10))
    '(typed number 10)))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%unchecked %number foo)))
    '(typed number foo)))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%lambda ((x %number)) x)))
    '(typed
      (pi (number) number)
      (lambda (x) x))))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%lambda ((x %number) (y %string)) y)))
    '(typed
      (pi (number string) string)
      (lambda (x y) y))))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%lambda ((x %number) (y %string %...)) y)))
    '(typed
      (pi (number string ...) string)
      (lambda (x . y) y))))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%forall (t) (%lambda ((x t)) x))))
    '(typed
      (forall ($0) (pi ($0) $0))
      (lambda (x) x))))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%unchecked (%pi (%number %number) %number) +)))
    '(typed
      (pi (number number) number)
      +)))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'((%unchecked (%pi (%number %number) %number) +) 10 20)))
    '(typed
      number
      (+ 10 20))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%quote (+ 10 20))))
    '(typed
      datum
      '(+ 10 20))))

; === tuple-constructor

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-constructor 0)))
    '(typed
      (tuple)
      (lambda () '()))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-constructor 1)))
    '(typed
      (forall ($0) (tuple $0))
      (lambda (x) x))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-constructor 2)))
    '(typed
      (forall ($0 $1) (tuple $0 $1))
      cons)))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-constructor 3)))
    '(typed
      (forall ($0 $1 $2) (tuple $0 $1 $2))
      vector)))

; === tuple-ref

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-ref (%unchecked (%tuple %number) x) 0)))
    '(typed number x)))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-ref (%unchecked (%tuple %number %string) x) 0)))
    '(typed number (car x))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-ref (%unchecked (%tuple %number %string) x) 1)))
    '(typed string (cdr x))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-ref (%unchecked (%tuple %number %string %boolean) x) 0)))
    '(typed number (vector-ref x 0))))

; === union

(check (raises (compile-typed test-lookup #'(%union "not-arity" 0 "foo"))))
(check (raises (compile-typed test-lookup #'(%union -1 0 "foo"))))
(check (raises (compile-typed test-lookup #'(%union 3 "not index" "foo"))))
(check (raises (compile-typed test-lookup #'(%union 3 -1 "foo"))))
(check (raises (compile-typed test-lookup #'(%union 3 3 "foo"))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%union 1 0 "foo")))
    '(typed (union string) identity)))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%union 2 0 "foo")))
    '(typed
      (forall ($0) (union string $0))
      (lambda (v) (cons #t "foo")))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%union 2 1 "foo")))
    '(typed
      (forall ($0) (union $0 string))
      (lambda (v) (cons #f "foo")))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%union 3 0 "foo")))
    '(typed
      (forall ($0 $1) (union string $0 $1))
      (lambda (v) (cons 0 "foo")))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%union 3 1 "foo")))
    '(typed
      (forall ($0 $1) (union $0 string $1))
      (lambda (v) (cons 1 "foo")))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%union 3 2 "foo")))
    '(typed
      (forall ($0 $1) (union $0 $1 string))
      (lambda (v) (cons 2 "foo")))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%if #f 10 20)))
    '(typed
      number
      (if #f 10 20))))

(check
  (raises
    (compile-typed test-lookup
      #'(%if #f 10 "foo"))))

(check
  (raises
    (compile-typed test-lookup
      #'(%if 10 20 30))))
