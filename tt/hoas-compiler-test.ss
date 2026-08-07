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

(define point-class (class point-declaration (list)))
(define (list-class $item) (class list-declaration (list $item)))
(define (pair-class $car $cdr) (class pair-declaration (list $car $cdr)))

(define test-lookup
  (lookup
    (tt (typed (kind 1) (kind 0)))
    (point point-declaration)
    (list list-declaration)
    (pair pair-declaration)
    (point-t (typed (kind 0) point-class))
    (list-t
      (typed
        (product (kind 0)
          (lambda (_) (kind 0)))
        (abstraction
          (lambda ($arg)
            (list-class $arg)))))
    (pair-t
      (typed
        (product (kind 0)
          (lambda (_)
            (product (kind 0)
              (lambda (_) (kind 0)))))
        (abstraction
          (lambda ($car)
            (abstraction
              (lambda ($cdr)
                (pair-class $car $cdr)))))))))

; === compile-typed-type

(check
  (typed-type->datum
    (compile-typed-type test-lookup #'(%kind 3)))
  (typed-type->datum
    (typed (kind 4) (kind 3))))

(check
  (raises
    (compile-typed-type test-lookup #'dupa)))

(check
  (equal?
    (typed-type->datum
      (compile-typed-type test-lookup #'point-t))
    (typed-type->datum
      (typed (kind 0) point-class))))

(check
  (equal?
    (typed-type->datum
      (compile-typed-type test-lookup #'(list-t %number)))
    (typed-type->datum
      (typed (kind 0) (list-class number-type)))))

(check
  (equal?
    (typed-type->datum
      (compile-typed-type test-lookup #'(pair-t %number %boolean)))
    (typed-type->datum
      (typed (kind 0) (pair-class number-type boolean-type)))))

(check
  (equal?
    (typed-type->datum
      (compile-typed-type test-lookup #'(%lambda () %number)))
    (typed-type->datum
      (typed (kind 0) number-type))))

(check
  (equal?
    (typed-type->datum
      (compile-typed-type test-lookup #'(%lambda ((t (%kind 0))) t)))
    (typed-type->datum
      (typed
        (product (kind 0)
          (lambda (_) (kind 0)))
        (abstraction
          (lambda ($0) $0))))))

(check
  (equal?
    (typed-type->datum
      (compile-typed-type test-lookup #'(%lambda ((t1 (%kind 0)) (t2 (%kind 0))) t2)))
    (typed-type->datum
      (typed
        (product (kind 0)
          (lambda (_)
            (product (kind 0)
              (lambda (_)
                (kind 0)))))
        (abstraction
          (lambda ($0)
            (abstraction
              (lambda ($1) $1))))))))

; === compile-type

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
    (compile-type test-lookup #'(%typeof 10))
    number-type))

(check
  (type=?
    (compile-type test-lookup #'(%tuple %number %string))
    (tuple (list number-type string-type))))

(check
  (type=?
    (compile-type test-lookup #'(%choice %number %string))
    (choice (list number-type string-type))))

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
    (typed->datum (compile-typed test-lookup #'(%is? %number 10)))
    '(typed boolean #t)))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%is? %number "foo")))
    '(typed boolean #f)))

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
    (typed->datum
      (compile-typed test-lookup
        #'(%lambda (repeat %string) ((x %number)) (repeat x))))
    '(typed
      (pi (number) string)
      (letrec
        ((repeat (lambda (x) (repeat x))))
        repeat))))

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
      (pi () (tuple))
      (lambda () '()))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-constructor 1)))
    '(typed
      (forall ($0) (pi ($0) (tuple $0)))
      (lambda (x) x))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-constructor 2)))
    '(typed
      (forall ($0 $1) (pi ($0 $1) (tuple $0 $1)))
      cons)))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-constructor 3)))
    '(typed
      (forall ($0 $1 $2) (pi ($0 $1 $2) (tuple $0 $1 $2)))
      vector)))

; === tuple-accessor

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-accessor 1 0)))
    '(typed
      (forall ($0) (pi ((tuple $0)) $0))
      (lambda (x) x))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-accessor 2 0)))
    '(typed
      (forall ($0 $1) (pi ((tuple $0 $1)) $0))
      car)))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-accessor 2 1)))
    '(typed
      (forall ($0 $1) (pi ((tuple $0 $1)) $1))
      cdr)))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-accessor 3 0)))
    '(typed
      (forall ($0 $1 $2) (pi ((tuple $0 $1 $2)) $0))
      (lambda (x) (vector-ref x 0)))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-accessor 3 1)))
    '(typed
      (forall ($0 $1 $2) (pi ((tuple $0 $1 $2)) $1))
      (lambda (x) (vector-ref x 1)))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%tuple-accessor 3 2)))
    '(typed
      (forall ($0 $1 $2) (pi ((tuple $0 $1 $2)) $2))
      (lambda (x) (vector-ref x 2)))))

; === choice-constructor

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%choice-constructor 1 0)))
    '(typed
      (forall ($0) (pi ($0) (choice $0)))
      (lambda (x) x))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%choice-constructor 2 0)))
    '(typed
      (forall ($0 $1) (pi ($0) (choice $0 $1)))
      (lambda (x) (cons #t x)))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%choice-constructor 2 1)))
    '(typed
      (forall ($0 $1) (pi ($1) (choice $0 $1)))
      (lambda (x) (cons #f x)))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%choice-constructor 3 0)))
    '(typed
      (forall ($0 $1 $2) (pi ($0) (choice $0 $1 $2)))
      (lambda (x) (cons 0 x)))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%choice-constructor 3 1)))
    '(typed
      (forall ($0 $1 $2) (pi ($1) (choice $0 $1 $2)))
      (lambda (x) (cons 1 x)))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%choice-constructor 3 2)))
    '(typed
      (forall ($0 $1 $2) (pi ($2) (choice $0 $1 $2)))
      (lambda (x) (cons 2 x)))))

; === choice-matcher

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%choice-matcher 1)))
    '(typed
      (forall ($0 $1)
        (pi ((choice $1) (pi ($1) $0)) $0))
      (lambda (x f) (f x)))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%choice-matcher 2)))
    '(typed
      (forall ($0 $1 $2) (pi ((choice $1 $2) (pi ($1) $0) (pi ($2) $0)) $0))
      (lambda (x f0 f1) ((if (car x) f0 f1) (cdr x))))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'(%choice-matcher 3)))
    '(typed
      (forall ($0 $1 $2 $3) (pi ((choice $1 $2 $3) (pi ($1) $0) (pi ($2) $0) (pi ($3) $0)) $0))
      (lambda (x f0 f1 f2) ((index-switch (car x) f0 f1 f2) (cdr x))))))

; === if

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
