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
  (identifier-lookup
    (point point-declaration)
    (list list-declaration)
    (pair pair-declaration)))

(check
  (raises
    (compile-type test-lookup #'dupa)))

(check
  (type=?
    (compile-type test-lookup #'%type)
    universe))

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
    (compile-type test-lookup #'(%lambda %number))
    number-type))

(check
  (type=?
    (compile-type test-lookup #'(%lambda x (list x)))
    (abstraction
      (lambda ($arg)
        (class list-declaration (list $arg))))))

(check
  (type=?
    (compile-type test-lookup #'(%lambda x y (pair x y)))
    (abstraction
      (lambda ($0)
        (abstraction
          (lambda ($1)
            (class pair-declaration (list $0 $1))))))))

(check
  (type=?
    (compile-type test-lookup #'(%lambda x (pair x x)))
    (abstraction
      (lambda ($0)
        (class pair-declaration (list $0 $0))))))

(check
  (type=?
    (compile-type test-lookup #'(%pi %boolean))
    (arrow (list) boolean-type)))

(check
  (type=?
    (compile-type test-lookup #'(%pi %number %boolean))
    (arrow (list number-type) boolean-type)))

(check
  (type=?
    (compile-type test-lookup #'(%pi %number %string %boolean))
    (arrow (list number-type string-type) boolean-type)))

; --- compile-typed

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'10))
    '(typed number 10)))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%typed %number foo)))
    '(typed number foo)))

; (check
;   (equal?
;     (typed->datum (compile-typed test-lookup #'(%type %number)))
;     '(typed type number)))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%lambda (x %number) x)))
    '(typed
      (pi number number)
      (lambda (x) x))))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%lambda (x %number) (y %string) y)))
    '(typed
      (pi number string string)
      (lambda (x y) y))))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%typed (%pi %number %number %number) +)))
    '(typed
      (pi number number number)
      +)))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'((%typed (%pi %number %number %number) +) 10 20)))
    '(typed
      number
      (+ 10 20))))
