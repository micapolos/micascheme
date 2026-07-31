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

(define list-type (atomic #''list 'list))
(define pair-type (atomic #''pair 'pair))

(define add-type
  (native-abstraction
    primitive-apply-term
    (atomic #'+ +)
    $0 $1))

(define inc-type
  (abstraction* $0
    (term-apply add-type $0 (literal->atomic 1))))

(define test-lookup
  (identifier-lookup
    (list list-type)
    (pair pair-type)
    (+ add-type)
    (inc inc-type)))

(check
  (raises
    (compile-type test-lookup #'dupa)))

(check
  (type=?
    (compile-type test-lookup #'(%quote dupa))
    (atomic #''dupa 'dupa)))

(check
  (type=?
    (compile-type test-lookup #'1)
    (literal->atomic 1)))

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

; (check
;   (type=?
;     (compile-type test-lookup #'list)
;     list-type))

; (check
;   (type=?
;     (compile-type test-lookup #'(list number))
;     (application list-type number-type)))

; (check
;   (type=?
;     (compile-type test-lookup #'(pair number string))
;     (application
;       (application pair-type number-type)
;       string-type)))

(check
  (type=?
    (compile-type test-lookup #'(%lambda %number))
    number-type))

; (check
;   (type=?
;     (compile-type test-lookup #'(%lambda x (list x)))
;     (abstraction
;       (lambda ($arg)
;         (term-apply list-type $arg)))))

; (check
;   (type=?
;     (compile-type test-lookup #'(%lambda x y (pair x y)))
;     (abstraction
;       (lambda ($0)
;         (abstraction
;           (lambda ($1)
;             (term-apply
;               (term-apply pair-type $0)
;               $1)))))))

; (check
;   (type=?
;     (compile-type test-lookup #'(%lambda x (pair x x)))
;     (abstraction
;       (lambda ($0)
;         (term-apply (term-apply pair-type $0) $0)))))

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

(check
  (type=?
    (compile-type test-lookup #'+)
    add-type))

(check
  (type=?
    (compile-type test-lookup #'(+ 1))
    (term-apply add-type (atomic #'1 1))))

(check
  (type=?
    (compile-type test-lookup #'(+ 1 2))
    (literal->atomic 3)))

(check
  (type=?
    (compile-type test-lookup #'inc)
    inc-type))

(check
  (type=?
    (compile-type test-lookup #'(inc 1))
    (literal->atomic 2)))

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
