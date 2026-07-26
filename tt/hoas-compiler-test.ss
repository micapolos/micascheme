(import
  (scheme)
  (check)
  (tt hoas-compiler)
  (tt lookup)
  (tt hoas)
  (prefix (tt keywords) %))

(define boolean-declaration (declaration 'boolean 0))
(define number-declaration (declaration 'number 0))
(define string-declaration (declaration 'string 0))
(define list-declaration (declaration 'list 1))
(define pair-declaration (declaration 'pair 2))

(define boolean-type (class boolean-declaration (list)))
(define number-type (class number-declaration (list)))
(define string-type (class string-declaration (list)))
(define (list-type $element) (class list-declaration (list $element)))
(define (pair-type $car $cdr) (class pair-declaration (list $car $cdr)))

(define boolean-type-term (native boolean-type))
(define number-type-term (native number-type))
(define string-type-term (native string-type))
(define (list-type-term $element) (native (list-type $element)))
(define (pair-type-term $car $cdr) (native (pair-type $car $cdr)))

(define test-lookup
  (identifier-lookup
    (boolean boolean-declaration)
    (number number-declaration)
    (string string-declaration)
    (list list-declaration)
    (pair pair-declaration)))

(check
  (raises
    (compile-type-term test-lookup #'dupa)))

(check
  (type-term=? 0
    (compile-type-term test-lookup #'number)
    (native number-type)))

(check
  (type-term=? 0
    (compile-type-term test-lookup #'(number))
    (native number-type)))

(check
  (raises
    (compile-type-term test-lookup #'(number number))))

(check
  (type-term=? 0
    (compile-type-term test-lookup #'(list number))
    (list-type-term number-type-term)))

(check
  (raises
    (compile-type-term test-lookup #'list)))

(check
  (raises
    (compile-type-term test-lookup #'(list))))

(check
  (type-term=? 0
    (compile-type-term test-lookup #'(%forall number))
    number-type-term))

(check
  (type-term=? 0
    (compile-type-term test-lookup #'(%forall x (list x)))
    (abstraction
      (lambda ($arg)
        (list-type-term $arg)))))

(check
  (type-term=? 0
    (compile-type-term test-lookup #'(%forall x y (pair x y)))
    (abstraction
      (lambda ($0)
        (abstraction
          (lambda ($1)
            (pair-type-term $0 $1)))))))

(check
  (type-term=? 0
    (compile-type-term test-lookup #'(%forall x (pair x x)))
    (abstraction
      (lambda ($0)
        (pair-type-term $0 $0)))))

(check
  (type-term=? 0
    (compile-type-term test-lookup #'(%lambda number string boolean))
    (native
      (arrow
        (list number-type-term string-type-term)
        (list boolean-type-term)))))

(check
  (type-term=? 0
    (compile-type-term test-lookup #'(%lambda number string %void))
    (native
      (arrow
        (list number-type-term string-type-term)
        (list)))))

(check
  (type-term=? 0
    (compile-type-term test-lookup #'(%lambda number string (%values boolean string)))
    (native
      (arrow
        (list number-type-term string-type-term)
        (list boolean-type-term string-type-term)))))
