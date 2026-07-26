(import
  (scheme)
  (check)
  (tt hoas-compiler)
  (tt lookup)
  (tt hoas)
  (prefix (tt keywords) %))

(define boolean-type (native 'boolean))
(define number-type (native 'number))
(define string-type (native 'string))
(define list-type (native 'list))
(define pair-type (native 'pair))

(define test-lookup
  (identifier-lookup
    (boolean boolean-type)
    (number number-type)
    (string string-type)
    (list list-type)
    (pair pair-type)))

(check
  (raises
    (compile-type test-lookup #'dupa)))

(check
  (type=?
    (compile-type test-lookup #'%type)
    (universe 0)))

(check
  (type=?
    (compile-type test-lookup #'number)
    number-type))

(check
  (type=?
    (compile-type test-lookup #'(number))
    number-type))

(check
  (type=?
    (compile-type test-lookup #'list)
    list-type))

(check
  (type=?
    (compile-type test-lookup #'(list number))
    (application list-type number-type)))

(check
  (type=?
    (compile-type test-lookup #'(pair number string))
    (application
      (application pair-type number-type)
      string-type)))

(check
  (type=?
    (compile-type test-lookup #'(%forall number))
    number-type))

(check
  (type=?
    (compile-type test-lookup #'(%forall x (list x)))
    (abstraction
      (lambda ($arg)
        (term-apply list-type $arg)))))

(check
  (type=?
    (compile-type test-lookup #'(%forall x y (pair x y)))
    (abstraction
      (lambda ($0)
        (abstraction
          (lambda ($1)
            (term-apply
              (term-apply pair-type $0)
              $1)))))))

(check
  (type=?
    (compile-type test-lookup #'(%forall x (pair x x)))
    (abstraction
      (lambda ($0)
        (term-apply (term-apply pair-type $0) $0)))))

(check
  (type=?
    (compile-type test-lookup #'(%lambda boolean))
    boolean-type))

(check
  (type=?
    (compile-type test-lookup #'(%lambda number boolean))
    (arrow number-type boolean-type)))

(check
  (type=?
    (compile-type test-lookup #'(%lambda number string boolean))
    (arrow number-type
      (arrow string-type
        boolean-type))))
