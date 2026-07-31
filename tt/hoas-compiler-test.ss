(import
  (scheme)
  (check)
  (switch)
  (tt hoas-compiler)
  (tt lookup)
  (tt hoas)
  (prefix (tt keywords) %))

(define boolean-type (native 'boolean))
(define number-type (native 'number))
(define string-type (native 'string))
(define list-type (native 'list))
(define pair-type (native 'pair))

(define add-type
  (native-abstraction
    (lambda ($fn . $args) (native (apply $fn $args)))
    (native +)
    $0 $1))

(define inc-type
  (abstraction* $0
    (term-apply add-type $0 (native 1))))

(define test-lookup
  (identifier-lookup
    (boolean boolean-type)
    (number number-type)
    (string string-type)
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
    (native 'dupa)))

(check
  (type=?
    (compile-type test-lookup #'1)
    (native 1)))

(check
  (type=?
    (compile-type test-lookup #'%type)
    (universe 0)))

(check
  (type=?
    (compile-type test-lookup #'(%type 0))
    (universe 0)))

(check
  (type=?
    (compile-type test-lookup #'(%type 12))
    (universe 12)))

(check
  (raises
    (compile-type test-lookup #'(%type 12.3))))

(check
  (raises
    (compile-type test-lookup #'(%type -1))))

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
    (compile-type test-lookup #'(%lambda number))
    number-type))

(check
  (type=?
    (compile-type test-lookup #'(%lambda x (list x)))
    (abstraction
      (lambda ($arg)
        (term-apply list-type $arg)))))

(check
  (type=?
    (compile-type test-lookup #'(%lambda x y (pair x y)))
    (abstraction
      (lambda ($0)
        (abstraction
          (lambda ($1)
            (term-apply
              (term-apply pair-type $0)
              $1)))))))

(check
  (type=?
    (compile-type test-lookup #'(%lambda x (pair x x)))
    (abstraction
      (lambda ($0)
        (term-apply (term-apply pair-type $0) $0)))))

(check
  (type=?
    (compile-type test-lookup #'(%pi boolean))
    boolean-type))

(check
  (type=?
    (compile-type test-lookup #'(%pi number boolean))
    (arrow number-type boolean-type)))

(check
  (type=?
    (compile-type test-lookup #'(%pi number string boolean))
    (arrow number-type
      (arrow string-type
        boolean-type))))

(check
  (type=?
    (compile-type test-lookup #'+)
    add-type))

(check
  (type=?
    (compile-type test-lookup #'(+ 1))
    (term-apply add-type (native 1))))

(check
  (type=?
    (compile-type test-lookup #'(+ 1 2))
    (native 3)))

(check
  (type=?
    (compile-type test-lookup #'inc)
    inc-type))

(check
  (type=?
    (compile-type test-lookup #'(inc 1))
    (native 2)))

; --- compile-typed

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'10))
    '(typed number 10)))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%typed number foo)))
    '(typed number foo)))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%type number)))
    '(typed type number)))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%lambda 10)))
    '(typed number 10)))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%lambda (x number) x)))
    '(typed
      (arrow number number)
      (lambda (x) x))))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%lambda (x number) (y string) y)))
    '(typed
      (arrow number (arrow string string))
      (lambda (x) (lambda (y) y)))))

(check
  (equal?
    (typed->datum (compile-typed test-lookup #'(%typed (%pi number number number) +)))
    '(typed
      (arrow number (arrow number number))
      +)))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'((%typed (%pi number number number) +) 10)))
    '(typed
      (arrow number number)
      (+ 10))))

(check
  (equal?
    (typed->datum
      (compile-typed test-lookup
        #'((%typed (%pi number number number) +) 10 20)))
    '(typed
      number
      ((+ 10) 20))))
