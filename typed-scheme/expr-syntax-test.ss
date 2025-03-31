(import
  (micascheme)
  (syntax lookup)
  (typed-scheme type)
  (typed-scheme types)
  (typed-scheme expr)
  (typed-scheme keywords)
  (typed-scheme type-syntax)
  (typed-scheme expr-syntax))

(define $type-definition-lookup
  (lookup-with
    (a-boolean boolean-type-definition)
    (a-string string-type-definition)
    (a-number number-type-definition)
    (a-pair pair-type-definition)))

(define $type-lookup
  (lookup-with))

(define (test-syntax->type $type-definition-lookup $scope $syntax)
  (syntax->type
    test-syntax->type
    $type-definition-lookup
    $scope
    $syntax))

(define (test-syntax->expr $type-definition-lookup $type-lookup $type-scope $scope $syntax)
  (syntax-case $syntax ()
    (n
      (boolean? (datum n))
      (expr boolean-type (native-term (datum n))))
    (n
      (number? (datum n))
      (expr number-type (native-term (datum n))))
    (n
      (string? (datum n))
      (expr string-type (native-term (datum n))))
    (other
      (syntax->expr
        test-syntax->type
        test-syntax->expr
        $type-definition-lookup
        $type-lookup
        $type-scope
        $scope
        #'other))))

(check
  (equal?
    (test-syntax->expr $type-definition-lookup $type-lookup (stack) (stack) #'"foo")
    (expr string-type (native-term "foo"))))

(check
  (equal?
    (test-syntax->expr $type-definition-lookup $type-lookup (stack) (stack)
      #'(lambda ((a-string s) (a-boolean b)) s))
    (expr
      (lambda-type 0 (immutable-vector string-type boolean-type) string-type)
      (lambda-term
        (immutable-vector string-type boolean-type)
        (expr string-type (variable-term 1))))))

(check
  (equal?
    (test-syntax->expr $type-definition-lookup $type-lookup (stack) (stack)
      #'(if #t "foo" "bar"))
    (expr string-type
      (if-term
        (expr boolean-type (native-term #t))
        (expr string-type (native-term "foo"))
        (expr string-type (native-term "bar"))))))

(check
  (equal?
    (test-syntax->expr $type-definition-lookup $type-lookup (stack) (stack)
      #'(if #t "foo" 123))
    (expr
      (union-type (immutable-vector string-type number-type))
      (if-term
        (expr boolean-type (native-term #t))
        (expr string-type (native-term "foo"))
        (expr number-type (native-term 123))))))

; === expr->syntax

(check
  (equal?
    (pretty-datum
      (syntax->datum
        (expr->syntax #'id identity (stack)
          (expr string-type (native-term #'"foo")))))
    "foo"))

(check
  (equal?
    (pretty-datum
      (syntax->datum
        (expr->syntax #'id identity (stack #'v0 #'v1)
          (expr string-type (variable-term 0)))))
    'v1))

(check
  (equal?
    (pretty-datum
      (syntax->datum
        (expr->syntax #'id identity (stack #'v0 #'v1)
          (expr string-type (variable-term 1)))))
    'v0))


(check
  (equal?
    (pretty-datum
      (syntax->datum
        (expr->syntax #'id identity (stack #'v0 #'v1)
          (expr string-type
            (if-term
              (expr boolean-type (native-term #'cond))
              (expr string-type (native-term #'foo))
              (expr string-type (native-term #'bar)))))))
    '(if cond foo bar)))

(check
  (equal?
    (pretty-datum
      (syntax->datum
        (expr->syntax #'id identity (stack)
          (expr string-type
            (bind-term
              (list
                (expr string-type (native-term #'"foo"))
                (expr string-type (native-term #'"bar")))
              (expr string-type (variable-term 0)))))))
    '(let ((v0 "foo") (v1 "bar")) v1)))

(check
  (equal?
    (pretty-datum
      (syntax->datum
        (expr->syntax #'id identity (stack)
          (expr
            (lambda-type 0 (immutable-vector string-type string-type) string-type)
            (lambda-term
              (immutable-vector string-type string-type)
              (expr string-type (variable-term 0)))))))
    '(lambda (v0 v1) v1)))

(check
  (equal?
    (pretty-datum
      (syntax->datum
        (expr->syntax #'id identity (stack #'fn #'foo #'bar)
          (expr (native-type 'result)
            (application-term
              (expr
                (lambda-type 0
                  (immutable-vector (native-type 'p1) (native-type 'p2))
                  (native-type 'result))
                (variable-term 2))
              (list
                (expr (native-type 'p1) (variable-term 1))
                (expr (native-type 'p2) (variable-term 0))))))))
    '(fn foo bar)))
