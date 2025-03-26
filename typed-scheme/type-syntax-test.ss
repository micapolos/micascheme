(import
  (micascheme)
  (syntax lookup)
  (typed-scheme keywords)
  (typed-scheme type)
  (typed-scheme types)
  (typed-scheme type-syntax))

(define $lookup
  (lookup-with
    (a-boolean boolean-type-definition)
    (a-string string-type-definition)
    (a-number number-type-definition)
    (a-pair pair-type-definition)))

(define (test-syntax->type $lookup $scope $syntax)
  (syntax->type test-syntax->type $lookup $scope $syntax))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'a-boolean)
    boolean-type))

(check
  (raises
    (test-syntax->type $lookup (stack) #'(a-boolean a-boolean))))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'a-string)
    string-type))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'a-number)
    number-type))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'(a-pair a-string a-boolean))
    (pair-type string-type boolean-type)))

(check
  (raises
    (test-syntax->type $lookup (stack) #'a-pair)))

(check
  (raises
    (test-syntax->type $lookup (stack) #'(a-pair a-string))))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'(a-lambda (a-string a-boolean) a-number))
    (lambda-type 0 (immutable-vector string-type boolean-type) number-type)))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'(oneof a-string a-boolean))
    (union-type (immutable-vector string-type boolean-type))))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'(forall (a b) (a-pair a b)))
    (forall-type 2 (pair-type (variable-type 1) (variable-type 0)))))
