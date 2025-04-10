(import
  (micascheme)
  (syntax lookup)
  (typed-scheme keywords)
  (typed-scheme type)
  (typed-scheme types)
  (typed-scheme type-syntax))

(define $lookup
  (lookup-with
    (any-boolean boolean-type-definition)
    (any-string string-type-definition)
    (any-number number-type-definition)
    (any-pair pair-type-definition)))

(define (test-syntax->type $lookup $scope $syntax)
  (syntax->type test-syntax->type $lookup $scope $syntax))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'any-boolean)
    boolean-type))

(check
  (raises
    (test-syntax->type $lookup (stack) #'(any-boolean any-boolean))))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'any-string)
    string-type))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'any-number)
    number-type))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'(any-pair any-string any-boolean))
    (pair-type string-type boolean-type)))

(check
  (raises
    (test-syntax->type $lookup (stack) #'any-pair)))

(check
  (raises
    (test-syntax->type $lookup (stack) #'(any-pair any-string))))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'(any-lambda (any-string any-boolean) any-number))
    (lambda-type (list string-type boolean-type) number-type)))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'(oneof any-string any-boolean))
    (union-type (list string-type boolean-type))))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'(forall (a b) (any-pair a b)))
    (forall-type
      (list inout-variance inout-variance)
      (pair-type (variable-type 1) (variable-type 0)))))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'(forall ((in a) (out b)) (any-pair a b)))
    (forall-type
      (list in-variance out-variance)
      (pair-type (variable-type 1) (variable-type 0)))))

(check
  (equal?
    (test-syntax->type $lookup (stack) #'(forall (a b) (any-lambda (a) b)))
    (forall-type
      (list inout-variance inout-variance)
      (lambda-type (list (variable-type 1)) (variable-type 0)))))
