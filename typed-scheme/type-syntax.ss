(library (typed-scheme type-syntax)
  (export syntax->type)
  (import
    (micascheme)
    (syntax lookup)
    (typed-scheme keywords)
    (typed-scheme type)
    (typed-scheme types))

  (define (scope+ $scope $identifier)
    (push $scope $identifier))

  (define (scope-ref $scope $identifier)
    (scope-ref-from $scope $identifier 0))

  (define (scope-ref-from $scope $identifier $index)
    (switch $scope
      ((null? _)
        #f)
      ((pair? (pair $head $scope))
        (if (free-identifier=? $identifier $head)
          $index
          (scope-ref-from $scope $identifier (+ $index 1))))))

  (define (syntaxes->types $recurse $lookup $scope $syntaxes)
    (vector->immutable-vector
      (list->vector
        (map (partial $recurse $lookup $scope) $syntaxes))))

  (define (syntax->type $recurse $lookup $scope $syntax)
    (syntax-case $syntax (a-lambda oneof forall)
      ((a-lambda (forall type-param ...) (param ...) result)
        (lets
          ($type-params (syntaxes type-param ...))
          ($arity (length $type-params))
          ($scope (fold-left scope+ $scope $type-params))
          (lambda-type $arity
            (syntaxes->types $recurse $lookup $scope (syntaxes param ...))
            ($recurse $lookup $scope #'result))))
      ((a-lambda (param ...) result)
        (lambda-type 0
          (syntaxes->types $recurse $lookup $scope (syntaxes param ...))
          ($recurse $lookup $scope #'result)))
      ((oneof item ...)
        (union-type
          (syntaxes->types $recurse $lookup $scope (syntaxes item ...))))
      ((forall (param ...) type)
        (lets
          ($params (syntaxes param ...))
          ($arity (length $params))
          ($scope (fold-left scope+ $scope $params))
          (forall-type $arity ($recurse $lookup $scope #'type))))
      (id
        (and (identifier? #'id) (scope-ref $scope #'id))
        (variable-type (scope-ref $scope #'id)))
      (id
        (and (identifier? #'id) (type-definition? ($lookup #'id)))
        (lets
          ($type-definition ($lookup #'id))
          ($arity (type-definition-arity $type-definition))
          (if (= (type-definition-arity $type-definition) 0)
            (defined-type #f ($lookup #'id) (immutable-vector))
            (syntax-error #'id
              (format "expected ~s arguments in" $arity)))))
      ((id arg ...)
        (and (identifier? #'id) (type-definition? ($lookup #'id)))
        (lets
          ($type-definition ($lookup #'id))
          ($arity (type-definition-arity $type-definition))
          ($args (syntaxes arg ...))
          (if (= $arity (length $args))
            (defined-type #f ($lookup #'id)
              (syntaxes->types $recurse $lookup $scope (syntaxes arg ...)))
            (syntax-error #'id
              (format "expected ~s arguments in" $arity)))))
      (other
        (syntax-error #'other "invalid type"))))
)
