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
        (syntax-error $identifier "not bound"))
      ((pair? (pair $head $scope))
        (if (free-identifier=? $identifier $head)
          $index
          (scope-ref-from $scope $identifier (+ $index 1))))))

  (define (syntax->type $scope $syntax)
    (syntax-case $syntax (a-boolean a-string a-number)
      (a-boolean #'boolean-type)
      (a-string #'string-type)
      (a-number #'number-type)
      (other (core-syntax->type syntax->type $scope #'other))))

  (define (core-syntax->type $recurse $scope $syntax)
    (syntax-case $syntax (a-lambda oneof forall)
      ((a-lambda (param ...) result)
        #`(lambda-type
          (immutable-vector #,@(map (partial $recurse $scope) (syntaxes param ...)))
          #,($recurse $scope #'result)))
      ((oneof item ...)
        #`(union-type
          (immutable-vector #,@(map (partial $recurse $scope) (syntaxes item ...)))))
      ((forall (param ...) type)
        (lets
          ($params (syntaxes param ...))
          ($arity (length $params))
          ($scope (fold-left scope+ $scope $params))
          #`(forall-type
            #,(datum->syntax #'forall $arity)
            #,($recurse $scope #'type))))
      (id
        (identifier? #'id)
        #`(variable-type
          #,(datum->syntax #'id (scope-ref $scope #'id))))
      (other
        (syntax-error #'other "invalid type"))))
)
