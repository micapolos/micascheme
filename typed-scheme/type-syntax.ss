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

  (define (syntax->type $lookup $scope $syntax)
    (core-syntax->type syntax->type $lookup $scope $syntax))

  (define (syntaxes->types $recurse $lookup $scope $syntaxes)
    (vector->immutable-vector
      (list->vector
        (map (partial $recurse $lookup $scope) $syntaxes))))

  (define (core-syntax->type $recurse $lookup $scope $syntax)
    (syntax-case $syntax (a-lambda oneof forall)
      ((a-lambda (param ...) result)
        (lambda-type
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
        (and (identifier? #'id) ($lookup #'id))
        (defined-type #f ($lookup #'id) (immutable-vector)))
      ((id arg ...)
        (and (identifier? #'id) ($lookup #'id))
        (defined-type #f ($lookup #'id)
          (syntaxes->types $recurse $lookup $scope (syntaxes arg ...))))
      (other
        (syntax-error #'other "invalid type"))))
)
