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

  (define (core-syntax->type $recurse $lookup $scope $syntax)
    (syntax-case $syntax (a-lambda oneof forall)
      ((a-lambda (param ...) result)
        #`(lambda-type
          (immutable-vector #,@(map (partial $recurse $lookup $scope) (syntaxes param ...)))
          #,($recurse $lookup $scope #'result)))
      ((oneof item ...)
        #`(union-type
          (immutable-vector #,@(map (partial $recurse $lookup $scope) (syntaxes item ...)))))
      ((forall (param ...) type)
        (lets
          ($params (syntaxes param ...))
          ($arity (length $params))
          ($scope (fold-left scope+ $scope $params))
          #`(forall-type
            #,(datum->syntax #'forall $arity)
            #,($recurse $lookup $scope #'type))))
      (id
        (identifier? #'id)
        (lets
          ($index? (scope-ref $scope #'id))
          (cond
            ($index?
              #`(variable-type
                #,(datum->syntax #'id $index?)))
            (($lookup #'id)
              #'id)
            (else (syntax-error #'id "undefined")))))
      ((id arg ...)
        (identifier? #'id)
        (cond
          (($lookup #'id)
            #`(id #,@(map (partial $recurse $lookup $scope) (syntaxes arg ...))))
          (else (syntax-error #'id "undefined"))))
      (other
        (syntax-error #'other "invalid type"))))
)
