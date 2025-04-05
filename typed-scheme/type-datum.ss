(library (typed-scheme type-datum)
  (export
    type->datum
    scope-type->datum)
  (import
    (micascheme)
    (typed-scheme type))

  (define (type->datum $type)
    (scope-type->datum (stack) $type))

  (define (scope-type->datum $scope $type)
    (switch-exhaustive $type
      ((native-type? (native-type $value))
        $value)
      ((defined-type? (defined-type $parent? $definition $args))
        (lets
          ($name (string->symbol (type-definition-name $definition)))
          ($args $args)
          (if (null? $args) $name `(,$name ,@(map scope-type->datum $scope $args)))))
      ((lambda-type? (lambda-type $params $result))
        `(any-lambda
          (,@(map*
            (partial scope-type->datum $scope)
            (partial scope-type->datum $scope)
            $params))
          ,(scope-type->datum $scope $result)))
      ((union-type? (union-type $items))
        `(oneof ,@(map (partial scope-type->datum $scope) $items)))
      ((recursive-type? (recursive-type $type))
        (todo))
      ((variable-type? (variable-type $index))
        (list-ref $scope $index))
      ((forall-type? (forall-type $variances $type))
        (lets
          ($gensyms (map (lambda (_) (gensym)) $variances))
          ($scope (fold-left push $scope $gensyms))
          `(forall (,@$gensyms)
            (scope-type->datum $scope $type))))))
)
