(library (typed-scheme type-syntax)
  (export
    syntax->type
    type->syntax
    type-definition->syntax)
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
    (syntax-case $syntax (any-lambda oneof forall)
      ((any-lambda (param ...) result)
        (lambda-type
          (syntaxes->types $recurse $lookup $scope (syntaxes param ...))
          ($recurse $lookup $scope #'result)))
      ((oneof item ...)
        (union-type
          (syntaxes->types $recurse $lookup $scope (syntaxes item ...))))
      ((forall (param ...) type)
        (lets
          ($params (map syntax->variance-identifier (syntaxes param ...)))
          ($variances (list->immutable-vector (map car $params)))
          ($scope (fold-left scope+ $scope (map cdr $params)))
          (forall-type $variances ($recurse $lookup $scope #'type))))
      (id
        (and (identifier? #'id) (scope-ref $scope #'id))
        (variable-type (scope-ref $scope #'id)))
      (id
        (and (identifier? #'id) (type-definition? ($lookup #'id)))
        (lets
          ($type-definition ($lookup #'id))
          ($arity (vector-length (type-definition-variances $type-definition)))
          (if (= $arity 0)
            (defined-type #f ($lookup #'id) (immutable-vector))
            (syntax-error #'id
              (format "expected ~s arguments in" $arity)))))
      ((id arg ...)
        (and (identifier? #'id) (type-definition? ($lookup #'id)))
        (lets
          ($type-definition ($lookup #'id))
          ($arity (vector-length (type-definition-variances $type-definition)))
          ($args (syntaxes arg ...))
          (if (= $arity (length $args))
            (defined-type #f ($lookup #'id)
              (syntaxes->types $recurse $lookup $scope (syntaxes arg ...)))
            (syntax-error #'id
              (format "expected ~s arguments in" $arity)))))
      (other
        (syntax-error #'other "invalid type"))))

  (define (syntax->variance-identifier $syntax)
    (syntax-case $syntax (in out)
      ((in id)
        (cons in-variance (identifier id)))
      ((out id)
        (cons out-variance (identifier id)))
      (id
        (cons inout-variance (identifier id)))))

  (define (type->syntax $value->syntax $id $type)
    (switch-exhaustive $type
      ((native-type? (native-type $value))
        #`(native-type #,($value->syntax $value)))
      ((defined-type? (defined-type $parent? $definition $args))
        #`(defined-type
          #,(if $parent?
            (type->syntax $value->syntax $id $parent?)
            (datum->syntax $id #f))
          #,(type-definition->syntax $id $definition)
          (immutable-vector #,@(map (partial type->syntax $value->syntax $id) (vector->list $args)))))
      ((lambda-type? (lambda-type $params $result))
        #`(lambda-type
          (immutable-vector #,@(map (partial type->syntax $value->syntax $id) (vector->list $params)))
          #,(type->syntax $value->syntax $id $result)))
      ((union-type? (union-type $items))
        #`(union-type
          (immutable-vector #,@(map (partial type->syntax $value->syntax $id) (vector->list $items)))))
      ((recursive-type? (recursive-type $type))
        #`(recursive-type
          #,(type->syntax $value->syntax $id $type)))
      ((variable-type? (variable-type $index))
        #`(variable-type
          #,(datum->syntax $id $index)))
      ((forall-type? (forall-type $variances $type))
        #`(forall-type
          (immutable-vector #,@(map variance->syntax (vector->list $variances)))
          #,(type->syntax $value->syntax $id $type)))))

  (define (type-definition->syntax $id (type-definition $parent? $gensym $name $variances))
    #`(type-definition
      #,(if $parent?
        (type-definition->syntax $id $parent?)
        (datum->syntax $id #f))
      '#,(datum->syntax $id $gensym)
      #,(datum->syntax $id $name)
      (immutable-vector
        #,@(map variance->syntax (vector->list $variances)))))

  (define (variance->syntax $variance)
    (switch-exhaustive $variance
      ((in-variance? _) #'in-variance)
      ((out-variance? _) #'out-variance)
      ((inout-variance? _) #'inout-variance)))
)
