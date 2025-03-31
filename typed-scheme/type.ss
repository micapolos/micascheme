(library (typed-scheme type)
  (export
    type-definition
    type-definition?
    type-definition-parent?
    type-definition-gensym
    type-definition-name
    type-definition-arity

    defined-type
    defined-type?
    defined-type-parent?
    defined-type-definition
    defined-type-arguments

    native-type
    native-type?
    native-type-value

    lambda-type
    lambda-type?
    lambda-type-arity
    lambda-type-params
    lambda-type-result

    union-type
    union-type?
    union-type-items

    record-type
    record-type?
    record-type-rtd
    record-type-gensym
    record-type-items

    variable-type
    variable-type?
    variable-type-index

    forall-type
    forall-type?
    forall-type-arity
    forall-type-type

    recursive-type
    recursive-type?
    recursive-type-type

    type?
    type-assignable-to?

    type+)
  (import (micascheme))

  (data (type-definition parent? gensym name arity))

  (data (native-type value))
  (data (defined-type parent? definition arguments))
  (data (lambda-type arity params result))
  (data (union-type items))
  (data (record-type parent? gensym items))
  (data (variable-type index))
  (data (forall-type arity type))
  (data (recursive-type type))

  (data hole)
  (data (recursion scope type))

  (define (type? $obj)
    (or
      (native-type? $obj)
      (defined-type? $obj)
      (lambda-type? $obj)
      (union-type? $obj)
      (recursive-type? $obj)
      (variable-type? $obj)
      (forall-type? $obj)
      (record-type? $obj)))

  (define (type-assignable-to? $type $to-type)
    (scope-type-assignable-to? (stack) $type $to-type))

  ; TODO: Implement properly
  (define (scope-type-assignable-to? $scope $type $to-type)
    (switch $type
      ((lambda-type? (lambda-type $arity $params $result))
        (switch? $to-type
          ((lambda-type? (lambda-type $to-arity $to-params $to-result))
            (and
              (= $arity $to-arity)
              (= (vector-length $params) (vector-length $to-params))
              (for-all
                (partial scope-type-assignable-to? $scope)
                (vector->list $to-params)
                (vector->list $params))
              (scope-type-assignable-to? $scope $result $to-result)))))
      ((union-type? $union-type)
        (for-all
          (lambda ($type)
            (scope-type-assignable-to? $scope $type $to-type))
          (vector->list (union-type-items $union-type))))
      ((forall-type? (forall-type $arity $type))
        (switch? $to-type
          ((forall-type? (forall-type $to-arity $to-type))
            (and
              (= $arity $to-arity)
              (scope-type-assignable-to?
                (fold-left push $scope (make-list $arity hole))
                $type
                $to-type)))))
      ((recursive-type? (recursive-type $type))
        (switch? $to-type
          ((recursive-type? (recursive-type $to-type))
            (scope-type-assignable-to?
              (push $scope (recursion $scope $to-type))
              $type
              $to-type))))
      ((else $type)
        (switch $to-type
          ((union-type? $to-union-type)
            (exists
              (lambda ($to-type)
                (scope-type-assignable-to? $scope $type $to-type))
              (vector->list (union-type-items $to-union-type))))
          ((else $to-type)
            (equal? $type $to-type))))))

  (define (type-list $type)
    (switch $type
      ((union-type? $union-type)
        (vector->list (union-type-items $union-type)))
      ((else $other)
        (list $other))))

  (define (type-list->type $type-list)
    (case (length $type-list)
      ((1) (car $type-list))
      (else (union-type (list->immutable-vector $type-list)))))

  (define (type-list+type $type-list $type)
    (cond
      ((exists (partial type-assignable-to? $type) $type-list)
        $type-list)
      (else
        (cons $type $type-list))))

  (define (type+ $type-a $type-b)
    (type-list->type
      (reverse
        (fold-left
          type-list+type
          (reverse (type-list $type-a))
          (type-list $type-b)))))
)
