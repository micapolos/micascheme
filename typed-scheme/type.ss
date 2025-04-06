(library (typed-scheme type)
  (export
    variance?
    in-variance
    in-variance?
    out-variance
    out-variance?
    inout-variance
    inout-variance?

    type-definition
    type-definition?
    type-definition-parent?
    type-definition-gensym
    type-definition-name
    type-definition-variances

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
    lambda-type-params
    lambda-type-result

    union-type
    union-type?
    union-type-items

    intersection-type
    intersection-type?
    intersection-type-types

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
    forall-type-variances
    forall-type-type

    recursive-type
    recursive-type?
    recursive-type-type

    type-definition-gensym=?

    type?
    type-assignable-to?
    type=?

    type+

    proc-scope-specialize?
    proc-scope-type-substitute)
  (import (micascheme))

  (data in-variance)
  (data out-variance)
  (data inout-variance)

  (data (type-definition parent? gensym name variances))

  (data (native-type value))
  (data (defined-type parent? definition arguments))
  (data (lambda-type params result))
  (data (union-type items))
  (data (intersection-type types))
  (data (record-type parent? gensym items))
  (data (variable-type index))
  (data (forall-type variances type))
  (data (recursive-type type))

  (data hole)
  (data (recursion scope type))

  (define (variance? $obj)
    (or
      (in-variance? $obj)
      (out-variance? $obj)
      (inout-variance? $obj)))

  (define (type? $obj)
    (or
      (native-type? $obj)
      (defined-type? $obj)
      (lambda-type? $obj)
      (union-type? $obj)
      (intersection-type? $obj)
      (recursive-type? $obj)
      (variable-type? $obj)
      (forall-type? $obj)
      (record-type? $obj)))

  (define (type-assignable-to? $type $to-type)
    (scope-type-assignable-to? (stack) $type $to-type))

  (define (type=? $type-a $type-b)
    (scope-type=? (stack) $type-a $type-b))

  (define (scope-type=? $scope $type-a $type-b)
    (and
      (scope-type-assignable-to? $scope $type-a $type-b)
      (scope-type-assignable-to? $scope $type-b $type-a)))

  ; TODO: Implement properly
  (define (scope-type-assignable-to? $scope $type $to-type)
    (switch $type
      ((intersection-type? (intersection-type $types))
        (for-all-with
          ($to-type (type-intersection-list $to-type))
          (exists-with
            ($type $types)
            (scope-type-assignable-to? $scope $type $to-type))))
      ((else $type)
        (switch $to-type
          ((native-type? (native-type $to-value))
            (switch? $type
              ((native-type? (native-type $value))
                ; Introduce $value-assignable-to?
                (equal? $value $to-value))))
          ((defined-type? (defined-type $to-parent? $to-definition $to-arguments))
            (switch? $type
              ((defined-type? (defined-type $parent? $definition $arguments))
                (cond
                  ((type-definition-gensym=? $definition $to-definition)
                    ; TODO: Implement variance, so it's possible to declare "in" and "out".
                    ; In any-lambda, implicit variable is "in" for params and "out" for result.
                    (for-all (partial scope-type=? $scope) $arguments) $to-arguments)
                  (else
                    (and $parent? (scope-type-assignable-to? $scope $parent? $to-type)))))))
          ((lambda-type? (lambda-type $to-params $to-result))
            (switch? $type
              ((lambda-type? (lambda-type $params $result))
                (and
                  (= (length $params) (length $to-params))
                  (for-all (partial scope-type-assignable-to? $scope) $to-params $params)
                  (scope-type-assignable-to? $scope $result $to-result)))))
          ((union-type? $to-union-type)
            (for-all
              (lambda ($type)
                (exists
                  (lambda ($to-type)
                    (scope-type-assignable-to? $scope $type $to-type))
                  (union-type-items $to-union-type)))
              (type-union-list $type)))
          ((intersection-type? (intersection-type $to-types))
            (for-all-with
              ($to-type $to-types)
              (scope-type-assignable-to? $scope $type $to-type)))
          ((forall-type? (forall-type $to-variances $to-type))
            (switch? $type
              ((forall-type? (forall-type $variances $type))
                (and
                  (equal? $variances $to-variances)
                  (scope-type-assignable-to?
                    (fold-left push $scope (map-with ($variance $variances) hole))
                    $type
                    $to-type)))))
          ((recursive-type? (recursive-type $to-type))
            (switch? $type
              ((recursive-type? (recursive-type $type))
                (scope-type-assignable-to?
                  (push $scope (recursion $scope $to-type))
                  $type
                  $to-type))))
          ((variable-type? (variable-type $to-index))
            (switch? $type
              ((variable-type? (variable-type $index))
                (and
                  (= $index $to-index)
                  (switch-exhaustive (list-ref $scope $index)
                    ((hole? _)
                      (switch? (list-ref $scope $to-index)
                        ((hole? _) #t)))
                    ((recursion? (recursion $scope $to-type))
                      (scope-type-assignable-to? $scope $type $to-type)))))))
          ((else $to-type)
            (todo))))))

  (define (proc-scope-specialize? $proc $scope $lhs-type $rhs-type)
    (switch-exhaustive $lhs-type
      ((native-type? (native-type $lhs-value))
        (switch? $rhs-type
          ((native-type? (native-type $rhs-value))
            (lets?
              ($value ($proc $scope $lhs-value $rhs-value))
              (native-type $value)))))
      ((defined-type? (defined-type $lhs-parent? $lhs-definition $lhs-arguments))
        (switch? $rhs-type
          ((defined-type (defined-type $rhs-parent? $rhs-definition $rhs-arguments))
            (cond
              ((type-definition-gensym=? $lhs-definition $rhs-definition)
                (fold-left?
                  (partial proc-scope-specialize? $proc)
                  $scope
                  $lhs-arguments
                  $rhs-arguments))
              (else
                (and $rhs-parent?
                  (proc-scope-specialize? $proc $scope $lhs-type $rhs-parent?)))))))
      ((lambda-type? (lambda-type $lhs-params $lhs-result))
        (switch? $rhs-type
          ((lambda-type? (lambda-type $rhs-params $rhs-result))
            (and
              (= (length $lhs-params) (length $rhs-params))
              (lets?
                ($scope
                  (fold-left?
                    (partial proc-scope-specialize? $proc)
                    $rhs-params
                    $lhs-params))
                (proc-scope-specialize? $proc $scope $lhs-result $rhs-result))))))))

  (define (proc-scope-variance-specialize? $proc $scope $variance $lhs-type $rhs-type)
    (switch-exhaustive $variance
      ((out-variance? _)
        (proc-scope-specialize? $proc $scope $lhs-type $rhs-type))
      ((in-variance? _)
        (proc-scope-specialize? $proc $scope $rhs-type $lhs-type))
      ((inout-variance? _)
        (lets?
          ($scope (proc-scope-specialize? $proc $scope $lhs-type $rhs-type))
          (proc-scope-specialize? $proc $scope $rhs-type $lhs-type)))))

  (define (type-union-list $type)
    (switch $type
      ((union-type? (union-type $items)) $items)
      ((else $other) (list $other))))

  (define (type-intersection-list $type)
    (switch $type
      ((intersection-type? (intersection-type $types)) $types)
      ((else $other) (list $other))))

  (define (type-union-list->type $type-union-list)
    (case (length $type-union-list)
      ((1) (car $type-union-list))
      (else (union-type $type-union-list))))

  (define (type-union-list+type $type-union-list $type)
    (cond
      ((exists (partial type-assignable-to? $type) $type-union-list)
        $type-union-list)
      (else
        (cons $type $type-union-list))))

  (define (type+ $type-a $type-b)
    (type-union-list->type
      (reverse
        (fold-left
          type-union-list+type
          (reverse (type-union-list $type-a))
          (type-union-list $type-b)))))

  (define (type-definition-gensym=? $type-definition-a $type-definition-b)
    (symbol=?
      (type-definition-gensym $type-definition-a)
      (type-definition-gensym $type-definition-b)))

  (define (proc-scope-type-substitute $proc $scope $type)
    (switch $type
      ((variable-type? (variable-type $index))
        (switch (list-ref? $scope $index)
          ((false? _) (variable-type $index))
          ((else $type) $type)))
      ((native-type? (native-type $value))
        (native-type ($proc $scope $value)))
      ((defined-type? (defined-type $parent $definition $arguments))
        (defined-type $parent $definition
          (map (partial proc-scope-type-substitute $proc $scope) $arguments)))
      ((lambda-type? (lambda-type $params $result))
        (lambda-type
          (map (partial proc-scope-type-substitute $proc $scope) $params)
          (proc-scope-type-substitute $proc $scope $result)))
      ((union-type? (union-type $items))
        (union-type (partial proc-scope-type-substitute $proc $scope) $items))
      ((forall-type (forall-type $variances $type))
        (forall-type $variances
          (proc-scope-type-substitute
            $proc
            (fold-left push $scope (map (lambda (_) #f) $variances))
            $type)))
      ((recursive-type? (recursive-type $type))
        (recursive-type
          (proc-scope-type-substitute
            $proc
            (push $scope #f)
            $type)))))
)
