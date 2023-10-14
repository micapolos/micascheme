(library (tico term)
  (export
    anything anything?
    any-boolean any-boolean?
    any-number any-number?
    any-string any-string?
    any-list any-list? any-list-item
    any-function any-function? any-function-params any-function-result
    struct struct? struct-name struct-items
    static static? static-value

    term-matches?)
  (import (micascheme))

  (data (anything))
  (data (nothing))
  (data (any-boolean))
  (data (any-number))
  (data (any-string))
  (data (any-list item))
  (data (any-function params result))
  (data (struct name items))
  (data (struct-get target type))
  (data (variable type))
  (data (function params body))
  (data (application target args))

  (data (evaluated type term))
  (data (thunk scope function))

  (define (scope-ref $scope $pattern)
    (switch $scope
      ((null? _)
        (throws `not-bound $pattern))
      ((else $pair)
        (unpair $pair $term $scope
          (cond
            ((type-matches? $evaluated $) $term)
            (else (scope-ref $terms $pattern)))))))

  (define (term-matches? $term $pattern)
    (switch $pattern
      ((anything? _) #t)
      ((any-boolean? _)
        (or
          (boolean? $term)
          (any-boolean? $term)))
      ((any-number? _)
        (or
          (number? $term)
          (any-number? $term)))
      ((any-string? _)
        (or
          (string? $term)
          (any-string? $term)))
      ((else $other)
        (equal? $other $pattern))))

  (define (term-evaluate $scope $term)
    (switch $term
      ((variable? $variable)
        (switch (scope-ref $scope (variable-type $variable))
          ((variable? $variable)
            (evaluated $variable (variable-type $variable)))
          (($else $other)
            (evaluated $other $other))))
      ((function? $function)
        (lets
          ($params
            (map evaluated-term
              (map
                (partial term-evaluate $scope)
                (function-params $function))))
          ($evaluated-body
            (term-evaluate
              (push-list $scope (map variable $params))
              (function-body $function)))
          (evaluated
            (any-function $params (evaluated-type $evaluated-body))
            (thunk $scope (function $params (evaluated-term $evaluated-body))))))
      ((application? $application)
        (evaluated-apply
          (term-evaluate $scope (application-target $application))
          (map (partial term-evaluate $scope) (application-args $application))))
      ((struct? $struct)
        (evaluated-struct
          (struct-name $struct)
          (map (partial term-evaluate $scope) (struct-items $struct))))
      ((struct-get? $struct-get)
        (evaluated-struct-get
          (term-evaluate $scope (struct-get-struct $struct-get))
          (term-evaluate $scope (evaluated-type (struct-get-type $struct-get)))))
      ((else $other)
        (evaluated $other $other)))

  (define (evaluated-apply $evaluated-target $evaluated-args)
    (evaluated
      (type-apply
        (evaluated-type $evaluated-target)
        (map evaluated-type $evaluated-args))
      (term-apply
        (evaluated-term $evaluated-target)
        (map evaluated-term $evaluated-args))))

  (define (term-apply $target $args)
    (switch $target
      ((thunk? $thunk)
        (evaluated-term
          (term-evaluate
            (push-list (thunk-scope $thunk) $args)
            (thunk-body $thunk))))
      ((else $other)
        (application $target $args))))

  (define (type-apply $target-type $arg-types)
    (switch $target-type
      ((any-function? $any-function)
        (cond
          ((for-all term-matches? $arg-types (any-function-params $any-function))
            (any-function-result $any-function))
          (else (throw `type-apply $target-type $arg-types))))
      ((else $other)
        (throw `type-apply $target-type $arg-types))))

  (define (evaluated-struct $name $evaluated-items)
    (evaluated
      (struct $name (map evaluated-type $evaluated-items))
      (struct $name (map evaluated-term $evaluated-items))))

  (define (term-struct-get $term $item-type)
    (switch $type
      ((struct? $struct)
        (scope-ref (struct-items $struct) $item-type)
      ((else $other)
        (struct-get $other $item-type)))))

  (define (type-struct-get $type $item-type)
    (switch $type
      ((struct? $struct)
        (scope-ref (struct-items $struct) $item-type)
      ((else $other)
        (throw `type-struct-get $type $item-type)))))

  (define (evaluated-struct-get $evaluated $item-type)
    (evaluated
      (type-struct-get (evaluated-type $evaluated) $item-type)
      (term-struct-get (evaluated-term $evaluated) $item-type)))
)
