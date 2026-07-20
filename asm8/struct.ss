(library (struct)
  (export)
  (import (scheme) (lets))

  (define-record-type type-parameter
    (fields name bound-type))

  (define-record-type type-declaration
    (parent type)
    (fields id name parent? type-parameters fields))

  (define-record-type type)

  (define-record-type wildcard-type
    (parent type)
    (fields id name upper-bound lower-bound?))

  (define-record-type declared-type
    (parent type)
    (fields declaration argument-types))

  (define-record-type function-type
    (parent type)
    (fields parameter-types result-type))

  (define-record-type parameters
    (fields types variadic?))

  (define any-type-declaration
    (make-type-declaration
      (gensym)
      'any
      #f
      (list)
      (list)))

  (define fixnum-type-declaration
    (make-type-declaration
      (gensym)
      'fixnum
      any-type-declaration
      (list)
      (list)))

  (define string-type-declaration
    (make-type-declaration
      (gensym)
      'fixnum
      any-type-declaration
      (list)
      (list)))

  (define pair-type-declaration
    (make-type-declaration
      (gensym)
      'pair
      any-type-declaration
      (list
        (make-type-parameter 'car any-type-declaration)
        (make-type-parameter 'cdr any-type-declaration))
      (list)))

  (define list-type-declaration
    (make-type-declaration
      (gensym)
      'list
      any-type-declaration
      (list (make-type-parameter 'item any-type-declaration))
      (list)))

  (data (binding mutable? ref))
  (data (compiled type ref))

  (data (type-parameter id name))
  (data (field-declaration name mutable? type))
  (data (type-declaration id name parent? type-parameters field-declarations))

  (data (declared-type type-arguments))

  (data any-type)
  (data boolean-type)
  (data char-type)
  (data string-type)
  (data number-type)
  (data fixnum-type)

  (define (type>= $lhs $rhs)
    (switch $lhs
      ((any-type? _) #t)
      ((number-type? $lhs)
        (or
          (number-type? $rhs)
          (fixnum-type? $rhs)))
      ((record-type-descriptor? $lhs-rtd)
        (switch? $rhs
          ((record-type-descriptor? $rhs-rtd)
            (record-type>= $lhs-rtd $rhs-rtd))))
      ((boolean-type? #f) (boolean-type? $rhs))
      ((char-type? #f) (char-type? $rhs))
      ((string-type? #f) (string-type? $rhs))
      ((fixnum-type? #f) (fixnum-type? $rhs))
      ((else _) #f)))

  (define (record-type>= $lhs $rhs)
    (or
      (eq? $lhs $rhs)
      (lets?
        ($rhs-parent (record-type-parent $rhs))
        (record-type>= $lhs $rhs-parent))))

  (define empty-scope (lambda ($bindings $read? $type) '()))

  (define (scope-push $scope $mutable? $type)
    (lambda ($bindings $read? $lookup-type)
      (lets
        ($bindings
          (if (type=? $type $looked-type)
            (cons (binding $mutable? $type) $bindings)
            $bindings))
        ($scope $bindings $read? $lookup-type))))

  (define (compile-mutable? $syntax)
    (syntax-case $syntax (val var)
      (val #f)
      (var #t)
      (else (syntax-error $syntax "invalid mutability"))))

  (define (compile-binding $scope $syntax)
    (syntax-case $syntax ()
      ((mutability expr)
        (binding
          (compile-mutable #'mutability)
          (car (generate-temporaries #'(expr)))
          (compile-expression $scope #'expr)))))

  (define (scope-binding $scope $type)
    )

  (define (compile-expression-of $scope $type $syntax)
    (lets
      ($compiled (compile-expression $scope $syntax))
      (cond
        ((type>= $type (compiled-type $compiled))
          (compiled-ref $compiled))
        (else
          (syntax-error $syntax "invalid type")))))

  (define (compile-arguments $scope $types $syntaxes)
    (map (partial compile-expression-of $scope) $types $syntaxes))

  (define (compile-function $scope $syntax)
    (lets
      ($compiled (compile-expression $scope $syntax))
      (switch (compiled-type $compiled)
        ((function-type? $function-type) $compiled)
        ((else _) (syntax-error $syntax "not function")))))

  (define (compile-identifier $syntax)
    (cond
      ((identifier? $syntax) $syntax)
      (else (syntax-error $syntax "not identifier"))))

  (define (compile-param $scope $syntax)
    (syntax-case $syntax ()
      ((id type)
        (compiled
          (compile-type $scope #'type))
          (compile-identifier #'id))))

  (define (compile-params $scope $syntax)
    (syntax-case $syntax ()
      (() '())
      ((param . params)
        (cons
          (compile-param $scope #'param))
          (compile-params $scope #'params))
      (param
        (compile-param $scope #'param))))

  (define (compile-lambda $scope $syntax)
    (syntax-case $syntax (lambda)
      ((lambda params . body)
        (lets
          ($compiled-params (compile-params $scope #'params))
          ($compiled-body (scope-push $scope $compiled-params) #'body)
          (compiled
            (compiled-type $compiled-body)
            #`(lambda
              #,(map* compiled-type $compiled-params)
              #,@(compiled-ref $compiled-body)))))))

  (define (compile-body $scope $syntax)
    (syntax-case $syntax (lambda)
      ((lambda params xs ... x)

  (define (compile-expression $scope $syntax)
    (syntax-case $syntax (let in begin set! the)
      ((lambda . x)
        (compile-lambda $scope $syntax))
      ((set! expr)
        (lets
          ((compiled $type $expr)
            (compile-expression $scope #'expr))
          ($identifier
            (scope-mutable-identifier $scope $type))
          (compiled
            #f
            #`(set! #,$identifier #,$expr))))
      (id
        (identifier? #'id)
        ($scope $id))
      ((fn arg ...)
        (lets
          ((compiled (function-type $param-types $result-type) $fn-syntax)
            (compile-function $scope #'fn))
          (compiled
            $result-type
            #`(
              #,$fn-syntax
              #,@(compile-arguments $scope $param-types #'(arg ...))

  (define (struct-field? $struct $name)
    (fields-field? (struct-fields $struct) $name))

  (define (fields-field? $fields $name $start-index)
    (cond
      ((= $start-index (vector-length $fields)) #f)
      (or
        (field-field? (vector-ref $fields $start-index) $name)
        (fields-field? $fields $name (fx+ $start-index 1)))))

  (define (field-field? $field $name)
    (and
      (symbol=? (field-name $field) $name)
      $field))

  (define (struct-field $syntax $lookup)
    (syntax-case $syntax ()
      ((struct-id field-name)
        (switch ($lookup #'struct-id)
          ((struct? $struct)
            (switch (struct-field? $struct)
              ((false? _) (syntax-error #'field-name "no field"))
              ((else $field) $field)))
          ((else _)
            (syntax-error #'struct-id "not struct"))))))

  (define (struct-accessor $syntax $lookup)
    (syntax-case $syntax ()
      ((struct-id field-name obj)
        (lets
          ($field (struct-field #'(struct-id field-name)))
          (field-accessor-id $field)))))

  (define (struct-mutator $syntax $lookup)
    (syntax-case $syntax ()
      ((struct-id field-name obj)
        (lets
          ($field (struct-field #'(struct-id field-name)))
          (switch (field-accessor-id? $field)
            ((false? _) (syntax-error #'field-name "no mutator"))
            ((else $mutator-id) $mutator-id))))))
)
