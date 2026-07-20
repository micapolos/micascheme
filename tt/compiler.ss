(library (tt compiler)
  (export
    type-declaration
    type-declaration?
    type-declaration-id
    type-declaration-name
    type-declaration-arity

    declared-type
    declared-type?
    declared-type-declaration
    declared-type-arguments

    lambda-type
    lambda-type?
    lambda-type-params
    lambda-type-result

    typed
    typed?
    typed-type
    typed-ref

    syntax->type-declaration
    syntax->type)
  (import
    (scheme)
    (data)
    (switch)
    (procedure)
    (lets))

  (data (lambda-type params result))
  (data (type-declaration id name arity))
  (data (declared-type declaration arguments))
  (data (typed type ref))

  (define (type? $obj)
    (or
      (lambda-type? $obj)
      (declared-type? $obj)))

  (define (syntax->identifier $syntax)
    (switch $syntax
      ((identifier? $identifier) $identifier)
      ((else $other) (syntax-error $other "not identifier"))))

  (define (type-declaration-apply $syntax $type-declaration $arguments)
    (cond
      ((= (length $arguments) (type-declaration-arity $type-declaration))
        (declared-type $type-declaration $arguments))
      (else
        (syntax-error $syntax "invalid arity"))))

  (define (syntax->type-declaration $lookup $syntax)
    (switch ($lookup (syntax->identifier $syntax))
      ((type-declaration? $type-declaration)
        $type-declaration)
      ((else $other)
        (syntax-error $other "not type declaration"))))

  (define (syntax->type $lookup $syntax)
    (syntax-case $syntax ()
      (id
        (identifier? #'id)
        (switch ($lookup #'id)
          ((type-declaration? $type-declaration)
            (type-declaration-apply
              $syntax
              $type-declaration
              '()))
          ((else $other) (syntax-error $other "not type"))))
      ((id arg args ...)
        (identifier? #'fn)
        (switch ($lookup #'id)
          ((type-declaration? $type-declaration)
            (type-declaration-apply
              $syntax
              $type-declaration
              (map (partial syntax->type $lookup) #'(arg args ...))))
          ((else $other)
            (syntax-error $other "not type declaration"))))))

  (define (syntax->typed-param $lookup $syntax)
    (syntax-case $syntax ()
      ((id type)
        (typed
          (syntax->type $lookup #'type)
          (syntax->identifier #'id)))))
)
