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

    forall-type
    forall-type?
    forall-type-arity
    forall-type-procedure

    typed
    typed?
    typed-type
    typed-ref

    type=?
    type->datum
    typed-syntax->datum

    syntax->type-declaration
    syntax->type
    syntax->typed-param
    syntax->typed)
  (import
    (scheme)
    (data)
    (switch)
    (list)
    (procedure)
    (lets))

  (data (variable-type id))
  (data (forall-type arity procedure))
  (data (lambda-type params result))
  (data (type-declaration id name arity))
  (data (declared-type declaration arguments))
  (data (typed type ref))

  (define (type? $obj)
    (or
      (lambda-type? $obj)
      (declared-type? $obj)
      (forall-type? $obj)))

  (define (type-declaration=? $first $second)
    (symbol=?
      (type-declaration-id $first)
      (type-declaration-id $second)))

  (define (type=? $first $second)
    (switch-exhaustive $first
      ((variable-type? $first)
        (switch? $second
          ((variable-type? $second)
            (symbol=?
              (variable-type-id $first)
              (variable-type-id $second)))))
      ((forall-type? $first)
        (switch? $second
          ((forall-type? $second)
            (and
              (=
                (forall-type-arity $first)
                (forall-type-arity $second))
              (lets
                ($args
                  (map
                    (lambda (_) (variable-type (gensym)))
                    (iota (forall-type-arity $first))))
                (type=?
                  (apply (forall-type-procedure $first) $args)
                  (apply (forall-type-procedure $second) $args)))))))
      ((declared-type? $first)
        (and
          (type-declaration=?
            (declared-type-declaration $first)
            (declared-type-declaration $second))
          (for-all*
            (declared-type-arguments $first)
            (declared-type-arguments $second))))
      ((lambda-type? $first)
        (and
          (for-all* type=?
            (lambda-type-params $first)
            (lambda-type-params $second))
          (type=?
            (lambda-type-result $first)
            (lambda-type-result $second))))))

  (define (type->datum $type)
    (switch-exhaustive $type
      ((lambda-type? $lambda-type)
        `(lambda
          ,(map type->datum (lambda-type-params $lambda-type))
          ,(type->datum (lambda-type-result $lambda-type))))
      ((declared-type? $declared-type)
        (switch (declared-type-arguments $declared-type)
          ((null? _) (type-declaration-name (declared-type-declaration $declared-type)))
          ((else $args)
            `(
              ,(type-declaration-name (declared-type-declaration $declared-type))
              ,@(map type->datum $args)))))))

  (define (typed-syntax->datum $typed)
    `(typed
      ,(type->datum (typed-type $typed))
      ,(syntax->datum (typed-ref $typed))))

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
    (syntax-case $syntax (lambda)
      ((lambda (param ...) result)
        (lambda-type
          (map (partial syntax->type $lookup) #'(param ...))
          (syntax->type $lookup #'result)))
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

  (define (syntax->typed $lookup $syntax)
    (syntax-case $syntax (lambda)
      (n
        (number? (datum n))
        (typed
          (declared-type (type-declaration 'number 'number 0) '())
          #'n))
      (s
        (string? (datum s))
        (typed
          (declared-type (type-declaration 'string 'string 0) '())
          #'s))
      (id
        (identifier? #'id)
        (switch ($lookup #'id)
          ((typed? $typed) $typed)
          ((else $other) (syntax-error #'id "not typed"))))
      ((lambda (param ...) xs ... x)
        (lets
          ($typed-params (map (partial syntax->typed-param $lookup) #'(param ...)))
          ($lookup
            (lambda ($identifier)
              (or
                (find
                  (lambda ($typed-param)
                    (free-identifier=? (typed-ref $typed-param) $identifier))
                  $typed-params)
                ($lookup $identifier))))
          ($typed-xs (map (partial syntax->typed $lookup) #'(xs ...)))
          ($typed-x (syntax->typed $lookup #'x))
          (typed
            (lambda-type
              (map typed-type $typed-params)
              (typed-type $typed-x))
            #`(lambda
              #,(map typed-ref $typed-params)
              #,@(map typed-ref $typed-xs)
              #,(typed-ref $typed-x)))))
      ((fn arg ...)
        (lets
          ($typed-fn (syntax->typed-function $lookup #'fn))
          ($lambda-type (typed-type $typed-fn))
          ($args (syntax->arguments $lookup (lambda-type-params $lambda-type) #'(arg ...)))
          (typed
            (lambda-type-result $lambda-type)
            #`(#,(typed-ref $typed-fn) #,@$args))))))

  (define (syntax->typed-function $lookup $syntax)
    (lets
      ($typed (syntax->typed $lookup $syntax))
      (switch (typed-type $typed)
        ((lambda-type? _) $typed)
        ((else $other) (syntax-error $other "not function")))))

  (define (syntax->value $lookup $type $syntax)
    (lets
      ($typed (syntax->typed $lookup $syntax))
      (cond
        ((type=? (typed-type $typed) $type)
          (typed-ref $typed))
        (else
          (syntax-error $syntax "invalid type")))))

  (define (syntax->arguments $lookup $param-types $syntax)
    (syntax-case $syntax ()
      ((x ...)
        (map
          (partial syntax->value $lookup)
          $param-types
          #'(x ...)))))
)
