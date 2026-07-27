(library (tt runtime)
  (export
    (rename (create-type-declaration type-declaration))
    type-declaration?
    type-declaration-id
    type-declaration-name
    type-declaration-arity

    (rename (create-declared-type declared-type))
    declared-type?
    declared-type-args)
  (import
    (scheme)
    (data)
    (procedure)
    (throw))

  (data (type-declaration id name arity))
  (data (declared-type declaration args))

  (define (create-type-declaration id name arity)
    (ensure symbol? id)
    (ensure string? name)
    (ensure integer? arity)
    (ensure nonnegative? arity)
    (type-declaration id name arity))

  (define (create-declared-type $type-declaration . $args)
    (ensure type-declaration? $type-declaration)
    (ensure list? $args)
    (ensure (partial = (type-declaration-arity $type-declaration)) (length $args))
    (declared-type $args))
)
