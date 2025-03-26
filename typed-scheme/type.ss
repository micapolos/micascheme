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

    lambda-type
    lambda-type?
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

    type?)
  (import (micascheme))

  (data (type-definition parent? gensym name arity))

  (data (defined-type parent? definition arguments))
  (data (lambda-type params result))
  (data (union-type items))
  (data (record-type parent? gensym items))
  (data (variable-type index))
  (data (forall-type arity type))
  (data (recursive-type type))

  (define (type? $obj)
    (or
      (defined-type? $obj)
      (lambda-type? $obj)
      (union-type? $obj)
      (recursive-type? $obj)
      (variable-type? $obj)
      (forall-type? $obj)
      (record-type? $obj)))
)
