(library (typico fragment)
  (export
    (rename
      (fragment make-fragment)
      (fragment-with fragment))
    fragment?
    fragment-imports
    fragment-obj

    obj->fragment
    id->fragment

    fragment-bind
    fragment-eval)
  (import
    (typico base)
    (typico id))

  (data (fragment imports obj))

  (define (obj->fragment $obj)
    (fragment (list) $obj))

  (define (id->fragment $id)
    (obj->fragment (id->symbol $id)))

  (define-rules-syntax (literals import)
    ((fragment-with obj)
      (fragment-with (import) obj))
    ((fragment-with (import i ...) obj)
      (fragment (list 'i ...) 'obj)))

  (define (fragment-bind $proc $fragment)
    (lets
      ((fragment $imports $obj) $fragment)
      ((fragment $proc-imports $proc-obj) ($proc (fragment-obj $fragment)))
      (fragment
        (append $imports
          (filter
            (lambda ($import) (not (member $import $imports)))
            $proc-imports))
        $proc-obj)))

  (define (fragment-eval $fragment)
    (eval
      (fragment-obj $fragment)
      (apply environment (fragment-imports $fragment))))
)
