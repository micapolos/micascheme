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

    list->fragment
    fragment-append
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

  (define (list->fragment $fragments)
    (switch-exhaustive $fragments
      ((null? _)
        (fragment (list) (list)))
      ((pair? (pair $fragment $fragments))
        (fragment-bind
          (lambda ($obj)
            (fragment-map
              (lambda ($objs) (cons $obj $objs))
              (list->fragment $fragments)))
          $fragment))))

  (define (fragment-append . $fragments)
    (list->fragment $fragments))

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

  (define (fragment-map $proc $fragment)
    (fragment-bind
      (lambda ($obj) (obj->fragment ($proc $obj)))
      $fragment))

  (define (fragment-eval $fragment)
    (eval
      (fragment-obj $fragment)
      (apply environment (fragment-imports $fragment))))
)
