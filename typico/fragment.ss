(library (typico fragment)
  (export
    (rename
      (fragment make-fragment)
      (fragment-with fragment))
    fragment?
    fragment-imports
    fragment-obj

    pure-fragment
    id->fragment

    list->fragment
    list*->fragment
    fragment-append
    fragment-bind
    fragment-map
    fragment-eval
    fragment-bind-with

    symbol->fragment)
  (import
    (typico base)
    (typico id))

  (data (fragment imports obj))

  (define (pure-fragment $obj)
    (fragment (list) $obj))

  (define (id->fragment $id)
    (pure-fragment (id->symbol $id)))

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
        (fragment-bind-with
          ($obj $fragment)
          ($objs (list->fragment $fragments))
          (pure-fragment (cons $obj $objs))))))

  (define (list*->fragment $fragments)
    (switch-exhaustive $fragments
      ((null? $null)
        (fragment (list) (list)))
      ((pair? (pair $fragment $fragments))
        (fragment-bind-with
          ($obj $fragment)
          ($objs (list*->fragment $fragments))
          (pure-fragment (cons $obj $objs))))
      ((fragment? $fragment)
        $fragment)))

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
      (lambda ($obj) (pure-fragment ($proc $obj)))
      $fragment))

  (define (fragment-eval $fragment)
    (eval
      (fragment-obj $fragment)
      (apply environment (fragment-imports $fragment))))

  (define-rules-syntax
    ((fragment-bind-with body) body)
    ((fragment-bind-with (obj frag) rest ...)
      (fragment-bind
        (lambda (obj) (fragment-bind-with rest ...))
        frag)))

  (define (symbol->fragment $symbol)
    (fragment-bind-with
      ($quote (fragment-with (import (scheme)) quote))
      (pure-fragment `(,$quote ,$symbol))))
)
