(library (asm-2 fragment)
  (export
    fragment fragment? fragment-deps fragment-ref
    wrap-fragment
    fragment-with
    fragment-ref
    fragment-map
    pure-fragment
    list->fragment
    wrap-fragment
    fragment-append)
  (import (micascheme) (syntax lookup))

  (data (fragment deps proc))

  (define-rules-syntax
    ((fragment-with value)
      (fragment-with (dep) value))
    ((fragment-with (dep x ...) body)
      (fragment
        (list #'x ...)
        (lambda ($lookup)
          (let-syntax
            ((dep
              (syntax-rules ()
                ((_ id) (lookup-ref $lookup #'id)))))
            body)))))

  (define (wrap-fragment $obj)
    (switch $obj
      ((fragment? $fragment) $fragment)
      ((else $value) (fragment-with $value))))

  (define (fragment-ref $fragment $lookup)
    ((fragment-proc $fragment) $lookup))

  (define (pure-fragment $value)
    (fragment-with $value))

  (define (fragment-map $proc $fragment)
    (fragment
      (fragment-deps $fragment)
      (lambda ($lookup)
        ($proc (fragment-ref $fragment $lookup)))))

  (define (list->fragment $fragments)
    (fragment
      (flatten (map fragment-deps $fragments))
      (lambda ($lookup)
        (map
          (lambda ($fragment) ((fragment-proc $fragment) $lookup))
          $fragments))))

  (define (fragment-append . $fragments)
    (list->fragment $fragments))
)
