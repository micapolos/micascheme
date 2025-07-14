(library (asm-2 fragment)
  (export
    fragment fragment? fragment-deps fragment-ref
    fragment-with
    fragment-ref
    pure-fragment
    list->fragment
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

  (define (fragment-ref $fragment $lookup)
    ((fragment-proc $fragment) $lookup))

  (define (pure-fragment $value)
    (fragment-with $value))

  (define (fragment-map $proc $fragment)
    (fragment
      (fragment-deps $fragment)
      (lambda ($lookup)
        ($proc (fragment-proc $fragment)))))

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
