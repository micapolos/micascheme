(library (typico fragment)
  (export
    fragment
    fragment?
    fragment-imports
    fragment-obj

    fragment-with
    fragment-bind)
  (import
    (typico base))

  (data (fragment imports obj))

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
)
