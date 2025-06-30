(library (typico fragment)
  (export
    fragment
    fragment?
    fragment-obj
    fragment-imports

    fragment-bind)
  (import
    (typico base))

  (data (fragment obj imports))

  (define (fragment-bind $proc $fragment)
    (lets
      ((fragment $obj $imports) $fragment)
      ((fragment $proc-obj $proc-imports) ($proc (fragment-obj $fragment)))
      (fragment $proc-obj
        (append $imports
          (filter
            (lambda ($import) (not (member $import $imports)))
            $proc-imports)))))
)
