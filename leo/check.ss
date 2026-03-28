(library (leo check)
  (export check)
  (import
    (rename (micascheme)
      (check %check)))

  (define-rules-syntaxes
    ((check . x)
      (with-exception-handler
        (base-exception-handler)
        (lambda () (%check . x)))))
)
