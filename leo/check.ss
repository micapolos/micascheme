(library (leo check)
  (export checking? check)
  (import
    (rename (micascheme)
      (check %check)
      (checking? %checking?)))

  (define checking? (make-thread-parameter #f))

  (define-rules-syntaxes
    ((check . x)
      (meta-cond
        ((checking?) (%check . x)))))
)
