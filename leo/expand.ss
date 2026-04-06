(library (leo expand)
  (export
    leo-expand-once
    leo-expand)
  (import
    (scheme)
    (syntax)
    (system)
    (only (leo transform) transform-leo from))

  (define (leo-expand $datum . $args)
    (apply sc-expand (leo-expand-once $datum) $args))

  (define (leo-expand-once $datum)
    (map syntax->datum/annotation
      (syntax->list
        (transform-leo
          (datum->syntax #'leo-expand $datum)))))
)
