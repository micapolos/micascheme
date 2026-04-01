(library (leo expand)
  (export
    leo-expand-once
    leo-expand)
  (import
    (scheme)
    (syntax)
    (only (leo transform) transform-leo from))

  (define (leo-expand $datum . $args)
    (apply sc-expand (leo-expand-once $datum) $args))

  (define (leo-expand-once $datum)
    (syntax->datum/annotation
      (transform-leo
        (datum->syntax #'leo-expand $datum))))
)
