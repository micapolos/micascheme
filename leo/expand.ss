(library (leo expand)
  (export
    leo-expand-once
    leo-expand)
  (import
    (micascheme)
    (only (leo transform) transform-leo))

  (define (leo-expand $datum . $args)
    (apply sc-expand
      (syntax->datum/annotation
        (transform-leo
          (datum->syntax #'leo-expand $datum)))
      $args))

  (define (leo-expand-once $datum)
    (syntax->datum/annotation
      (transform-leo (datum->syntax #'leo-expand $datum))))
)
