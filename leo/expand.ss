(library (leo expand)
  (export leo-expand)
  (import
    (micascheme)
    (leo transform))

  (define (leo-expand $datum)
    (syntax->datum/annotation
      (transform-leo (datum->syntax #'leo-expand $datum))))
)
