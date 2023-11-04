(library (tico layout)
  (export
    empty-layout empty-layout?
    simple-layout simple-layout?
    tuple-layout tuple-layout? tuple-layout-items
    lambda-layout lambda-layout? lambda-layout-params lambda-layout-body

    layout-not-empty?
    tuple-or-empty-layout)
  (import
    (micascheme))

  (data (empty-layout))
  (data (simple-layout))
  (data (tuple-layout items))
  (data (lambda-layout params body))

  (define (tuple-or-empty-layout $layouts)
    (if (for-all empty-layout? $layouts)
      (empty-layout)
      (tuple-layout $layouts)))

  (define (layout-not-empty? $layout)
    (not (empty-layout? $layout)))
)
