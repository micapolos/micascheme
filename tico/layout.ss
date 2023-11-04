(library (tico layout)
  (export
    empty-layout empty-layout?
    simple-layout simple-layout?
    tuple-layout tuple-layout? tuple-layout-items
    lambda-layout lambda-layout? lambda-layout-params lambda-layout-body

    layout-not-empty?
    tuple-or-empty-layout
    type->layout)
  (import
    (micascheme)
    (tico type))

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

  (define (type->layout $type)
    (switch $type
      ((value-type? _)
        (empty-layout))
      ((native-type? _)
        (simple-layout))
      ((type-type? _)
        (simple-layout))
      ((struct? $struct)
        (tuple-or-empty-layout
          (map type->layout (struct-fields $struct))))
      ((arrow? $arrow)
        (switch (type->layout (arrow-result $arrow))
          ((empty-layout? _) (empty-layout))
          ((else $result-layout)
            (lambda-layout
              (map type->layout (arrow-params $arrow))
              $result-layout))))
      ((else $type)
        (throw type->layout $type))))
)
