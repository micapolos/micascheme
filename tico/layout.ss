(library (tico layout)
  (export
    empty-layout empty-layout?
    simple-layout simple-layout?
    tuple-layout tuple-layout? tuple-layout-items
    lambda-layout lambda-layout? lambda-layout-params lambda-layout-body

    layout-not-empty?
    tuple-or-empty-layout
    type->layout

    layout-application
    layout-abstraction
    layout-struct)
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

  (define (layout-abstraction $param-layouts $body-layout)
    (cond
      ((layout-not-empty? $body-layout)
        (lambda-layout $param-layouts $body-layout))
      (else
        (empty-layout))))

  (define (layout-application $target $args)
    (switch $target
      ((lambda-layout? $lambda-layout)
        (lambda-layout-body $lambda-layout))
      ((empty-layout? _)
        (empty-layout))))

  (define (layout-struct $name $field-layouts)
    (tuple-or-empty-layout $field-layouts))

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
        (layout-abstraction
          (map type->layout (arrow-params $arrow))
          (type->layout (arrow-result $arrow))))
      ((else $type)
        (throw type->layout $type))))
)
