(library (tico layout)
  (export
    empty-layout empty-layout?
    simple-layout simple-layout?
    tuple-layout tuple-layout? tuple-layout-items
    struct-layout struct-layout? struct-layout-fields struct-layout-size
    layout-field layout-field? layout-field-layout layout-field-index-opt
    lambda-layout lambda-layout? lambda-layout-params lambda-layout-body

    empty-struct-layout
    struct-layout+layout
    make-struct-layout

    layout-empty?
    layout-not-empty?
    tuple-or-empty-layout
    type->layout

    literal->layout
    layout-application
    layout-abstraction
    layout-struct)
  (import
    (micascheme)
    (tico type))

  (data (empty-layout))
  (data (simple-layout))
  (data (tuple-layout items))
  (data (struct-layout fields size))
  (data (layout-field layout index-opt))
  (data (lambda-layout params body))

  (define (literal->layout $literal)
    (simple-layout))

  (define (tuple-or-empty-layout $layouts)
    (if (for-all empty-layout? $layouts)
      (empty-layout)
      (tuple-layout $layouts)))

  (define (layout-empty? $layout)
    (switch $layout
      ((empty-layout? _) #t)
      ((simple-layout? _) #f)
      ((tuple-layout? _) #f)
      ((struct-layout? $struct-layout)
        (zero? (struct-layout-size $struct-layout)))
      ((lambda-layout? $lambda-layout)
        (layout-empty? (lambda-layout-body $lambda-layout)))
      ((else $other) #f))) ; (throw not-layout $other))))

  (define empty-struct-layout
    (struct-layout (stack) 0))

  (define (struct-layout+layout $struct-layout $layout)
    (lets
      ($fields (struct-layout-fields $struct-layout))
      ($size (struct-layout-size $struct-layout))
      (cond
        ((layout-empty? $layout)
          (struct-layout
            (push $fields (layout-field $layout #f))
            $size))
        (else
          (struct-layout
            (push $fields (layout-field $layout $size))
            (+ $size 1))))))

  (define (make-struct-layout $layouts)
    (fold-left
      struct-layout+layout
      empty-struct-layout
      (reverse $layouts)))

  (define (layout-not-empty? $layout)
    (not (layout-empty? $layout)))

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
    (make-struct-layout (reverse $field-layouts)))

  (define (type->layout $type)
    (switch $type
      ((value-type? _)
        (empty-layout))
      ((native-type? _)
        (simple-layout))
      ((type-type? _)
        (simple-layout))
      ((struct? $struct)
        (layout-struct (struct-name $struct)
          (map type->layout (struct-fields $struct))))
      ((arrow? $arrow)
        (layout-abstraction
          (map type->layout (arrow-params $arrow))
          (type->layout (arrow-result $arrow))))
      ((else $type)
        (throw type->layout $type))))
)
