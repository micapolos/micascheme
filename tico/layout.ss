(library (tico layout)
  (export
    empty-layout empty-layout?
    simple-layout simple-layout?
    native-layout native-layout?
    struct-layout struct-layout? struct-layout-fields struct-layout-size
    layout-field layout-field? layout-field-layout layout-field-index-opt
    lambda-layout lambda-layout? lambda-layout-params lambda-layout-body

    empty-struct-layout
    struct-layout+layout
    make-struct-layout

    layout-empty?
    layout-not-empty?
    type->layout

    literal->layout
    layout-application
    layout-abstraction
    layout-struct
    layout-ref)
  (import
    (micascheme)
    (tico type))

  (data (empty-layout))
  (data (simple-layout)) ; TODO: Remove it?
  (data (native-layout))
  (data (struct-layout fields size))
  (data (layout-field layout index-opt))
  (data (lambda-layout params body))

  (define (literal->layout $literal)
    (type->layout (literal->type $literal)))

  (define (layout-empty? $layout)
    (switch $layout
      ((empty-layout? _) #t)
      ((native-layout? _) #f)
      ((simple-layout? _) #f)
      ((struct-layout? $struct-layout)
        (zero? (struct-layout-size $struct-layout)))
      ((lambda-layout? $lambda-layout)
        (layout-empty? (lambda-layout-body $lambda-layout)))
      ((else $other) #f))) ; (throw not-layout $other))))

  (define empty-struct-layout
    (struct-layout (list) 0))

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
    (lambda-layout
      (make-struct-layout $param-layouts)
      $body-layout))

  (define (layout-application $target $args)
    (switch $target
      ((lambda-layout? $lambda-layout)
        (lambda-layout-body $lambda-layout))
      ((else $other)
        (throw layout-application $target))))

  (define (layout-struct $name $field-layouts)
    (make-struct-layout (reverse $field-layouts)))

  (define (layout-ref $layout $index)
    (list-ref
      (struct-layout-fields $layout)
      (- (length (struct-layout-fields $layout)) $index 1)))

  (define (type->layout $type)
    (switch $type
      ((value-type? _)
        (empty-layout))
      ((native-type? _)
        (native-layout))
      ((type-type? _)
        (simple-layout))
      ((struct? $struct)
        (layout-struct (struct-name $struct)
          (map type->layout (struct-fields $struct))))
      ((arrow? $arrow)
        (layout-abstraction
          (map type->layout (arrow-params $arrow))
          (type->layout (arrow-result $arrow))))
      ((property? $property)
        (layout-abstraction
          (list (type->layout (property-owner $property)))
          (type->layout (property-body $property))))
      ((else $type)
        (throw type->layout $type))))
)
