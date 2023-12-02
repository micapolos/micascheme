(library (tico layout)
  (export
    empty-layout empty-layout?
    simple-layout simple-layout?
    native-layout native-layout?
    struct-layout struct-layout? struct-layout-fields struct-layout-size
    layout-field layout-field? layout-field-layout layout-field-index-opt
    lambda-layout lambda-layout? lambda-layout-params lambda-layout-body

    empty-struct-layout
    struct-layout-reverse
    struct-layout+layout
    make-struct-layout

    layout-empty?
    layout-not-empty?
    layout-arity
    type->layout

    literal->layout
    layout-application
    layout-abstraction
    layout-args
    layout-struct
    layout-ref

    empty-stack-layout
    stack-layout-push
    stack-layout-ref

    list-layout)
  (import
    (micascheme)
    (tico arity)
    (tico type))

  (data (empty-layout))
  (data (simple-layout)) ; TODO: Remove it?
  (data (native-layout))
  (data (struct-layout fields size))
  (data (layout-field layout index-opt))
  (data (lambda-layout params body))

  (function (literal->layout $literal)
    (type->layout (literal->type $literal)))

  (function (layout-empty? $layout)
    (switch $layout
      ((empty-layout? _) #t)
      ((native-layout? _) #f)
      ((simple-layout? _) #f)
      ((struct-layout? $struct-layout)
        (zero? (struct-layout-size $struct-layout)))
      ((lambda-layout? $lambda-layout)
        (layout-empty? (lambda-layout-body $lambda-layout)))
      ((else $other) #f))) ; (throw not-layout $other))))

  (function (layout-arity $layout)
    (switch-exclusive $layout
      ((empty-layout? _) (arity 0))
      ((native-layout? _) (arity 1))
      ((simple-layout? _) (arity 1))
      ((struct-layout? $struct-layout)
        (arity (struct-layout-size $struct-layout)))
      ((lambda-layout? $lambda-layout)
        (if (layout-empty? (lambda-layout-body $lambda-layout))
          (arity 0)
          (arity 1)))))

  (define empty-struct-layout
    (struct-layout (list) 0))

  (function (struct-layout-reverse $struct-layout)
    (struct-layout
      (reverse (struct-layout-fields $struct-layout))
      (struct-layout-size $struct-layout)))

  (function (struct-layout+layout $struct-layout $layout)
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

  (function (make-struct-layout $layouts)
    (fold-left
      struct-layout+layout
      empty-struct-layout
      (reverse $layouts)))

  (function (layout-not-empty? $layout)
    (not (layout-empty? $layout)))

  (function (layout-abstraction $param-layouts $body-layouts)
    (lambda-layout
      (make-struct-layout $param-layouts)
      (make-struct-layout $body-layouts)))

  (function (layout-application $target $args)
    (switch $target
      ((lambda-layout? $lambda-layout)
        (lambda-layout-body $lambda-layout))
      ((else $other)
        (throw layout-application $target))))

  (function (layout-args $layouts)
    (fold-left
      struct-layout+layout
      empty-struct-layout
      $layouts))

  (function (list-layout $layouts)
    (struct-layout-reverse
      (fold-left
        struct-layout+layout
        empty-struct-layout
        $layouts)))

  (function (layout-struct $name $field-layouts)
    (make-struct-layout (reverse $field-layouts)))

  (function (layout-ref $layout $index)
    (list-ref
      (struct-layout-fields $layout)
      (- (length (struct-layout-fields $layout)) $index 1)))

  (function (type->layout $type)
    (switch $type
      ((value-type? _)
        (empty-layout))
      ((unchecked-type? _)
        (native-layout))
      ((type-type? _)
        (simple-layout))
      ((args-type? $args-type)
        (layout-args
          (map type->layout
            (args-type-items $args-type))))
      ((struct? $struct)
        (layout-struct (struct-name $struct)
          (map type->layout (struct-fields $struct))))
      ((arrow? $arrow)
        (layout-abstraction
          (map type->layout (arrow-params $arrow))
          (map type->layout (arrow-results $arrow))))
      ((property? $property)
        (layout-abstraction
          (list (type->layout (property-param $property)))
          (list (type->layout (property-body $property)))))
      ((constant-type? $constant-type)
        (type->layout (constant-type-value $constant-type)))
      ((list-of? $list-of)
        (simple-layout))
      ((else $type)
        (throw type->layout $type))))

  (function (empty-stack-layout)
    empty-struct-layout)

  (function (stack-layout-push $stack-layout $layout)
    (struct-layout+layout $stack-layout $layout))

  (function (stack-layout-ref $stack-layout $index)
    (list-ref (struct-layout-fields $stack-layout) $index))
)
