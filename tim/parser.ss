(library (tim parser)
  (export
    type index array
    typed? typed typed-type typed-value
    typed-expr)
  (import
    (micascheme)
    (tim type))

  (define-aux-keywords type index array)

  (data (typed type value))

  (define (typed-expr $lookup $syntax)
    (syntax-case $syntax (type string index array)
      (type
        (typed
          (universe 1)
          (type->syntax (universe 0))))
      (string
        (typed
          (universe 0)
          (type->syntax (string-type))))
      ((index $size)
        (positive-integer? (datum $size))
        (typed
          (universe 0)
          (type->syntax (index-type (datum $size)))))
      ((array $element $size)
        (positive-integer? (datum $size))
        (typed
          (universe 0)
          #`(array-type
            #,(syntax->type-syntax $lookup #'$element)
            $size)))
      ($string
        (string? (datum $string))
        (typed (string-type) (syntax $string)))
      ((index $size $value)
        (lets
          (run
            (unless
              (and (integer? (datum $size)) (positive? (datum $size)))
              (syntax-error #'$size "invalid size")))
          (run
            (unless
              (and (integer? (datum $value)) (>= (datum $value) 0) (< (datum $value) (datum $size)))
              (syntax-error #'$value "out of range")))
          (typed
            (index-type (datum $size))
            #'$value)))
      ($id (identifier? #'$id)
        (switch ($lookup #'$id)
          ((false? _) (syntax-error #'$id "unknown type"))
          ((else $type) (typed $type #'$id))))
      (_ (syntax-error $syntax))))

  (define (syntax->type-syntax $lookup $syntax)
    (lets
      ($typed (typed-expr $lookup $syntax))
      (run
        (unless
          (equal? (typed-type $typed) (universe 0))
          (syntax-error $syntax "not a type")))
      (typed-value $typed)))
)
