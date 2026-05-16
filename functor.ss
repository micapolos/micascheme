(library (functor)
  (export
    make-functor
    functor
    functor?
    functor-map-proc

    functor-map
    functor-replace

    option-functor
    list-functor
    vector-functor)
  (import
    (scheme)
    (data)
    (procedure))

  (data (functor map-proc))

  (define (functor-map $functor $obj $fn)
    ((functor-map-proc $functor) $obj $fn))

  (define (functor-replace $functor $obj $value)
    (functor-map $functor $obj (always $value)))

  (define option-functor
    (functor
      (lambda ($option $fn)
        (and $option ($fn $option)))))

  (define list-functor
    (functor
      (lambda ($list $fn)
        (map $fn $list))))

  (define vector-functor
    (functor
      (lambda ($vector $fn)
        (vector-map $fn $vector))))
)
