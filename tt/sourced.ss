(library (tt sourced)
  (export
    sourced
    sourced?
    sourced-source
    sourced-ref

    sourced=?
    sourced-map
    sourced-strip)
  (import
    (scheme)
    (data)
    (switch)
    (lets)
    (procedure)
    (source-object))

  (data (sourced source ref))

  (define (sourced=? $ref=? $a $b)
    (switch $a
      ((sourced? $a)
        (and
          (sourced? $b)
          (source-object=?
            (sourced-source $a)
            (sourced-source $b))
          (sourced=? $ref=?
            (sourced-ref $a)
            (sourced-ref $b))))
      ((else $a)
        ($ref=? $a $b))))

  (define (sourced-map $fn $term)
    (switch $term
      ((sourced? $sourced)
        (sourced-map
          (lambda ($resource $term)
            ($fn
              (lambda ($term)
                (sourced
                  (sourced-source $sourced)
                  ($resource $term)))
              $term))
          (sourced-ref $sourced)))
      ((else $other)
        ($fn identity $other))))

  (define (sourced-strip $term)
    (sourced-map
      (lambda ($rewrap $term) $term)
      $term))
)
