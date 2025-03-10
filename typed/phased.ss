(library (typed phased)
  (export
    phased phased? phased-phase phased-value
    type-lookup)
  (import
    (micascheme)
    (syntax lookup)
    (typed type)
    (any)
    (typed typed))

  (data (phased phase value))

  (define (type-lookup $lookup)
    (lambda ($identifier)
      (switch ($lookup $identifier)
        ((false? $false) $false)
        ((else $phased)
          (lets
            ($phase (phased-phase $phased))
            (case $phase
              ((0) #f)
              (else
                (lets
                  ($typed (phased-value $phased))
                  ($type (typed-type $typed))
                  ($value (typed-value $typed))
                  ($phase (- $phase 1))
                  (phased $phase
                    (typed $type
                      (case $phase
                        ((0)
                          (switch $type
                            ((any-type? $any-type)
                              (type->syntax (typed-value $typed)))
                            ((else $other)
                              (syntax-error $identifier "not type"))))
                        (else $value))))))))))))
)
