(library (typed evaluated)
  (export
    evaluated-max-index?
    evaluated-list-max-index?
    evaluated-bind)
  (import
    (micascheme)
    (typed thunk)
    (typed compiled))

  ;(enum (evaluated thunk value))

  (define (evaluated-max-index? $evaluated)
    (switch $evaluated
      ((thunk? $thunk) (thunk-max-index $thunk))
      ((else _) #f)))

  (define (evaluated-list-max-index? $evaluated-list)
    (fold-left
      (lambda ($max-index? $evaluated)
        (lets
          ($evaluated-max-index? (evaluated-max-index? $evaluated))
          (if $max-index?
            (if $evaluated-max-index?
              (max $max-index? $evaluated-max-index?)
              $max-index?)
            $evaluated-max-index?)))
      #f
      $evaluated-list))

  (define (evaluated-bind $environment $evaluated $value $datum-proc)
    (switch $evaluated
      ((thunk? $thunk)
        (thunk-bind $thunk $value $datum-proc))
      ((else $other)
        (compiled-value $environment
          (compiled-bind (value-compiled $other) $value $datum-proc)))))
)
