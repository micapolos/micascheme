(library (typed evaluated)
  (export
    evaluated-promote
    evaluated-max-index?
    evaluated-list-max-index?
    evaluated-compiled
    combine-evaluated-list)
  (import
    (micascheme)
    (typed thunk)
    (typed compiled))

  ;(enum (evaluated thunk value))

  (define (evaluated-promote $environment $evaluated $depth)
    (switch $evaluated
      ((thunk? $thunk)
        (thunk-promote $environment $thunk $depth))
      ((else $value)
        $value)))

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

  (define (evaluated-compiled $evaluated)
    (switch $evaluated
      ((thunk? $thunk)
        (thunk-compiled $thunk))
      ((else $value)
        (value-compiled $value))))

  (define (combine-evaluated-list $environment $evaluated-list $datum-proc)
    (lets
      ($compiled
        (combine-compiled-list
          (map evaluated-compiled $evaluated-list)
          $datum-proc))
      ($max-index? (evaluated-list-max-index? $evaluated-list))
      (if $max-index?
        (thunk $max-index? $compiled)
        (compiled-value $environment $compiled))))
)
