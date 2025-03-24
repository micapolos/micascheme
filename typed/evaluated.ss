(library (typed evaluated)
  (export
    evaluated-promote
    evaluated-max-index?
    evaluated-list-max-index?
    evaluated-compiled
    combine-evaluated-list
    evaluated-bound)
  (import
    (micascheme)
    (typed thunk)
    (typed compiled)
    (typed hole)
    (typed combo))

  ;(enum (evaluated thunk combo))

  (define (evaluated-promote $environment $evaluated $depth)
    (switch-exhaustive $evaluated
      ((thunk? $thunk)
        (thunk-promote $environment $thunk $depth))
      ((combo? $combo)
        $combo)))

  (define (evaluated-max-index? $evaluated)
    (switch-exhaustive $evaluated
      ((thunk? $thunk) (thunk-max-index $thunk))
      ((combo? $combo) #f)))

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
    (switch-exhaustive $evaluated
      ((thunk? $thunk)
        (thunk-compiled $thunk))
      ((combo? $combo)
        (combo-compiled $combo))))

  (define (combine-evaluated-list $environment $evaluated-list $datum-proc)
    (lets
      ($compiled
        (combine-compiled-list
          (map evaluated-compiled $evaluated-list)
          $datum-proc))
      ($max-index? (evaluated-list-max-index? $evaluated-list))
      (if $max-index?
        (thunk $max-index? $compiled)
        (compiled-combo $environment $compiled))))

  (define (evaluated-bound $evaluated)
    (switch $evaluated
      ((thunk? $thunk) hole)
      ((combo? $combo) $combo)))
)
