(library (micalang typed)
  (export
    typed typed? typed-type typed-ref
    typed-equal?)
  (import (micalang base) (micalang term))

  (data (typed type ref))

  (define (typed-equal? $lhs $rhs)
    (and
      (term-equal? (typed-type $lhs) (typed-type $rhs))
      (equal? (typed-ref $lhs) (typed-ref $rhs))))
)
