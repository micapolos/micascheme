(library (micalang typed)
  (export
    typed typed? typed-type typed-type-term typed-ref
    typed-equal?)
  (import (micalang base) (micalang term))

  (data (typed type type-term ref))

  (define (typed-equal? $lhs $rhs)
    (and
      (term-equal? (typed-type $lhs) (typed-type $rhs))
      (equal? (typed-ref $lhs) (typed-ref $rhs))))
)
