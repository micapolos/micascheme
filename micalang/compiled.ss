(library (micalang compiled)
  (export
    compiled compiled? compiled-type compiled-type-term compiled-ref
    compiled-equal?)
  (import (micalang base) (micalang term))

  (data (compiled type type-term ref))

  (define (compiled-equal? $lhs $rhs)
    (and
      (term-equal? (compiled-type $lhs) (compiled-type $rhs))
      (equal? (compiled-type-term $lhs) (compiled-type-term $rhs))
      (equal? (compiled-ref $lhs) (compiled-ref $rhs))))
)
