(library (micalang compiled)
  (export
    compiled compiled? compiled-type compiled-type-term compiled-value-term
    compiled-equal?)
  (import (micalang base) (micalang term))

  (data (compiled type type-term value-term))

  (define (compiled-equal? $lhs $rhs)
    (and
      (term=? (compiled-type $lhs) (compiled-type $rhs))
      (equal? (compiled-type-term $lhs) (compiled-type-term $rhs))
      (equal? (compiled-value-term $lhs) (compiled-value-term $rhs))))
)
