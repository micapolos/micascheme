(library (micalang compiled)
  (export
    compiled compiled? compiled-type compiled-ref
    compiled-equal?)
  (import (micalang base) (micalang term))

  (data (compiled type ref))

  (define (compiled-equal? $lhs $rhs)
    (and
      (term-equal? (compiled-type $lhs) (compiled-type $rhs))
      (equal? (compiled-ref $lhs) (compiled-ref $rhs))))
)
