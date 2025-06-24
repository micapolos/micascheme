(library (typico lookup)
  (export empty-lookup lookup+)
  (import (micascheme))

  (define (empty-lookup)
    (lambda ($symbol) #f))

  (define (lookup+ $lookup $symbol $value)
    (lambda ($lookup-symbol)
      (cond
        ((symbol=? $lookup-symbol $symbol) $value)
        (else ($lookup $lookup-symbol)))))
)
