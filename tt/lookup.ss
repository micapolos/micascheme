(library (tt lookup)
  (export
    empty-lookup
    lookup-push
    lookup-push*
    lookup
    check-lookup)
  (import
    (scheme)
    (procedure)
    (syntax))

  (define empty-lookup
    (case-lambda
      (($id) #f)
      (($id $key) #f)))

  (define (lookup-push $lookup $key $value)
    (case-lambda
      (($id)
        (cond
          ((free-identifier=? $id $key) $value)
          (else ($lookup $id))))
      (($id $key)
        ($lookup $id $key))))

  (define (lookup-push* $lookup $keys $values)
    (fold-left lookup-push $lookup $keys $values))

  (define-rule-syntax (lookup (id x) ...)
    (fold-left
      lookup-push
      empty-lookup
      (list #'id ...)
      (list x ...)))

  (define-rule-syntax (check-lookup lookup (id x) ...)
    (lets
      ($lookup? lookup?)
      (run (equal? ($lookup #'id) x) ...)))
)
