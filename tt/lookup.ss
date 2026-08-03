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

  (define lookup-push
    (case-lambda
      (($lookup $id $value)
        (case-lambda
          (($lookup-id)
            (cond
              ((free-identifier=? $lookup-id $id) $value)
              (else ($lookup $lookup-id))))
          (($lookup-id $lookup-key)
            ($lookup $lookup-id $lookup-key))))
      (($lookup $id $key $value)
        (case-lambda
          (($lookup-id)
            ($lookup $lookup-id))
          (($lookup-id $lookup-key)
            (cond
              ((and
                (free-identifier=? $lookup-id $id)
                (free-identifier=? $lookup-key $key)) $value)
              (else ($lookup $lookup-id $lookup-key))))))))

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
