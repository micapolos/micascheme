(library (tt lookup)
  (export
    empty-lookup?
    syntax-error-lookup
    lookup-push
    lookup-push*
    identifier-lookup
    symbol-lookup?)
  (import
    (scheme)
    (procedure)
    (syntax))

  (define empty-lookup? (lambda ($id) #f))

  (define (syntax-error-lookup $message)
    (lambda ($id)
      (syntax-error $id $message)))

  (define (lookup-push $eq? $lookup $key $value)
    (lambda ($id) (if ($eq? $id $key) $value ($lookup $id))))

  (define (lookup-push* $eq? $lookup $keys $values)
    (fold-left (partial lookup-push $eq?) $lookup $keys $values))

  (define-rule-syntax (identifier-lookup (id x) ...)
    (fold-left
      (partial lookup-push free-identifier=?)
      syntax-error-lookup
      (list #'id ...)
      (list x ...)))

  (define-rule-syntax (symbol-lookup? (id x) ...)
    (fold-left
      (partial lookup-push symbol=?)
      empty-lookup?
      (list 'id ...)
      (list x ...)))
)
