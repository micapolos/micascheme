(library (tt lookup)
  (export
    false-lookup
    syntax-error-lookup
    lookup-push
    lookup-push*)
  (import
    (scheme)
    (procedure))

  (define false-lookup (lambda ($id) #f))

  (define (syntax-error-lookup $message)
    (lambda ($id)
      (syntax-error $id $message)))

  (define (lookup-push $eq? $lookup $key $value)
    (lambda ($id) (if ($eq? $id $key) $value ($lookup $id))))

  (define (lookup-push* $eq? $lookup $keys $values)
    (fold-left (partial lookup-push $eq?) $lookup $keys $values))
)
