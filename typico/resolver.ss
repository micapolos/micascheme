(library (typico resolver)
  (export
    resolver
    empty-resolver
    list->resolver
    resolver-append
    resolve?
    resolve)
  (import
    (typico base)
    (typico resolved))

  (define-rule-syntax (resolver ($resolver $value) body)
    (lambda ($resolver $value) body))

  (define (empty-resolver)
    (resolver ($resolver $value) #f))

  (define (list->resolver $resolvers)
    (resolver ($resolver $value)
      (exists
        (lambda ($inner-resolver)
          ($inner-resolver $resolver $value))
        $resolvers)))

  (define (resolver-append . $resolvers)
    (list->resolver $resolvers))

  (define (resolve? $resolver $value)
    ($resolver $resolver $value))

  (define (resolve $resolver $value)
    (switch (resolve? $resolver $value)
      ((resolved? $resolved) (resolved-ref $resolved))
      ((false? _) $value)))
)
