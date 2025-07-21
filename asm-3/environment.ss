(library (asm-3 environment)
  (export
    environment environment? environment-identified-values
    empty-environment
    environment-with
    environment+
    environment+identified-value
    environment-ref
    environment->lookup
    list->environment
    environment-append
    environment->identifiers
    environment->entry-datums)
  (import
    (asm-3 base)
    (asm-3 identified))

  (data (environment identified-values))

  (define-rule-syntax (environment-with (id value) ...)
    (fluent (empty-environment)
      (environment+ #'id value) ...))

  (define (empty-environment)
    (environment (list)))

  (define (environment+ $environment $identifier $value)
    (environment
      (cons
        (identified $identifier $value)
        (environment-identified-values $environment))))

  (define (environment+identified-value $environment $identified-value)
    (environment
      (cons
        $identified-value
        (environment-identified-values $environment))))

  (define (environment-ref $environment $identifier)
    (or
      (lets?
        ($found
          (memp
            (lambda ($identified)
              (identified-identifier=? $identified $identifier))
            (environment-identified-values $environment)))
        (identified-ref (car $found)))
      (syntax-error $identifier "undefined")))

  (define (environment->lookup $environment)
    (lambda ($identifier)
      (environment-ref $environment $identifier)))

  (define-list->/append (environment $list)
    (environment (apply append (map environment-identified-values $list))))

  (define (environment->entry-datums $environment)
    (map identified->entry-datum (environment-identified-values $environment)))

  (define (environment->identifiers $environment)
    (map identified-identifier (environment-identified-values $environment)))
)
