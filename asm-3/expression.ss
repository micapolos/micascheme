(library (asm-3 expression)
  (export
    org
    pure-expression
    org-expression
    identifier-expression
    application-expression
    combine-expressions
    expression-map
    expression-ref
    expression->datum
    offset-expression
    check-expression)
  (import
    (asm-3 base)
    (asm-3 dependent)
    (asm lookable)
    (asm-2 relocable)
    (asm-3 org)
    (syntax lookup))

  (define-type (expression ref) (dependent (relocable (lookable ref))))

  (define (pure-expression $value)
    (dependent (list)
      (relocable-with ($org)
        (lookable ($lookup)
          $value))))

  (define (org-expression)
    (pure-dependent
      (relocable-with ($org)
        (pure-lookable
          $org))))

  (define (identifier-expression $identifier)
    (dependent (list $identifier)
      (relocable-with ($org)
        (lookable ($lookup)
          (lookup-ref $lookup $identifier)))))

  (define (application-expression $fn-expression . $arg-expressions)
    (apply dependent-append-map
      (lambda ($fn-relocable . $arg-relocables)
        (apply relocable-append-map
          (lambda ($fn-lookable . $arg-lookables)
            (apply lookable-append-map
              (lambda ($fn . $args)
                (apply $fn $args))
              (cons $fn-lookable $arg-lookables)))
          (cons $fn-relocable $arg-relocables)))
      (cons $fn-expression $arg-expressions)))

  (define (expression-ref $org $lookup $expression)
    (lookable-ref (relocable-ref (dependent-ref $expression) $org) $lookup))

  (define (offset-expression $offset $expression)
    (map-dependent (partial offset-relocable $offset) $expression))

  (define (expression-map $expression $proc)
    (dependent-map $expression
      (lambda ($relocable)
        (relocable-map $relocable
          (lambda ($lookable)
            (lookable-map $lookable
              (lambda ($ref)
                ($proc $ref))))))))

  (define (combine-expressions $value-proc $expressions)
    (dependent-map (list->dependent $expressions)
      (lambda ($relocables)
        (relocable-map
          (list->relocable $relocables)
          (lambda ($lookables)
            (lookable-map (list->lookable $lookables) $value-proc))))))

  (define (expression->datum $org $lookup $expression)
    `(expression
      ,(dependent->datum
        (dependent-map $expression
          (lambda ($relocable)
            (lookable-ref (relocable-ref $relocable $org) $lookup))))))

  (define-rule-syntax (check-expression org lookup expression datum)
    (check (equal? (expression->datum org lookup expression) 'datum)))
)
