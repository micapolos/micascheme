(library (asm-3 expression)
  (export
    org
    pure-expression
    org-expression
    identifier-expression
    application-expression
    syntax->expression
    combine-expressions
    expression-map
    expression-ref
    expression->datum
    check-expression)
  (import
    (asm-3 base)
    (asm-3 dependent)
    (asm lookable)
    (asm-2 relocable)
    (asm-3 org))

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
          ($lookup $identifier)))))

  (define (application-expression $fn-expression . $arg-expressions)
    (apply dependent-append-with
      (lambda ($fn-relocable . $arg-relocables)
        (apply relocable-append-with
          (lambda ($fn-lookable . $arg-lookables)
            (apply lookable-append-with
              (lambda ($fn . $args)
                (apply $fn $args))
              (cons $fn-lookable $arg-lookables)))
          (cons $fn-relocable $arg-relocables)))
      (cons $fn-expression $arg-expressions)))

  (define (expression-ref $org $lookup $expression)
    (lookable-ref (relocable-ref (dependent-ref $expression) $org) $lookup))

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

  (define (syntax->expression $syntax)
    (syntax-case $syntax (org)
      (org
        (dependent (list)
          (relocable-with ($org)
            (lookable ($lookup)
              $org))))
      (id
        (identifier? #'id)
        (dependent (list #'id)
          (relocable-with ($org)
            (lookable ($lookup)
              ($lookup #'id)))))
      (literal
        ((or? boolean? integer? string? char?) (datum literal))
        (dependent (list)
          (relocable-with ($org)
            (lookable ($lookup)
              (datum literal)))))
      ((fn arg ...)
        (combine-expressions
          (lambda ($values)
            (apply (car $values) (cdr $values)))
          (map syntax->expression #'(fn arg ...))))
      (other
        (syntax-error #'other "not expression"))))

  (define (expression->datum $org $lookup $expression)
    `(expression
      ,(dependent->datum
        (dependent-map $expression
          (lambda ($relocable)
            (lookable-ref (relocable-ref $relocable $org) $lookup))))))

  (define-rule-syntax (check-expression org lookup expression datum)
    (check (equal? (expression->datum org lookup expression) 'datum)))
)
