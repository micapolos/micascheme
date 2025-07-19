(library (asm-3 expression)
  (export
    org
    pure-expression
    org-expression
    syntax->expression
    combine-expressions
    expression-ref)
  (import
    (asm-3 base)
    (asm-3 dependent)
    (asm lookable)
    (asm-2 relocable)
    (asm-3 org))

  ; expression -> dependent-relocable-lookable-value

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

  (define (expression-ref $org $lookup $expression)
    (lookable-ref (relocable-ref (dependent-ref $expression) $org) $lookup))

  (define (combine-expressions $value-proc $expressions)
    (dependent-map
      (lambda ($relocables)
        (relocable-map
          (lambda ($lookables)
            (lookable-map $value-proc
              (list->lookable $lookables)))
          (list->relocable $relocables)))
      (list->dependent $expressions)))

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
)
