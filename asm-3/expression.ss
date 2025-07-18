(library (asm-3 expression)
  (export
    syntax->dependent-expression
    combine-dependent-expressions
    org
    expression-ref
    dependent-expression-ref
    define-expression)
  (import
    (micascheme)
    (asm-3 dependent)
    (asm lookable)
    (asm-2 relocable))

  (define-keywords org)

  (data (expression lookable-relocable-value))

  (define-rule-syntax (define-expression id x)
    (define id
      (make-compile-time-value
        (syntax->dependent-expression #'x))))

  (define (dependent-expression-ref $lookup $org $dependent-expression)
    (expression-ref $lookup $org (dependent-ref $dependent-expression)))

  (define (expression-ref $lookup $org $expression)
    (relocable-ref
      (lookable-ref
        (expression-lookable-relocable-value $expression)
        $lookup)
      $org))

  (define (combine-dependent-expressions $value-proc $dependent-expressions)
    (dependent-map
      (lambda ($expressions)
        (expression
          (lookable-map
            (lambda ($relocables)
              (relocable-map $value-proc
                (list->relocable $relocables)))
            (list->lookable (map expression-lookable-relocable-value $expressions)))))
      (list->dependent $dependent-expressions)))

  (define (syntax->dependent-expression $syntax)
    (syntax-case $syntax (org)
      (org
        (dependent (list #'id)
          (expression
            (lookable ($lookup)
              (relocable-with ($org)
                $org)))))
      (id
        (identifier? #'id)
        (dependent (list #'id)
          (expression
            (lookable ($lookup)
              (relocable-with ($org)
                ($lookup #'id))))))
      (literal
        ((or? boolean? integer? string? char?) (datum literal))
        (dependent (list)
          (expression
            (lookable ($lookup)
              (relocable-with ($org)
                (datum literal))))))
      ((fn arg ...)
        (combine-dependent-expressions
          (lambda ($values)
            (apply (car $values) (cdr $values)))
          (map syntax->dependent-expression #'(fn arg ...))))
      (other
        (syntax-error #'other "not expression"))))
)
