(library (asm-3 expression)
  (export
    syntax->expression
    combine-expressions
    org
    expression-ref
    define-expression)
  (import
    (micascheme)
    (asm-3 dependent)
    (asm lookable)
    (asm-2 relocable))

  (define-keywords org)

  (data (expression dependent-lookable-relocable-value))

  (define-rule-syntax (define-expression id x)
    (define id
      (make-compile-time-value
        (syntax->expression #'x))))

  (define (expression-ref $org $lookup $expression)
    (relocable-ref
      (lookable-ref
        (dependent-ref
          (expression-dependent-lookable-relocable-value $expression))
        $lookup)
      $org))

  (define (combine-expressions $value-proc $expressions)
    (expression
      (dependent-map
        (lambda ($lookables)
          (lookable-map
            (lambda ($relocables)
              (relocable-map $value-proc
                (list->relocable $relocables)))
            (list->lookable $lookables)))
        (list->dependent
          (map expression-dependent-lookable-relocable-value $expressions)))))

  (define (syntax->expression $syntax)
    (syntax-case $syntax (org)
      (org
        (expression
          (dependent (list #'id)
            (lookable ($lookup)
              (relocable-with ($org)
                $org)))))
      (id
        (identifier? #'id)
        (expression
          (dependent (list #'id)
            (lookable ($lookup)
              (relocable-with ($org)
                ($lookup #'id))))))
      (literal
        ((or? boolean? integer? string? char?) (datum literal))
        (expression
          (dependent (list)
            (lookable ($lookup)
              (relocable-with ($org)
                (datum literal))))))
      ((fn arg ...)
        (combine-expressions
          (lambda ($values)
            (apply (car $values) (cdr $values)))
          (map syntax->expression #'(fn arg ...))))
      (other
        (syntax-error #'other "not expression"))))
)
