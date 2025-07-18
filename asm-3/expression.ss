(library (asm-3 expression)
  (export
    expression expression? expression-dependent-lookable-relocable-value
    combine-expressions
    syntax->expression)
  (import
    (micascheme)
    (asm-3 dependent)
    (asm lookable)
    (asm-2 relocable))

  (data (expression dependent-lookable-relocable-value))

  (define (combine-expressions $value-proc $expressions)
    (dependent-map
      (lambda ($lookables)
        (lookable-map
          (lambda ($relocables)
            (relocable-map $value-proc
              (list->relocable $relocables)))
          (list->lookable $lookables)))
      (list->dependent $expressions)))

  (define (syntax->expression $syntax)
    (syntax-case $syntax ()
      (id
        (identifier? #'id)
        (dependent (list #'id)
          (lookable ($lookup)
            (relocable-with ($org)
              ($lookup #'id)))))
      (literal
        ((or? boolean? integer? string? char?) (datum literal))
        (dependent (list)
          (lookable ($lookup)
            (relocable-with ($org)
              (datum literal)))))
      (expression
        #'expression)))
)
