(library (asm-3 syntax-block)
  (export
    align
    syntax->block
    check-syntax->block
    trace-block-expansion)
  (import (asm-3 base) (asm-3 block) (asm-3 syntax-expression))

  (define-keywords align)

  (define trace-block-expansion (make-parameter #f))

  (define (tracing $syntax)
    (lets
      (run (when (trace-block-expansion) (pretty-print (syntax->datum $syntax))))
      $syntax))

  (define (syntax->block $lookup $syntax)
    (syntax-case (tracing $syntax) (syntax begin align)
      ((begin x ...)
        (list->block (map (partial syntax->block $lookup) #'(x ...))))
      ((align x)
        (integer? (datum x))
        (align-block (datum x)))
      (id
        (identifier? #'id)
        (identifier-block #'id))
      ((id . x)
        (identifier? #'id)
        (switch ($lookup #'id)
          ((false? _) (syntax-error#'id "undefined block syntax"))
          ((else $transformer)
            (switch (transform $transformer $syntax $lookup)
              ((block? $block) $block)
              ((else $other) (syntax->block $lookup $other))))))
      (other
        (syntax-error $syntax "invalid block syntax"))))

  (define-rule-syntax (check-syntax->block lookup in out)
    (check-block (syntax->block lookup #'in) out))
)
