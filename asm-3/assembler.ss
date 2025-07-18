(library (asm-3 assembler)
  (export)
  (import))

  (define (assemble $identified-items)
    (lets
      ($identified-fragments
        (filter
          (lambda ($identified) (fragment? (identified-ref $identified)))
          $identified-items))
      ($identified-expressions
        (filter
          (lambda ($identified) (expression? (identified-ref $identified)))
          $identified-items))
      ($identified-sized-binary-expressions
        (map
          (lambda ($identified)
            (identified-map aligned-ref $identified))
          (sort
            (lambda ($identified-aligned-1 $identified-aligned-2)
              (aligned-more?
                (identified-ref $identified-aligned-1)
                (identified-ref $identified-aligned-2)))
            (map fragment-aligned-sized-binary-expression $identified-fragments))))
)
