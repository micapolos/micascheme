(library (asm-3 assembler)
  (export)
  (import)

  (define (assemble $identified-list)
    (lets
      ($identified-aligned-list
        (filter
          (lambda ($identified) (aligned? (identified-ref $identified)))
          $identified-list))
      ($identified-relocable-list
        (filter
          (lambda ($identified) (relocable? (identified-ref $identified)))
          $identified-list))
      ($identified-sized-list
        (map
          (lambda ($identified)
            (identified-map aligned-ref $identified))
          (sort
            (lambda ($identified-aligned-1 $identified-aligned-2)
              (aligned-more?
                (identified-ref $identified-aligned-1)
                (identified-ref $identified-aligned-2)))
            $identified-aligned-list)))
      ($next
        (identified-map
          (lambda ($sized-relocable-list)
            (list->sized-relocable $sized-relocable-list))
          (list->identified $identified-sized-list)))))

  (define (identified-sized-list->identified-))
)
