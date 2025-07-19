(library (asm-3 assembler)
  (export assemble)
  (import
    (micascheme)
    (syntax lookup)
    (asm-3 dependencies)
    (asm-2 relocable)
    (asm lookable)
    (asm-2 aligned)
    (asm-3 sized)
    (asm-3 located)
    (asm-3 identified)
    (asm-3 size-address))

  (define (assemble $lookup $org $identifier)
    (lets
      ($identified-list (resolve-dependencies $lookup $identifier))
      ($identified-aligned-list
        (filter
          (lambda ($identified) (aligned? (identified-ref $identified)))
          $identified-list))
      ($identified-relocable-list
        (filter
          (lambda ($identified) (relocable? (identified-ref $identified)))
          $identified-list))
      ($identified-sized-list
        (sort-identified-aligned-list $identified-aligned-list))
      ($identifiers (map identified-identifier $identified-sized-list))
      ($sizes (map (dot sized-size identified-ref) $identified-sized-list))
      ($relocables (map (dot sized-ref identified-ref) $identified-sized-list))
      ($offsets (sizes->addresses $sizes))
      ($addresses (map (partial + $org) $offsets))
      ($lookables (map locate-relocable $addresses $relocables))
      ($lookup (fold-left lookup+ (empty-lookup) $identifiers $addresses))
      ($lookup
        (fold-left
          (lambda ($lookup $identifier $lookable)
            (lookup+ $lookup $identifier (lookable-ref $lookable $lookup)))
          $lookup
          (map identified-identifier $identified-relocable-list)
          (map (dot (partial locate-relocable $org) identified-ref) $identified-relocable-list)))
      ($binaries (map (partial resolve-lookable $lookup) $lookables))
      ($binary (list->binary $binaries))
      ($bytevector (binary->bytevector $binary))
      (located ($lookup $identifier) $bytevector)))

  (define (sort-identified-aligned-list $identified-aligned-list)
    (map
      (lambda ($identified)
        (identified-map aligned-ref $identified))
      (sort
        (lambda ($identified-aligned-1 $identified-aligned-2)
          (aligned-more?
            (identified-ref $identified-aligned-1)
            (identified-ref $identified-aligned-2)))
        $identified-aligned-list)))
)
