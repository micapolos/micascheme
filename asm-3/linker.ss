(library (asm-3 linker)
  (export
    assemble-fragment
    assemble-identifier)
  (import
    (asm-3 base)
    (asm-3 dependencies)
    (asm-2 relocable)
    (asm lookable)
    (asm-3 linked)
    (asm-2 aligned)
    (asm-3 sized)
    (asm-3 located)
    (asm-3 identified)
    (asm-3 size-address)
    (asm-3 environment))

  (define (assemble-identifier $lookup $org $identifier)
    (assemble-fragment $lookup $org (lookup-ref $lookup $identifier)))

  (define (assemble-fragment $lookup $org $fragment)
    (lets
      ($lookup (lookup+ $lookup #'main $fragment))
      ($identified-list (resolve-dependencies $lookup #'main))
      ($identified-aligned-list (filter (dot aligned? identified-ref) $identified-list))
      ($identified-relocable-list (filter (dot relocable? identified-ref) $identified-list))
      ($identified-sized-list (sort-identified-aligned-list $identified-aligned-list))
      ($identifiers (map identified-identifier $identified-sized-list))
      ($sizes (map (dot sized-size identified-ref) $identified-sized-list))
      ($relocables (map (dot sized-ref identified-ref) $identified-sized-list))
      ($offsets (sizes->addresses $sizes))
      ($addresses (map (partial + $org) $offsets))
      ($lookables (map locate-relocable $addresses $relocables))
      ($environment (fold-left environment+ (empty-environment) $identifiers $addresses))
      ($environment
        (fold-left
          (lambda ($environment $identifier $lookable)
            (environment+ $environment $identifier (lookable-ref $lookable (environment->lookup $environment))))
          $environment
          (map identified-identifier $identified-relocable-list)
          (map (dot (partial locate-relocable $org) identified-ref) $identified-relocable-list)))
      ($lookup (environment->lookup $environment))
      ($binaries (map (partial resolve-lookable $lookup) $lookables))
      ($binary (list->binary $binaries))
      ($bytevector (binary->bytevector $binary))
      (assembled ($lookup #'main) $bytevector)))

  (define (sort-identified-aligned-list $identified-aligned-list)
    (map
      (lambda ($identified)
        (identified-map $identified aligned-ref))
      (sort
        (ordered-by aligned-more? identified-ref)
        $identified-aligned-list)))
)
