(library (asm-3 linked)
  (export
    list->linked
    linked->datum
    check-list->linked)
  (import
    (asm-3 base)
    (asm-3 dependencies)
    (asm-2 relocable)
    (asm lookable)
    (asm-2 aligned)
    (asm-3 sized)
    (asm-3 located)
    (asm-3 identified)
    (asm-3 size-address)
    (asm-3 environment)
    (asm-3 environmental)
    (asm-3 environment))

  (define-type linked (environmental offset relocable-binary-syntax))

  (define (list->linked $identified-list)
    (lets
      ($identified-expression-syntax-list (filter (dot (not? aligned?) identified-ref) $identified-list))
      ($identified-aligned-list (filter (dot aligned? identified-ref) $identified-list))
      ($identified-sized-list (sort-identified-aligned-list $identified-aligned-list))
      ($sized-identifiers (map identified-identifier $identified-sized-list))
      ($sized-list (map identified-ref $identified-sized-list))
      ($sizes (map sized-size $sized-list))
      ($relocable-binary-syntax-list (map sized-ref $sized-list))
      ($offsets (sizes->addresses $sizes))
      ($relocated-binary-syntax-list
        (map
          (lambda ($offset $relocable-binary-syntax)
            #`(offset-relocable #,$offset #,$relocable-binary-syntax))
          $offsets
          $relocable-binary-syntax-list))
      ($relocable-binary-syntax
        #`(map-relocable list->binary
          (relocable-append #,@$relocated-binary-syntax-list)))
      ($label-let-entries
        (map
          (lambda ($identifier $offset) #`(#,$identifier (+ $org #,$offset)))
          $sized-identifiers
          $offsets))
      ($expression-let-entries
        (map
          (lambda ($identified)
            #`(
              #,(identified-identifier $identified)
              #,(identified-ref $identified)))
          $identified-expression-syntax-list))
      (environmental
        (environment (map identified $sized-identifiers $offsets))
        #`(relocable-with ($org)
          (lets
            #,@$label-let-entries
            #,@$expression-let-entries
            (relocable-ref #,$relocable-binary-syntax $org))))))

  (define (sort-identified-aligned-list $identified-aligned-list)
    (map
      (lambda ($identified)
        (identified-map $identified aligned-ref))
      (sort
        (ordered-by aligned-more? identified-ref)
        $identified-aligned-list)))

  (define (linked->datum $linked)
    `(linked
      ,@(environment->entry-datums (environmental-environment $linked))
      ,(syntax->datum (environmental-ref $linked))))

  (define-rule-syntax (check-list->linked in ... out)
    (check (equal? (linked->datum (list->linked (list in ...))) 'out)))
)
