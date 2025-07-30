(library (asm linked)
  (export
    list->linked
    linked->datum
    check-list->linked)
  (import
    (asm base)
    (asm dependencies)
    (asm relocable)
    (asm aligned)
    (asm sized)
    (asm located)
    (asm identified)
    (asm fragment)
    (asm size-address)
    (asm environment)
    (asm environmental)
    (asm environment))

  (define-type linked (environmental offset relocable-binary-syntax))

  (define (list->linked $identified-list)
    (lets
      ($identified-expression-syntax-list (filter (dot (not? aligned?) identified-ref) $identified-list))
      ($identified-aligned-list (filter (dot aligned? identified-ref) $identified-list))
      ($identified-sized-list (pack-fragments $identified-aligned-list))
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

  (define (pack-fragments $fragments)
    (lets
      ($sorted
        (sort
          (ordered-by aligned-more? identified-ref)
          $fragments))
      (map
        (lambda ($fragment) (identified-map $fragment aligned-ref))
        (reverse
          (fold-left
            (lambda ($fragments $fragment)
              (switch $fragments
                ((null? _)
                  (stack $fragment))
                ((else (pair $top $fragments))
                  (push
                    (push $fragments
                      (fragment-alignment-pad $top
                        (fragment-alignment $fragment)))
                    $fragment))))
            (stack)
            $sorted)))))

  (define (fragment-alignment-pad $fragment $alignment)
    (identified-map $fragment
      (lambda ($aligned)
        (aligned-map $aligned
          (lambda ($sized)
            (lets
              ($size (sized-size $sized))
              ($aligned-size (bitwise-align $size $alignment))
              ($aligned-slack (- $aligned-size $size))
              (if (zero? $aligned-slack)
                $sized
                (sized $aligned-size
                  #`(relocable-map #,(sized-ref $sized)
                    (lambda ($binary)
                      (binary-append $binary
                        (zero-binary #,$aligned-slack))))))))))))

  (define (fragment-alignment $fragment)
    (aligned-alignment (identified-ref $fragment)))

  (define (linked->datum $linked)
    `(linked
      ,@(environment->entry-datums (environmental-environment $linked))
      ,(syntax->datum (environmental-ref $linked))))

  (define-rule-syntax (check-list->linked in ... out)
    (check (equal? (linked->datum (list->linked (list in ...))) 'out)))
)
