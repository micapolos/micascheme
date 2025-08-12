(library (asm linked)
  (export
    print-linked?
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

  (define print-linked? (make-thread-parameter #f))

  (define-type linked (environmental offset relocable-binary-syntax))

  (define list->linked
    (case-lambda
      (($identified-list)
        (list->linked $identified-list #t))
      (($identified-list $gen?)
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
          ($sized-tmps (if $gen? (generate-temporaries $sized-identifiers) $sized-identifiers))
          ($expression-identifiers (map identified-identifier $identified-expression-syntax-list))
          ($expression-tmps (if $gen? (generate-temporaries $expression-identifiers) $expression-identifiers))
          ($rewrite-ids
            (lambda ($syntax)
              (syntax-replace-all
                $sized-identifiers
                $sized-tmps
                (syntax-replace-all
                  $expression-identifiers
                  $expression-tmps
                  $syntax))))
          ($relocable-binary-syntax ($rewrite-ids $relocable-binary-syntax))
          ($label-let-entries
            (map
              (lambda ($identifier $offset) #`(#,$identifier (+ $org #,$offset)))
              $sized-tmps
              $offsets))
          ($expression-syntaxes (map identified-ref $identified-expression-syntax-list))
          ($expression-let-entries
            (map
              (lambda ($id $expr) #`(#,$id #,($rewrite-ids $expr)))
              $expression-tmps
              $expression-syntaxes))
          (run
            (when (print-linked?)
              (parameterize ((print-radix 16))
                (pretty-print
                  `(fragments
                    ,@(map
                      (lambda ($id $offset $size)
                        `(,(syntax->datum $id)
                          (offset ,$offset) (size ,$size)))
                      $sized-identifiers
                      $offsets
                      $sizes))))))
          (environmental
            (environment (map identified $sized-identifiers $offsets))
            #`(relocable-with ($org)
              (lets
                #,@$label-let-entries
                #,@$expression-let-entries
                (relocable-ref #,$relocable-binary-syntax $org))))))))

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
    (check (equal? (linked->datum (list->linked (list in ...) #f)) 'out)))
)
