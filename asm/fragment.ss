(library (asm fragment)
  (export
    pure-fragment
    fragment->datum
    fragment->syntax
    check-fragment)
  (import
    (asm base)
    (asm aligned)
    (asm sized)
    (asm dependent)
    (asm relocable))

  (define-type fragment (dependent (aligned (sized (syntax (relocable binary))))))

  (define (pure-fragment $syntax)
    (pure-dependent (pure-aligned (pure-sized #'(pure-relocable #,$syntax)))))

  (define (fragment->datum $fragment)
    (dependent->datum
      (dependent-map $fragment
        (lambda ($aligned)
          (aligned->datum
            (aligned-map $aligned
              (lambda ($sized)
                (sized->datum
                  (sized-map $sized syntax->datum)))))))))

  (define (fragment->syntax $fragment)
    (lets
      ($dependencies (dependent-identifiers $fragment))
      ($aligned (dependent-ref $fragment))
      ($alignment (aligned-alignment $aligned))
      ($sized (aligned-ref $aligned))
      ($size (sized-size $sized))
      ($relocable-binary-syntax (sized-ref $sized))
      #`(dependent-with (#,@$dependencies)
        (aligned #,$alignment
          (sized #,$size
            #'#,$relocable-binary-syntax)))))

  (define-rule-syntax (check-fragment fragment out)
    (check (equal? (fragment->datum fragment) 'out)))
)
