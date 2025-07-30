(library (asm fragment)
  (export
    pure-fragment
    fragment->datum
    fragment->syntax
    fragment-pad
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

  (define (fragment-pad $fragment)
    (dependent-map $fragment
      (lambda ($aligned)
        (lets
          ($alignment (aligned-alignment $aligned))
          (aligned-map $aligned
            (lambda ($sized)
              (lets
                ($size (sized-size $sized))
                ($aligned-size (bitwise-align $size $alignment))
                ($aligned-slack (- $aligned-size $size))
                (sized $aligned-size
                  #`(relocable-map #,(sized-ref $sized)
                    (lambda ($binary)
                      (binary-append $binary
                        (zero-binary #,$aligned-slack))))))))))))

  (define-rule-syntax (check-fragment fragment out)
    (check (equal? (fragment->datum fragment) 'out)))
)
