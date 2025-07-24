(library (asm-3 fragment)
  (export
    fragment?
    pure-fragment
    fragment->datum
    fragment->syntax
    check-fragment)
  (import
    (asm-3 base)
    (asm-2 aligned)
    (asm-3 sized)
    (asm-3 dependent))

  (define-type fragment (dependent (aligned (sized (syntax (relocable binary))))))

  (define (pure-fragment $syntax)
    (pure-dependent (pure-aligned (pure-sized #'(pure-relocable #,$syntax)))))

  (define (fragment? $obj)
    (and (dependent? $obj)
      (lets ($obj (dependent-ref $obj))
        (and (aligned? $obj)
          (lets ($obj (aligned-ref $obj))
            (and (sized? $obj)
              (lets ($obj (sized-ref $obj))
                (syntax? $obj))))))))

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
            #,$relocable-binary-syntax)))))

  (define-rule-syntax (check-fragment fragment out)
    (check (equal? (fragment->datum fragment) 'out)))
)
