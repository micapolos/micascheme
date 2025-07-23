(library (asm-3 fragment)
  (export
    fragment->datum
    check-fragment
    fragment->bytevector)
  (import
    (asm-3 base)
    (asm-2 aligned)
    (asm-3 sized)
    (asm-3 expression)
    (asm-3 syntax-expression)
    (asm-3 dependent)
    (asm-2 relocable)
    (asm lookable))

  (define-type fragment (dependent (aligned (sized (relocable (lookable (binary)))))))

  (define (fragment->datum $org $lookup $fragment)
    (dependent->datum
      (dependent-map $fragment
        (lambda ($aligned)
          (aligned->datum
            (aligned-map $aligned
              (lambda ($sized)
                (sized->datum
                  (sized-map $sized
                    (lambda ($relocable)
                      (binary->datum
                        (lookable-ref (relocable-ref $relocable $org) $lookup))))))))))))

  (define-rule-syntax (check-fragment org lookup fragment out)
    (check (equal? (fragment->datum org lookup fragment) 'out)))

  (define (fragment->bytevector $org $lookup $fragment)
    (binary->bytevector
      (lookable-ref
        (relocable-ref
          (sized-ref
            (aligned-ref
              (dependent-ref $fragment)))
          $org)
        $lookup)))
)
