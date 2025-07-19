(library (asm-3 dependencies)
  (export
    dependencies-ref?
    resolve-dependencies
    dependencies->datum
    check-dependencies)
  (import (asm-3 base) (asm-3 identified) (asm-3 dependent) (syntax lookup))

  (define (dependencies-ref? $dependencies $identifier)
    (lets?
      ($dependencies
        (memp
          (lambda ($identified)
            (free-identifier=?
              (identified-identifier $identified)
              $identifier))
          $dependencies))
      (identified-ref (car $dependencies))))

  (define (dependencies+identifier $dependent-lookup $dependencies $identifier)
    (cond
      ((dependencies-ref? $dependencies $identifier)
        $dependencies)
      (else
        (lets
          ($dependent (lookup-ref $dependent-lookup $identifier))
          (push
            (dependencies+identifiers $dependent-lookup $dependencies (dependent-identifiers $dependent))
            (identified $identifier (dependent-ref $dependent)))))))

  (define (dependencies+identifiers $dependent-lookup $dependencies $identifiers)
    (fold-left
      (partial dependencies+identifier $dependent-lookup)
      $dependencies
      $identifiers))

  (define (resolve-dependencies $dependent-lookup $identifier)
    (reverse (dependencies+identifier $dependent-lookup (stack) $identifier)))

  (define (dependencies->datum $ref->datum $dependencies)
    `(dependencies
      ,@(map-with ($identified $dependencies)
          (identified->datum $ref->datum $identified))))

  (define-rule-syntax (check-dependencies in out)
    (check (equal? (dependencies->datum identity in) 'out)))
)
