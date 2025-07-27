(library (asm dependencies)
  (export
    dependencies
    dependencies-ref?
    resolve-dependencies
    dependencies->datum
    check-dependencies
    dependencies-without
    dependencies-without-all
    check-environment)
  (import (asm base) (asm identified) (asm dependent) (syntax lookup) (asm environment))

  (define-rule-syntax (dependencies x ...)
    (list #'x ...))

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

  (define (dependencies-without $dependencies $identifier)
    (remp (partial free-identifier=? $identifier) $dependencies))

  (define (dependencies-without-all $dependencies $identifiers)
    (fold-left dependencies-without $dependencies $identifiers))

  (define (resolve-dependencies $dependent-lookup $identifier)
    (reverse (dependencies+identifier $dependent-lookup (stack) $identifier)))

  (define (environment->datum $ref->datum $dependencies)
    `(dependencies
      ,@(map-with ($identified $dependencies)
          (identified->datum $ref->datum $identified))))

  (define (dependencies->datum $dependencies)
    `(dependencies ,@(map syntax->datum $dependencies)))

  (define-rule-syntax (check-environment in out)
    (check (equal? (environment->datum identity in) 'out)))

  (define-rule-syntax (check-dependencies in out)
    (check (equal? (dependencies->datum in) 'out)))
)
