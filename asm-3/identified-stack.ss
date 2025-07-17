(library (asm-3 identified-stack)
  (export
    identified-stack-ref?
    identified-stack+identifier
    identifier->identified-stack
    identified-stack->datum
    check-identified-stack)
  (import (micascheme) (asm-3 identified) (asm-3 dependent) (syntax lookup))

  (define (identified-stack-ref? $identified-stack $identifier)
    (lets?
      ($identified-stack
        (memp
          (lambda ($identified)
            (free-identifier=?
              (identified-identifier $identified)
              $identifier))
          $identified-stack))
      (identified-ref (car $identified-stack))))

  (define (identified-stack+identifier $dependent-lookup $identified-stack $identifier)
    (cond
      ((identified-stack-ref? $identified-stack $identifier)
        $identified-stack)
      (else
        (lets
          ($dependent (lookup-ref $dependent-lookup $identifier))
          (push
            (fold-left
              (partial identified-stack+identifier $dependent-lookup)
              $identified-stack
              (dependent-identifiers $dependent))
            (identified $identifier (dependent-ref $dependent)))))))

  (define (identifier->identified-stack $dependent-lookup $identifier)
    (identified-stack+identifier $dependent-lookup (stack) $identifier))

  (define (identified-stack->datum $ref->datum $identified-stack)
    `(stack
      ,@(reverse
        (map-with ($identified $identified-stack)
          (identified->datum $ref->datum $identified)))))

  (define-rule-syntax (check-identified-stack in out)
    (check (equal? (identified-stack->datum identity in) 'out)))
)
