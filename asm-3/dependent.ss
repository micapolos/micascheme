(library (asm-3 dependent)
  (export
    dependent dependent? dependent-dep-stack dependent-ref
    pure-dependent
    dependent-with
    dependent->datum
    list->dependent
    dependent-append
    dependent-map
    check-dependent)
  (import (micascheme))

  (data (dependent dep-stack ref))

  (define-rule-syntax (dependent-with (dep ...) ref)
    (dependent (stack #'dep ...) ref))

  (define (pure-dependent $ref)
    (dependent-with () $ref))

  (define (dependent+dep $dependent $dep)
    (lets
      ($dep-stack (dependent-dep-stack $dependent))
      (dependent-with-dep-stack $dependent
        (cond
          ((memp (partial free-identifier=? $dep) $dep-stack) $dep-stack)
          (else (push $dep-stack $dep))))))

  (define (dependent->datum $dependent)
    `(dependent-with
      (,@(reverse (map syntax->datum (dependent-dep-stack $dependent))))
      ,(dependent-ref $dependent)))

  (define (list->dependent $dependents)
    (dependent
      (reverse (dedup free-identifier=? (apply append (map (dot reverse dependent-dep-stack) $dependents))))
      (map dependent-ref $dependents)))

  (define (dependent-append . $dependents)
    (list->dependent $dependents))

  (define (dependent-map $proc $dependent)
    (dependent-with-ref $dependent
      ($proc (dependent-ref $dependent))))

  (define-rule-syntax (check-dependent in out)
    (check (equal? (dependent->datum in) (dependent->datum out))))
)
