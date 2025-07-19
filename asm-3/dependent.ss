(library (asm-3 dependent)
  (export
    dependent dependent? dependent-identifiers dependent-ref
    pure-dependent
    dependent-with
    dependent->datum
    list->dependent
    dependent-append
    dependent-append-with
    dependent-map
    check-dependent)
  (import (asm-3 base))

  (data (dependent identifiers ref))

  (define-rule-syntax (dependent-with (identifier ...) ref)
    (dependent (list #'identifier ...) ref))

  (define (pure-dependent $ref)
    (dependent-with () $ref))

  (define dependent->datum
    (case-lambda
      (($ref->datum $dependent)
        `(dependent-with
          (,@(map syntax->datum (dependent-identifiers $dependent)))
          ,($ref->datum (dependent-ref $dependent))))
      (($dependent)
        (dependent->datum identity $dependent))))

  (define-list->/append (dependent $dependents)
    (dependent
      (dedup free-identifier=? (apply append (map dependent-identifiers $dependents)))
      (map dependent-ref $dependents)))

  (define (dependent-append-with $ref-append . $dependent-list)
    (dependent-map
      (apply dependent-append $dependent-list)
      (partial apply $ref-append)))

  (define (dependent-map $dependent $proc)
    (dependent-with-ref $dependent
      ($proc (dependent-ref $dependent))))

  (define-rule-syntax (check-dependent in out)
    (check (equal? (dependent->datum in) (dependent->datum out))))
)
