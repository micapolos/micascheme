(library (asm dependent)
  (export
    dependent dependent? dependent-identifiers dependent-ref
    pure-dependent
    dependent-with
    dependent->datum
    dependent->syntax
    list->dependent
    dependent-append
    dependent-append-map
    dependent-map
    map-dependent
    check-dependent)
  (import (asm base))

  (data (dependent identifiers ref))

  (define-rule-syntax (dependent-with (identifier ...) ref)
    (dependent (list #'identifier ...) ref))

  (define (pure-dependent $ref)
    (dependent-with () $ref))

  (define dependent->datum
    (case-lambda
      (($ref->datum $dependent)
        `(dependent
          ,@(filter-opts
            (list
              (switch (dependent-identifiers $dependent)
                ((null? _) #f)
                ((else $identifiers) `(,@(map syntax->datum $identifiers))))
              ($ref->datum (dependent-ref $dependent))))))
      (($dependent)
        (dependent->datum identity $dependent))))

  (define-list->/append (dependent $dependents)
    (dependent
      (dedup free-identifier=? (apply append (map dependent-identifiers $dependents)))
      (map dependent-ref $dependents)))

  (define (dependent-append-map $ref-append . $dependent-list)
    (dependent-map
      (apply dependent-append $dependent-list)
      (partial apply $ref-append)))

  (define (dependent-map $dependent $proc)
    (dependent-with-ref $dependent
      ($proc (dependent-ref $dependent))))

  (define (map-dependent $proc $dependent)
    (dependent-map $dependent $proc))

  (define (dependent->syntax $dependent)
    #`(dependent-with (#,@(dependent-identifiers $dependent))
      #'#,(dependent-ref $dependent)))

  (define-rule-syntax (check-dependent in out)
    (check (equal? (dependent->datum in) (dependent->datum out))))
)
