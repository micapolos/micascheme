(library (typico scoped)
  (export
    scoped scoped? scoped-lookup scoped-ref
    scoped->datum
    scoped-expand-typed)
  (import
    (micascheme)
    (typico type)
    (typico typed)
    (typico core types)
    (typico expand))

  (data (scoped lookup ref))

  (define (scoped->datum $scoped)
    (datum/annotation-stripped (scoped-ref $scoped)))

  (define (scoped-expand-typed (scoped $lookup $syntax))
    (lets
      ($typed (expand-typed $lookup $syntax))
      ($type (typed-type $typed))
      (cond
        ((type=? $type syntax-type)
          (scoped-expand-typed (typed-value $typed)))
        (else
          $typed))))
)
