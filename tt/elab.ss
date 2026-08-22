(library
  (tt elab)
  (export
    elab
    elab-let
    elaborate
    (rename (%check check)))
  (import
    (scheme)
    (lets)
    (syntax)
    (syntaxes)
    (check)
    (procedure)
    (tt lookup))

  (define (elaborate $lookup $elab)
    ($elab $lookup))

  (define-rules-syntax
    ((elab x)
      (lambda ($lookup)
        (values $lookup x)))
    ((elab (id val) . x)
      (identifier? #'id)
      (lambda ($lookup)
        (elaborate
          (lookup-push $lookup #'id val)
          (elab . x)))))

  (define-rules-syntax
    ((elab-let x) x)
    ((elab-let (id expr) . x)
      (identifier? #'id)
      (lambda ($lookup)
        (lets
          ((values $lookup id) (elaborate $lookup expr))
          (elaborate $lookup (elab-let . x))))))

  (define-rules-syntax
    (keywords elaborate)
    ((%check
      (elaborate (id val) ... x)
      (id2 val2) ... x2)
      (for-all identifier? #'(id ... id2 ...))
      (lets
        ((values $lookup $x)
          (elaborate
            (lookup (id val) ...)
            x))
        (run
          (check (equal? $x x2))
          (check-lookup $lookup (id2 val2) ...))))
    ((%check . x)
      (check . x)))
)
