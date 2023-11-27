(library (micascheme)
  (export)

  (import 
    (rename
      (except (scheme) do)
      (cons scheme-cons))
    (rename (base)
      (script do))
    (base-syntax)
    (base-transformers)
    (infix))

  (export
    cons
    (import 
      (rename
        (except (scheme) cons)
        (do do!))
      (rename (base) (script do))
      (base-syntax)
      (base-transformers)
      (infix)))

  (define cons scheme-cons)
  (define-binder (cons car cdr))
)
