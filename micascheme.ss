(library (micascheme)
  (export)

  (import 
    (rename
      (except (scheme) do)
      (cons scheme-cons))
    (rename (base)
      (script do))
    (base-syntax)
    (infix))

  (export
    cons
    (import 
      (rename
        (except (scheme) cons)
        (do do!))
      (rename (base) (script do))
      (base-syntax)
      (infix)))

  (define cons scheme-cons)
  (data-accessors (cons car cdr))
)
