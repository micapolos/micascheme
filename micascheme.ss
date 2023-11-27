(library (micascheme)
  (export)

  (import 
    (rename
      (except (scheme) do)
      (cons scheme-cons))
    (rename (base)
      (script do))
    (base-syntax)
    (binder)
    (check)
    (data)
    (identifier)
    (lets)
    (syntax)
    (infix)
    (switch)
    (throw))

  (export
    cons
    (import 
      (rename
        (except (scheme) cons)
        (do do!))
      (rename (base) (script do))
      (base-syntax)
      (binder)
      (check)
      (data)
      (identifier)
      (lets)
      (syntax)
      (infix)
      (switch)
      (throw)))

  (define cons scheme-cons)
  (define-binder (cons car cdr))
)
