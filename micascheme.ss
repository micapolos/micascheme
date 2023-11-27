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
    (throw)
    (number)
    (stack)
    (iterate)
    (generate))

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
      (throw)
      (number)
      (stack)
      (iterate)
      (generate)))

  (define cons scheme-cons)
  (define-binder (cons car cdr))
)
