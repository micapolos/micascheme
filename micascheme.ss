(library (micascheme)
  (export)

  (import 
    (rename
      (except (scheme) do)
      (cons scheme-cons))
    (rename (base)
      (script do))
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
    (generate)
    (test))

  (export
    cons
    (import 
      (rename
        (except (scheme) cons)
        (do do!))
      (rename (base) (script do))
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
      (generate)
      (test)))

  (define cons scheme-cons)
  (define-binder (cons car cdr))
)
