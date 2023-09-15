(library (micascheme)
  (export)

  (import 
    (except (scheme) do)
    (rename (base) (script do))
    (base-syntax)
    (infix))

  (export
    (import 
      (rename (scheme) (do do!))
      (rename (base) (script do))
      (base-syntax)
      (infix))))
