(library (micascheme)
  (export)

  (import 
    (except (chezscheme) do)
    (rename (base) (script do))
    (base-syntax)
    (infix))

  (export
    (import 
      (except (chezscheme) do)
      (rename (base) (script do))
      (base-syntax)
      (infix))))
