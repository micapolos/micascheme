(library (typico typed)
  (export
    typed typed? typed-type typed-value
    typed->datum)
  (import (micascheme) (typico type))

  (data (typed type value))

  (define (typed->datum $typed)
    `(typed
      ,(type->datum (typed-type $typed))
      ,(typed-value $typed)))

)
