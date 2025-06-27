(library (typico typed)
  (export
    typed typed? typed-type typed-value
    typed->datum
    typed-map-value)
  (import (micascheme) (typico type))

  (data (typed type value))

  (define (typed->datum $typed)
    `(typed
      ,(type->datum (typed-type $typed))
      ,(typed-value $typed)))

  (define (typed-map-value $proc $typed)
    (typed
      (typed-type $typed)
      ($proc (typed-value $typed))))
)
