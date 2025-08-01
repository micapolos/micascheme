(library (asm environmental)
  (export
    environmental environmental? environmental-environment environmental-ref
    environmental-with-environment environmental-with-ref
    pure-environmental
    environmental-map
    environmental-update-environment
    environmental-append
    list->environmental
    environmental-append-map
    environmental->datum)
  (import (asm base) (asm environment))

  (define-monoidical (environmental environment))

  (define (environmental-append-map $proc . $list)
    (environmental-map $proc
      (apply environmental-append $list)))

  (define (environmental->datum $environmental)
    `(environmental
      ,@(environment->entry-datums (environmental-environment $environmental))
      ,(environmental-ref $environmental)))
)
