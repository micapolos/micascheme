(library (leo lookup)
  (export safe-lookup?)
  (import (scheme))

  (define (safe-lookup? lookup? . xs)
    (guard
      (condition ((syntax-violation? condition) #f))
      (apply lookup? xs)))
)
