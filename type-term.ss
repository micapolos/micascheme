(library (type-term)
  (export type-term)
  (import (micascheme) (term))

  (define (type-term $type)
    (switch $type
      ((boolean-type? _) (pair 0 #f))
      ((number-type? _) (pair 1 #f))
      ((string-type? _) (pair 2 #f))
      ((tuple-type? $tuple-type)
        (pair 3 
          (cons 
            (tuple-type-name $tuple-type)
            (map type-term (tuple-type-types $tuple-type)))))
      ((choice-type? $choice-type)
        (pair 4 
          (map type-term (choice-type-types $choice-type))))
      ((function-type? $function-type)
        (pair 5
          (vector
            (function-type-name $function-type)
            (map type-term (function-type-params $function-type))
            (type-term (function-type-result $function-type)))))
      ((else $other) (throw type-term $other))))
)