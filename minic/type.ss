(library (minic type)
  (export
    int-type? int-type int-type-bits
    function-type? function-type function-type-param-types function-type-result-type
    type->datum)
  (import (micascheme))

  (data (int-type bits))
  (data (function-type param-types result-type))

  (define (type->datum $type)
    (switch $type
      ((int-type? (int-type $bits)) (string->symbol (string-append "u" (number->string $bits))))
      ((function-type? (function-type $param-types $result-type)) 'fun)))
)
