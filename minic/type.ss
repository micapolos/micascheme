(library (minic type)
  (export
    type-type? type-type
    syntax-type? syntax-type
    int-type? int-type int-type-bits
    function-type? function-type function-type-param-types function-type-result-type
    type->datum)
  (import (micascheme))

  (data (type-type))
  (data (syntax-type))
  (data (int-type bits))
  (data (function-type param-types result-type))

  (define (type->datum $type)
    (switch $type
      ((type-type? _) 'type)
      ((syntax-type? _) 'syntax)
      ((int-type? (int-type $bits))
        (string->symbol (string-append "u" (number->string $bits))))
      ((function-type? (function-type $param-types $result-type))
        `(-> (,@(map type->datum $param-types)) ,(type->datum $result-type)))))
)
