(library (minic type)
  (export
    type-type? type-type
    int-type? int-type int-type-bits
    function-type? function-type function-type-param-types function-type-result-type
    type->datum
    type->syntax)
  (import (micascheme))

  (data (type-type))
  (data (int-type bits))
  (data (function-type param-types result-type))

  (define (type->datum $type)
    (switch $type
      ((type-type? _) 'type)
      ((int-type? (int-type $bits))
        (string->symbol (string-append "u" (number->string $bits))))
      ((function-type? (function-type $param-types $result-type))
        `(-> (,@(map type->datum $param-types)) ,(type->datum $result-type)))))

  (define (type->syntax $type)
    (switch $type
      ((type-type? _)
        #`(type-type))
      ((int-type? (int-type $bits))
        #`(int-type #,$bits))
      ((function-type? (function-type $param-types $result-type))
        #`(function-type
          (list #,@(map type->syntax $param-types))
          #,(type->syntax $result-type)))))
)
