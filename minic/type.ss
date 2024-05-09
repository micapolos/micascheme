(library (minic type)
  (export
    int-type? int-type int-type-bits
    array-type? array-type array-type-element-type array-type-index-bits
    function-type? function-type function-type-param-types function-type-result-type
    ref-type? ref-type ref-type-type
    type->datum)
  (import (micascheme))

  (data (int-type bits))
  (data (array-type element-type index-bits))
  (data (function-type param-types result-type))
  (data (ref-type type))

  (define (type->datum $type)
    (switch $type
      ((int-type? (int-type $bits))
        `(int ,$bits))
      ((array-type? (array-type $element-type $index-bits))
        `(array ,(type->datum $element-type) ,$index-bits))
      ((function-type? (function-type $param-types $result-type))
        `(-> (,@(map type->datum $param-types)) ,(type->datum $result-type)))
      ((ref-type? (ref-type $type))
        `(& ,(type->datum $type)))))
)
