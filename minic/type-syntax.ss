(library (minic type-syntax)
  (export type->syntax)
  (import (micascheme) (minic type))

  (define (type->syntax $type)
    (switch $type
      ((int-type? (int-type $bits))
        #`(int-type #,$bits))
      ((array-type? (array-type $element-type $index-bits))
        #`(array-type #,(type->syntax $element-type) #,$index-bits))
      ((function-type? (function-type $param-types $result-type))
        #`(function-type
          (list #,@(map type->syntax $param-types))
          #,(type->syntax $result-type)))
      ((ref-type? (ref-type $type))
        #`(ref-type #,(type->syntax $type)))))
)
