(library (minic type-syntax)
  (export type->syntax)
  (import (micascheme) (minic type))

  (define (type->syntax $type)
    (switch $type
      ((type-type? _)
        #`(type-type))
      ((syntax-type? _)
        #`(syntax-type))
      ((int-type? (int-type $bits))
        #`(int-type #,$bits))
      ((function-type? (function-type $param-types $result-type))
        #`(function-type
          (list #,@(map type->syntax $param-types))
          #,(type->syntax $result-type)))
      ((ref-type? (ref-type $type))
        #`(ref-type #,(type->syntax $type)))))
)