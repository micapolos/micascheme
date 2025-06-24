(library (typico type)
  (export
    primitive-type primitive-type?
    function-type function-type? function-type-param-types function-type-result-type
    type->datum)
  (import (micascheme))

  (data (primitive-type gensym datum))

  (data (function-type param-types result-type))

  (define (type->datum $type)
    (switch-exhaustive $type
      ((primitive-type? $primitive-type)
        (primitive-type-datum $primitive-type))
      ((function-type? $function-type)
        `(function
          (
          ,@(map*
            type->datum
            (lambda ($type) `(,(type->datum $type) ...))
            (function-type-param-types $function-type)))
          ,(type->datum (function-type-result-type $function-type))))))
)
