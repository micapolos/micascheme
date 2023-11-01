(library (tico datum)
  (export value->datum)
  (import (micascheme))

  (define (value->datum $value)
    (switch $value
      ((null? $null) $null)
      ((boolean? $boolean) $boolean)
      ((number? $number) $number)
      ((string? $string) $string)
      ((char? $char) $char)
      ((pair? $pair)
        `(cons
          ,(value->datum (car $pair))
          ,(value->datum (cdr $pair))))
      ((vector? $vector)
        `(vector ,@(map value->datum (vector->list $vector))))
      ((else $other)
        (throw value->datum $value))))

)
