(library (typico fragment type)
  (export type->fragment)
  (import
    (typico base)
    (typico type)
    (typico fragment))

  (define (type->fragment $type)
    (switch $type
      ((primitive-type? $primitive-type)
        (fragment-bind-with
          ($new (fragment (import (typico type)) primitive-type))
          ($gensym (symbol->fragment (primitive-type-gensym $primitive-type)))
          ($datum (symbol->fragment (primitive-type-datum $primitive-type)))
          (pure-fragment `(,$new ,$gensym ,$datum))))
      ((function-type? $function-type)
        (lets
          ($param-types (function-type-param-types $function-type))
          ($list? (list? $param-types))
          (fragment-bind-with
            ($new
              (fragment (import (typico type)) function-type))
            ($list
              (if $list?
                (fragment (import (scheme)) list)
                (fragment (import (scheme)) list*)))
            ($params
              (if $list?
                (list->fragment (map type->fragment $param-types))
                (list->fragment (map type->fragment (list*->list $param-types)))))
            ($result
              (type->fragment (function-type-result-type $function-type)))
            (pure-fragment
              `(,$new
                (,$list ,@$params)
                ,$result)))))
      ((else $other)
        (throw dupa))))
)
