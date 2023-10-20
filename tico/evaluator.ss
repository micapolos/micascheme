(library (tico evaluator)
  (export)
  (import (micascheme))

  (data (compile-time value))
  (data (compiled datum free-variable-count))
  (data (combined evaluated-opt compiled))
  (data (typed type value))

  (data (native-type))
  (data (function-type params result))
  (data (struct-type name items))

  (data (binding symbol value))
  (data (evaluator bindings))
  (data (evaluated evaluator value))

  (define (bindings-syntax->evaluated $bindings $syntax)
    (syntax-case $syntax ()
      ($other
        (switch (syntax->datum #`$other)
          ((number? $number)
            ($fn
              (binding )))
)
