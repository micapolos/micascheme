(library (tico evaluator)
  (export)
  (import (micascheme))

  (data (typed value type))
  (data (compiled bindings args))

  (define (context+syntax))

  (define (bindings-syntax-list-bind-args $bindings $syntax-list $fn)
    (switch $syntax-list
      ((null? _)
        ($fn $bindings (stack)))
      ((pair? $pair)
        (bindings-syntax-bind-args $bindings (car $syntax-list)
          (lambda ($bindings $args)
            (bindings-syntax-list-bind-args $bindings (cdr $syntax-list)
              (lambda ($bindings $args)
                ($fn $bindings $args))))))))

  (define (bindings-syntax-bind-args $bindings $syntax $fn)
    (syntax-case $syntax ()
      (($name $item ...) (identifier? #`$name)
        (binding-syntax-list-bind-args $bindings (syntax->list #`($item ...))
          (lambda ($bindings $args)
            (typed
              (map typed-value $args)
              (struct (syntax->symbol #`$name) (map typed-type $args))))))
      ($other ($fn $bindings (stack (syntax->typed $other)))))))

  (define (syntax->typed $syntax)
    (switch (syntax->datum $syntax)
      ((symbol? $symbol)
        (case $symbol
          ((boolean) (typed #f (boolean-type)))
          ((number) (typed #f (number-type)))
          ((string) (typed #f (string-type)))
          (else (typed #f (struct $symbol (list))))))
      ((boolean? $boolean) (typed #f $boolean))
      ((number? $number) (typed #f $number))
      ((string? $string) (typed #f $string))
      ((else $other) (syntax-error $syntax "not literal"))))
)
