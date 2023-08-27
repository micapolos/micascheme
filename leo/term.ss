(library (term)
  (export)
  (import (micascheme))

  (data (function args body))
  (data (struct name fields))
  (data (enum name options))

  (data (any-string))
  (data (any-number))
  (data (any-thing))

  (data (binding identifier type))
  (data (term syntax type))
  (data (argument identifier term))
  (data (compiler bindings arguments))

  (define (binding-term $binding)
    (term 
      (binding-identifier $binding)
      (binding-type $binding)))

  (define (struct-term $name $terms)
    (term
      (tuple-syntax (map term-syntax $terms)) ; filter static
      (struct $name (map term-type $terms))))

  (define (bindings-syntaxes->arguments $bindings $syntaxes)
    (compiler-arguments
      (fold-left 
        compiler+syntax 
        (compiler $bindings (list)) 
        $syntaxes)))

  (define (arguments-bind $arguments $fn)
    (term )

  (define (bindings-syntax->term $bindings $syntax)
    (syntax-case $syntax ()
      (($name $syntax ...)
        (bindings-bind-syntaxes
          (compiler-bindings $compiler)
          (syntax->list #`($syntax ...))
          (lambda ($bindings)
            (struct-term
              (syntax->datum $name)
              (map binding-term $bindings)))))
      ($other
        (switch (syntax->datum #`$other)
          ((number? $number) (term $number (any-number)))
          ((string? $string) (term $string (any-string)))
          ((else _) (syntax-error $syntax))))))

  (define (compiler+term $compiler $term)
    (lets
      ($type (term-type $term))
      ($syntax (term-syntax $term))
      ($identifier (type-generate-temporary $type))
      (compiler
        (push (compiler-bindings $compiler) (binding $identifier $type))
        (push (compiler-arguments $term) (argument $identifier $term)))))

  (define (compiler+syntax $compiler $syntax)
    (syntax-case $syntax ()
      (($name $syntax ...)
        (bindings-bind-syntaxes
          (compiler-bindings $compiler)
          (syntax->list #`($syntax ...))
          (lambda ($bindings)
            (struct-term
              (syntax->datum $name)
              (map binding-term $bindings)))))
      ($other
        (bindings-syntax->term
          (compiler-bindings)
        (switch (syntax->datum #`$other)
          ((number? $number) (term $number (any-number)))
          ((string? $string) (term $string (any-string)))
          ((else _) (syntax-error $syntax))))))
)