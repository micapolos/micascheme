(library (leo compiler)
  (export
    binding binding? binding-identifier binding-type
    term term? term-syntax term-types

    term-bind
    compile-term)
  (import (micascheme) (leo2))

  (data (term syntax types))
  (data (binding identifier type))
  (data (compiler bindings terms))

  (define (type-name $type)
    (switch $type
      ((any-number? _) #`number)
      ((any-string? _) #`string)
      ((struct? $struct) (datum->syntax #`+ (struct-name $struct)))
      ((enum? $enum) (datum->syntax #`+ (enum-name $enum)))
      ((arrow? $arrow) #`arrow)))

  (define (type-generate-temporary $type)
    (generate-temporary (type-name $type)))

  (define (binding->term $binding)
    (term
      (binding-identifier $binding)
      (list (binding-type $binding))))

  (define (term-bind $term $fn)
    (lets
      ($syntax (term-syntax $term))
      ($types (term-types $term))
      ($identifiers (map type-generate-temporary $types))
      ($bindings (reverse (map binding $identifiers $types)))
      ($fn-term ($fn $bindings))
      (term
        #`(let-values (((#,@$identifiers) #,$syntax)) #,(term-syntax $fn-term))
        (term-types $fn-term))))

  (define (compile-term $compiler $syntax)
    (syntax-case $syntax (variable struct)
      ((variable $index) 
        (binding->term 
          (list-ref
            (compiler-bindings $compiler)
            (syntax->datum #`$index))))
      ((struct $name $value ...)
        (throw compile-struct))
      ($other 
        (switch (syntax->datum $syntax)
          ((number? $number) (term $number (list (any-number))))
          ((string? $string) (term $string (list (any-string))))
          ((else $other) (syntax-error $syntax))))))
)