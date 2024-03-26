(library (leo2)
  (export
    typed typed? typed-value typed-type

    variable variable? variable-index
    arrow! arrow arrow? arrow-params arrow-results
    struct! struct struct? struct-name struct-values
    enum! enum enum? enum-name enum-values
    string! any-string any-string?
    number! any-number any-number?
    anything! anything anything?

    type-static? type-dynamic? types-dynamic?

    indexing indexing? indexing-size indexing-index-options
    empty-indexing indexing-reverse indexing-ref
    indexing+type types-indexing types-indexed)
  
  (import (except (micascheme) enum) (leo-syntax))

  (data (typed value type))

  (data (variable index))
  (data (arrow params results))
  (data (struct name values))
  (data (enum name values))

  (data (any-string))
  (data (any-number))
  (data (anything))

  (define-rule-syntax (struct! $name $value ...)
    (struct (quote $name) (list $value ...)))

  (define-rule-syntax (enum! $name $value ...)
    (enum (quote $name) (list $value ...)))

  (define-rule-syntax (arrow! ($param ...) $result ...)
    (arrow (list $param ...) (list $result ...)))

  (define anything! (anything))
  (define string! (any-string))
  (define number! (any-number))

  ; --- type-static? type-dynamic?

  (define (types-dynamic? $types)
    (exists type-dynamic? $types))

  (define (type-static? $type)
    (not (type-dynamic? $type)))

  (define (type-dynamic? $type)
    (switch $type
      ((variable? _) #t)
      ((arrow? $arrow) (arrow-dynamic? $arrow))
      ((struct? $struct) (struct-dynamic? $struct))
      ((enum? $enum) (enum-dynamic? $enum))
      ((any-string? _) #t)
      ((any-number? _) #t)
      ((anything? _) #t)
      ((else $other) (throw type-dynamic? $other))))

  (define (arrow-dynamic? $arrow)
    (types-dynamic? (arrow-results $arrow)))

  (define (struct-dynamic? $struct)
    (types-dynamic? (struct-values $struct)))

  (define (enum-dynamic? $enum)
    (lets 
      ($values (enum-values $enum))
      (case (length $values)
        ((1) (type-dynamic? (car $values)))
        (else #t))))

  (define (typed-dynamic? $typed) 
    (type-dynamic? (typed-type $typed)))

  (define (typed-static? $typed) 
    (type-static? (typed-type $typed)))

  ; --- indexing

  (data (indexing size index-options))

  (define empty-indexing (indexing 0 (list)))

  (define (indexing-reverse $indexing)
    (indexing
      (indexing-size $indexing)
      (reverse (indexing-index-options $indexing))))

  (define (indexing+type $indexing $type)
    (lets 
      ($size (indexing-size $indexing))
      ($index-options (indexing-index-options $indexing))
      (if (type-dynamic? $type)
        (indexing (+ $size 1) (cons $size $index-options))
        (indexing $size (cons #f $index-options)))))

  (define (indexing-ref $indexing $index)
    (list-ref (indexing-index-options $indexing) $index))

  (define (types-indexing $types)
    (indexing-reverse (fold-left indexing+type empty-indexing $types)))

  (define (types-indexed $types)
    (map indexed $types (indexing-index-options (types-indexing $types))))
)
