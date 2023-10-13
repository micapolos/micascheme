(library (tico type)
  (export
    anything anything?
    any-boolean any-boolean?
    any-number any-number?
    any-string any-string?
    any-list any-list? any-list-item
    any-function any-function? any-function-params any-function-result
    struct struct? struct-name struct-items
    static static? static-value

    type-static? type-dynamic?)
  (import (micascheme))

  (data (anything))
  (data (any-boolean))
  (data (any-number))
  (data (any-string))
  (data (any-list item))
  (data (any-function params result))
  (data (struct name items))
  (data (static value))

  (define (type-static? $type)
    (switch $type
      ((static? $static) #t)
      ((anything? _) #f)
      ((any-boolean? _) #f)
      ((any-number? _) #f)
      ((any-string? _) #f)
      ((any-list? _) #f)
      ((any-function? $any-function)
        (type-static? (any-function-result $any-function)))
      ((struct? $struct)
        (for-all type-static? (struct-items $struct)))
      ((else $other) (throw not-type $other))))

  (define (type-dynamic? $type)
    (not (type-static? $type)))
)
