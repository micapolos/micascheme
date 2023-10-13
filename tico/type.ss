(library (tico type)
  (export
    anything anything?
    any-boolean any-boolean?
    any-number any-number?
    any-string any-string?
    any-list any-list? any-list-item
    any-function any-function? any-function-params any-function-result
    struct struct? struct-name struct-items)
  (import (micascheme))

  (data (anything))
  (data (any-boolean))
  (data (any-number))
  (data (any-string))
  (data (any-list item))
  (data (any-function params result))
  (data (struct name items))
)
