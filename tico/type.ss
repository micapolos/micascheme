(library (tico type)
  (export
    anything anything?
    any-boolean any-boolean?
    any-number any-number?
    any-string any-string?
    any-list any-list? any-list-item
    any-function any-function? any-function-input any-function-output
    field field? field-name field-items)
  (import (micascheme))

  (data (anything))
  (data (any-boolean))
  (data (any-number))
  (data (any-string))
  (data (any-list item))
  (data (any-function input output))
  (data (field name items))
  (data (abstraction params body))
  (data (application target args))
  (data (variable index))
)
