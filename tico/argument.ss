; Rename to binding, when we get rid of binding
(library (tico argument)
  (export
    argument
    argument?
    argument-key
    argument-value)
  (import
    (micascheme))

  (data (argument key value))
)
