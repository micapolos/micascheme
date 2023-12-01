; Rename to binding, when we get rid of binding
(library (tico bimbing)
  (export
    bimbing
    bimbing?
    bimbing-key
    bimbing-value)
  (import
    (micascheme))

  (data (bimbing key value))
)
