(library (tico thunk)
  (export
    thunk thunk? thunk-arity thunk-datum)
  (import
    (micascheme)
    (tico arity))

  (data (thunk arity datum))
)
