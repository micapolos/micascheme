(library (tico thunk)
  (export
    thunk thunk? thunk-arity thunk-datum
    thunk-values-app-datum)
  (import
    (micascheme)
    (tico arity))

  (data (thunk arity datum))

  (define (thunk-values-app-datum $thunk)
    `(
      ,(arity-value (thunk-arity $thunk))
      ,(thunk-datum $thunk)))
)
