(library (tico thunk)
  (export
    thunk thunk? thunk-arity thunk-datum
    thunk-values-app-datum
    thunks-app-datum-opt
    thunks-values-app-datum
    thunks-app-datum
    thunk-application)
  (import
    (micascheme)
    (tico arity))

  (data (thunk arity datum))

  (define (thunk-values-app-datum $thunk)
    `(
      ,(arity-value (thunk-arity $thunk))
      ,(thunk-datum $thunk)))

  (define (thunks-app-datum-opt $target-datum $thunks)
    (and
      (for-all arity-single? (map thunk-arity $thunks))
      `(app
        ,$target-datum
        ,@(map thunk-datum $thunks))))

  (define (thunks-values-app-datum $target-datum $thunks)
    `(values-app
      ,$target-datum
      ,@(map thunk-values-app-datum $thunks)))

  (define (thunks-app-datum $target-datum $thunks)
    (or
      (thunks-app-datum-opt $target-datum $thunks)
      (thunks-values-app-datum $target-datum $thunks)))

  (define (thunk-application $arity $target-thunk $arg-thunks)
    (thunk $arity
      (thunks-app-datum $target-thunk $arg-thunks)))
)
