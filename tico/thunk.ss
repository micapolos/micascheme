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

  (define (thunks-app-datum-opt $thunks)
    (and
      (for-all arity-single? (map thunk-arity $thunks))
      `(app ,@(map thunk-datum $thunks))))

  (define (thunks-values-app-datum $thunks)
    `(values-app ,@(map thunk-values-app-datum $thunks)))

  (define (thunks-app-datum $thunks)
    (or
      (thunks-app-datum-opt $thunks)
      (thunks-values-app-datum $thunks)))

  (define (thunk-application $arity $target-thunk $arg-thunks)
    (thunk $arity
      (thunks-app-datum (cons $target-thunk $arg-thunks))))
)
