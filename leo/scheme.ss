(library (leo scheme)
  (export library import define lambda)
  (import
    (except (micascheme)
      library import define lambda with)
    (leo transform))
  (export
    (import
      (except (micascheme)
        library import define lambda with)))

  (define-syntax library transform-library)
  (define-syntax import transform-import)
  (define-syntax define transform-define)
  (define-syntax lambda transform-lambda)
)
