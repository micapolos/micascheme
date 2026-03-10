(library (leo lang)
  (export library import)
  (import
    (except (micascheme) library import)
    (leo transform))
  (export
    (import
      (except (micascheme) library import)))

  (define-syntax library transform-library)
  (define-syntax import transform-import)
)
