(library (dynamic-wind)
  (export with-dynamic-wind)
  (import (scheme) (syntax) (syntaxes))

  (define-rules-syntax
    ((with-dynamic-wind (id create) body ... dispose)
      (let ()
        (define id)
        (dynamic-wind
          (lambda () (set! id create))
          (lambda () body ...)
          (lambda () dispose)))))
)
