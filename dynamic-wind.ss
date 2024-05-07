(library (dynamic-wind)
  (export with-dynamic-wind)
  (import (scheme) (syntax) (syntaxes))

  (define-rules-syntax
    ((with-dynamic-wind body) body)
    ((with-dynamic-wind (id create) body body* ... dispose)
      (let ()
        (define id)
        (dynamic-wind
          (lambda () (set! id create))
          (lambda () (with-dynamic-wind body body* ...))
          (lambda () dispose)))))
)
