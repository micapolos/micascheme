(library (top-level)
  (export top-level)
  (import (scheme))

  (define-syntax top-level
    (syntax-rules (import)
      ((_ (import spec ...) body ...)
        (eval '(begin body ...) (environment 'spec ...)))))
)
