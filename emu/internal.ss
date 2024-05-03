(library (emu internal)
  (export define-internal)
  (import (scheme) (syntax) (syntaxes) (lets))

  (define-rule-syntax (define-internal id)
    (define-lookup-syntax (id stx lookup)
      (syntax-case stx ()
        ((_ key value)
          #`(define-property key id #'value))
        ((_ key)
          (or
            (lookup #'key #'id)
            (syntax-error stx))))))
)
