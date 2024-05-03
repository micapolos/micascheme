(library (emu internal)
  (export define-internal)
  (import (scheme) (syntax) (lets) (identifier))

  (define-syntax (define-internal stx)
    (syntax-case stx ()
      ((_ id)
        (lets
          (define-id (identifier-append #'id #'define- #'id))
          #`(begin
            (define-rule-syntax (#,define-id key value)
              (define-property key id #'value))
            (define-lookup-syntax (id stx lookup)
              (syntax-case stx ()
                ((_ key)
                  (or
                    (lookup #'key #'id)
                    (syntax-error stx))))))))))
)
