(library (emu internal)
  (export define-constant define-internal)
  (import (scheme) (syntax) (lets) (identifier))

  (define-syntax (define-constant stx)
    (syntax-case stx ()
      ((_ id)
        (lets
          (define-id (identifier-append #'id #'define- #'id))
          #`(begin
            (define-rule-syntax (#,define-id key expr)
              (define-property key id #'expr))
            (define-lookup-syntax (id stx lookup)
              (syntax-case stx ()
                ((_ key)
                  (or
                    (lookup #'key #'id)
                    (syntax-error stx))))))))))

  (define-syntax (define-internal stx)
    (syntax-case stx ()
      ((_ id)
        (lets
          (define-id (identifier-append #'id #'define- #'id))
          #`(begin
            (define-rule-syntax (#,define-id key expr)
              (begin
                (define $internal expr)
                (define-property key id #'$internal)))
            (define-lookup-syntax (id stx lookup)
              (syntax-case stx ()
                ((_ key)
                  (or
                    (lookup #'key #'id)
                    (syntax-error stx))))))))))
)
