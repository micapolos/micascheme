(library (emu internal)
  (export define-internal)
  (import (scheme) (syntax) (lets) (identifier))

  (define-case-syntax (define-internal id)
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
                (syntax-error stx))))))))
)
