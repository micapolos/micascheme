(library (leo define-record)
  (export define-record)
  (import
    (only (chezscheme)
      map
      define-syntax
      syntax-case
      syntax
      quasisyntax
      unsyntax
      unsyntax-splicing
      constructor predicate prefix
      ...)
    (prefix (only (scheme) define-record) %)
    (syntax)
    (syntax-keywords)
    (keywords)
    (syntaxes)
    (system)
    (leo with)
    (leo transform))

  (define-syntax (define-record stx)
    (syntax-case stx (parent required optional options)
      ((define-record (name (required required-fields ...) (optional optional-fields ...)))
        #'(define-record (name (required required-fields ...) (optional optional-fields ...) (options))))

      ((define-record (name (required required-fields ...) (optional optional-fields ...) (options opts ...)))
        #`(%define-record name
          (#,@(map transform-name #'(required-fields ...)))
          (#,@(map transform-name #'(optional-fields ...)))
          (opts ...)))

      ((define-record (name (parent p) required-fields ...))
        #'(define-record (name (parent p) (required required-fields ...) (optional))))

      ((define-record (name (parent p) (required required-fields ...) (optional optional-fields ...)))
        #'(define-record (name (parent p) (required required-fields ...) (optional optional-fields ...) (options))))

      ((define-record (name (parent p) (required required-fields ...) (optional optional-fields ...) (options opts ...)))
        #'(%define-record name parent
          (#,@(map transform-name #'(required-fields ...)))
          (#,@(map transform-name #'(optional-fields ...)))
          (opts ...)))

      ((define-record (name required-fields ...))
        #'(define-record (name (required required-fields ...) (optional))))))
)
