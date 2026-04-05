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
      ...)
    (prefix (only (scheme) define-record) %)
    (syntax)
    (syntax-keywords)
    (syntaxes)
    (system)
    (leo with)
    (leo transform))

  (define-syntax (define-record stx)
    (syntax-case stx (with)
      ((define-record (name (with required-fields ...)))
        #'(define-record (name (with required-fields ...) (with))))

      ((define-record (name (with required-fields ...) (with optional-fields ...)))
        #'(define-record (name (with required-fields ...) (with optional-fields ...) (with))))

      ((define-record (name (with required-fields ...) (with optional-fields ...) (with options ...)))
        #`(%define-record name
          (#,@(map transform-name #'(required-fields ...)))
          (#,@(map transform-name #'(optional-fields ...)))
          (#,@(map transform-name #'(options ...)))))

      ((define-record (name (parent (with required-fields ...))))
        #'(define-record (name (parent (with required-fields ...) (with)))))

      ((define-record (name (parent (with required-fields ...) (with optional-fields ...))))
        #'(define-record (name (parent (with required-fields ...) (with optional-fields ...) (with)))))

      ((define-record (name (parent (with required-fields ...) (with optional-fields ...) (with options ...))))
        #'(%define-record name parent
          (#,@(map transform-name #'(required-fields ...)))
          (#,@(map transform-name #'(optional-fields ...)))
          (#,@(map transform-name #'(options ...)))))))
)
