(library (sjasm mapping)
  (export
    define-sjasm-mapping
    define-sjasm-mappings)
  (import (micascheme) (sjasm expr))

  (define-syntax (define-sjasm-mapping $syntax $lookup)
    (syntax-case $syntax ()
      ((_ id fallback-id)
        (for-all identifier? (syntaxes id fallback-id))
        #`(define (id . $args)
          (if (for-all number? $args)
            (apply fallback-id $args)
            `(id ,@$args))))))

  (define-rule-syntax (define-sjasm-mappings (from to) ...)
    (begin (define-sjasm-mapping from to) ...))
)
