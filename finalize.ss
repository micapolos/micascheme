(library (finalize)
  (export
    finalize
    define-finalize)
  (import
    (scheme)
    (syntax))

  (define-keyword finalize)

  (define-rule-syntax (define-finalize (id var) body)
    (define-property id finalize
      (lambda ($syntax)
        (syntax-case $syntax ()
          (x #'(let ((var x)) body))))))
)
