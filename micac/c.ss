(library (micac c)
  (export
    micac-c
    define-micac-syntax
    micac-externs
    micac-macro)
  (import (micascheme) (micac syntax-c) (micac syntax) (micac env) (micac variable) (syntax))
  (export (import (micac syntax)))

  (define-rule-syntax (micac-externs id ...)
    (begin
      (define-micac-syntax id (variable #'id)) ...))

  (define-rule-syntax (define-micac-syntax id transformer)
    (define-syntax id (make-compile-time-value transformer)))

  (define-rules-syntax (literals literals)
    ((micac-macro (id arg ...) (literals literal ...) body)
      (define-micac-syntax id
        (lambda ($syntax)
          (syntax-case $syntax (literal ...)
            ((_ arg ...) #'body)))))
    ((micac-macro (id arg ...) (literals literal ...) body ...)
      (define-micac-syntax id
        (lambda ($syntax)
          (syntax-case $syntax (literal ...)
            ((_ arg ...)
              #'(begin body ...))))))
    ((micac-macro (id arg ...) body ...)
      (micac-macro (id arg ...) (literals) body ...)))

  (define-syntax (micac-c $syntax $lookup)
    (syntax-case $syntax ()
      ((_ instr ...)
        (datum->syntax #'+
          (apply syntax-c (lookup-env $lookup)
            (syntax->list #'(instr ...)))))))
)
