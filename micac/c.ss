(library (micac c)
  (export
    micac-c
    define-micac-syntax
    micac-externs
    micac-macro
    define-micac-property)
  (import (micascheme) (micac syntax-c) (micac syntax) (syntax))
  (export (import (micac syntax)))

  (define-rule-syntax (define-micac-property id transformer)
    (define-property id micac transformer))

  (define-rule-syntax (micac-externs id ...)
    (define-aux-keywords id ...))

  (define-rule-syntax (define-micac-syntax id transformer)
    (begin
      (define-aux-keyword id)
      (define-micac-property id transformer)))

  (define-rules-syntax (literals literals)
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
          (apply syntax-c $lookup
            (syntax->list #'(instr ...)))))))
)
