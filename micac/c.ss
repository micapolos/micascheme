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

  (define-rule-syntax (micac-macro (id arg ...) body ...)
    (define-micac-syntax id
      (lambda ($syntax)
        (syntax-case $syntax ()
          ((_ arg ...)
            #'(begin body ...))))))

  (define-syntax (micac-c $syntax $lookup)
    (syntax-case $syntax ()
      ((_ instr ...)
        (datum->syntax #'+
          (apply syntax-c $lookup
            (syntax->list #'(instr ...)))))))
)
