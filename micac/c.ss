(library (micac c)
  (export
    micac-c
    micac-define
    micac-externs
    micac-macro)
  (import (micascheme) (micac syntax-c) (micac keywords) (syntax scope) (micac expr) (micac expand) (syntax))
  (export (import (micac keywords)))

  (define-rule-syntax (micac-externs id ...)
    (begin
      (micac-define id #'id) ...))

  (define-rule-syntax (micac-define id item)
    (define-syntax id (make-compile-time-value item)))

  (define-rules-syntax (literals literals)
    ((micac-macro (id arg ...) (literals literal ...) body)
      (micac-define id
        (lambda ($syntax)
          (syntax-case $syntax (literal ...)
            ((_ arg ...) #'body)))))
    ((micac-macro (id arg ...) (literals literal ...) body ...)
      (micac-define id
        (lambda ($syntax)
          (syntax-case $syntax (literal ...)
            ((_ arg ...)
              #'(begin body ...))))))
    ((micac-macro (id arg ...) body ...)
      (micac-macro (id arg ...) (literals) body ...)))

  (define-syntax (micac-c $syntax $lookup)
    (syntax-case $syntax ()
      ((_ instr ...)
        (datum->syntax #'micac-c
          (syntax-c
            #`(
              #,@(expand-instrs $lookup
                #'(instr ...))))))))
)
