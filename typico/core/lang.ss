(library (typico core lang)
  (export
    typico
    check-typico-equal?
    check-typico-raises
    check-typico-works)
  (import
    (typico base)
    (typico typed)
    (typico expander)
    (typico core expanders)
    (typico core environment))
  (export (import (typico base)))

  (define-case-syntax (typico expr)
    #`(eval
      '#,(datum->syntax #'+ (typed-value (expand core-expander #'expr)))
      (typico-environment)))

  (define-rule-syntax (check-typico-equal? in out)
    (check (equal? (typico in) (typico out))))

  (define-case-syntax (check-typico-raises in)
    (or
      (guard
        ($exception (else #'(void)))
        (expand core-expander #'in)
        #f)
      #'(syntax-error #'in "did not raise")))

  (define-case-syntax (check-typico-works in)
    (or
      (guard
        ($exception (else #f))
        (expand core-expander #'in)
        #'(void))
      #'(syntax-error #'in "does not work")))
)
