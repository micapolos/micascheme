(library (pair)
  (export
    unpair
    pair-values)
  (import (scheme))

  (define-syntax unpair
    (lambda (stx)
      (syntax-case stx ()
        ((_ expr lhs rhs body ...)
          (let ((tmp (car (generate-temporaries `(tmp)))))
            #`(let ((#,tmp expr))
              (let ((lhs (car #,tmp))
                    (rhs (cdr #,tmp)))
                body ...)))))))

  (define (pair-values $pair)
    (values (car $pair) (cdr $pair)))
)
