(library (pair)
  (export
    unpair
    pair-values
    null-or-pair?
    pair)
  (import
    (scheme)
    (binder))

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

  (define (null-or-pair? $obj)
    (or (null? $obj) (pair? $obj)))

  (define pair cons)

  (define-binder pair
    (lambda ($pair $fn)
      ($fn (car $pair) (cdr $pair))))
)
