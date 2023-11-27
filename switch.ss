(library (switch)
  (export switch switch-opt switch-exclusive index-switch)
  (import
    (scheme)
    (define-syntax)
    (throw))

  (define-syntax switch
    (lambda (stx)
      (syntax-case stx (else)
        ((_ expr ((pred var) body ...) ... ((else else-var) else-body ...))
          (let ((tmp (car (generate-temporaries `(tmp)))))
            #`(let ((#,tmp expr))
              (cond
                ((pred #,tmp)
                  (let ((var #,tmp)) body ...)) ...
                (else
                  (let ((else-var #,tmp)) else-body ...))))))
        ((_ expr ((pred var) body ...) ...)
          (let ((tmp (car (generate-temporaries `(tmp)))))
            #`(let ((#,tmp expr))
              (cond
                ((pred #,tmp)
                  (let ((var #,tmp)) body ...)) ...)))))))

  (define-syntax-rule (switch-opt $expr (($pred $var) $body ...) ...)
    (switch $expr
      (($pred $var) $body ...) ...
      ((else _) #f)))

  (define-syntax-rule (switch-exclusive $expr (($pred $var) $body ...) ...)
    (switch $expr
      (($pred $var) $body ...) ...
      ((else _)
        (throw non-exclusive
          (quote (switch $expr $pred ...))))))

  (define-syntax index-switch
    (lambda (stx)
      (syntax-case stx ()
        ((_ expr branch ... default)
          #`(case expr
            #,@(map
              (lambda ($index $branch) #`((#,$index) #,$branch))
              (iota (length (syntax->list #`(branch ...))))
              (syntax->list #`(branch ...)))
            (else default))))))
)
