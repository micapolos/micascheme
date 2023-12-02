(library (switch)
  (export switch switch-opt switch-exclusive index-switch)
  (import
    (scheme)
    (syntax)
    (binder)
    (procedure)
    (throw))

  (define-syntax switch
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax (else)
          ((_ expr ((pred var) body) ... ((else else-var) else-body))
            #`(let ((tmp expr))
              (cond
                #,@(map
                  (lambda ($pred $decl $body)
                    #`(
                      (#,$pred tmp)
                      #,(transform-binder $lookup $decl #'tmp $body)))
                  (syntax->list #'(pred ...))
                  (syntax->list #'(var ...))
                  (syntax->list #'(body ...)))
                (else
                  (let ((else-tmp tmp))
                    #,(transform-binder $lookup #'else-var #'else-tmp #'else-body))))))
          ((_ expr ((pred var) body) ...)
            #`(let ((tmp expr))
              (cond
                #,@(map
                  (lambda ($pred $decl $body)
                    #`(
                      (#,$pred tmp)
                      #,(transform-binder $lookup $decl #'tmp $body)))
                  (syntax->list #'(pred ...))
                  (syntax->list #'(var ...))
                  (syntax->list #'(body ...))))))))))

  (define-syntax-rule (switch-opt $expr (($pred $var) $body) ...)
    (switch $expr
      (($pred $var) $body) ...
      ((else _) #f)))

  (define-syntax-rule (switch-exclusive $expr (($pred $var) $body) ...)
    (switch $expr
      (($pred $var) $body) ...
      ((else _)
        (throw non-exclusive
          (quote (switch $expr $pred ...))))))

  (define-syntax-case (index-switch expr branch ... default)
    (let
      (($branches (syntax->list #`(branch ...))))
      #`(case expr
        #,@(map
          (lambda ($index $branch) #`((#,$index) #,$branch))
          (enumerate $branches)
          $branches)
        (else default))))
)
