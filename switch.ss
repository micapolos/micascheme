(library (switch)
  (export switch switch? switch-exhaustive index-switch)
  (import
    (scheme)
    (syntax)
    (binder)
    (list-syntax)
    (procedure)
    (throw))

  (define-lookup-syntax (switch $syntax $lookup)
    (syntax-case $syntax (else)
      ((_ expr ((pred var) body) ... ((else else-var) else-body))
        #`(let ((tmp expr))
          (cond
            #,@(map-with
              ($pred (syntax->list #'(pred ...)))
              ($decl (syntax->list #'(var ...)))
              ($body (syntax->list #'(body ...)))
              #`((#,$pred tmp) #,(transform-binder $lookup $decl #'tmp $body)))
            (else
              (let ((else-tmp tmp))
                #,(transform-binder $lookup #'else-var #'else-tmp #'else-body))))))
      ((_ expr ((pred var) body) ...)
        #`(let ((tmp expr))
          (cond
            #,@(map-with
              ($pred (syntax->list #'(pred ...)))
              ($decl (syntax->list #'(var ...)))
              ($body (syntax->list #'(body ...)))
              #`((#,$pred tmp) #,(transform-binder $lookup $decl #'tmp $body))))))))

  (define-rule-syntax (switch? $expr (($pred $var) $body) ...)
    (switch $expr
      (($pred $var) $body) ...
      ((else _) #f)))

  (define-rule-syntax (switch-exhaustive $expr (($pred $var) $body) ...)
    (switch $expr
      (($pred $var) $body) ...
      ((else _)
        (throw non-exhaustive
          (quasiquote (switch (unquote $expr) $pred ...))))))

  (define-case-syntax (index-switch expr branch ... default)
    (let
      (($branches (syntax->list #`(branch ...))))
      #`(case expr
        #,@(map
          (lambda ($index $branch) #`((#,$index) #,$branch))
          (enumerate $branches)
          $branches)
        (else default))))
)
