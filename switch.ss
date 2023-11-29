(library (switch)
  (export switch switch-opt switch-exclusive index-switch)
  (import
    (scheme)
    (syntax)
    (throw))

  (define-syntax switch
    (syntax-rules (else)
      ((_ expr ((pred var) body ...) ... ((else else-var) else-body ...))
        (let ((tmp expr))
          (cond
            ((pred tmp)
              (let ((var tmp)) body ...)) ...
            (else
              (let ((else-var tmp)) else-body ...)))))
      ((_ expr ((pred var) body ...) ...)
        (let ((tmp expr))
          (cond
            ((pred tmp)
              (let ((var tmp)) body ...)) ...)))))

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
