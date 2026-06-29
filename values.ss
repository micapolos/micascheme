(library (values)
  (export with-values values-append)

  (import
    (scheme)
    (lets)
    (syntax)
    (syntaxes))

  (define-rules-syntax
    ((with-values expr () x xs ...)
      (let () expr x xs ...))
    ((with-values expr (v) x xs ...)
      (let ((v expr)) x xs ...))
    ((with-values expr (v vs ...) x xs ...)
      (let-values (((v vs ...) expr)) x xs ...)))

  (define-syntax (values-append $syntax)
    (syntax-case $syntax ()
      ((_ (arity expr) ...)
        (lets
          ($arities (datum (arity ...)))
          ($exprs #'(expr ...))
          (case (length $exprs)
            ((0) #'(void))
            ((1) (car $exprs))
            (else
              (cond
                ((for-all (lambda ($arity) (= $arity 1)) $arities)
                  #`(values #,@$exprs))
                (else
                  (lets
                    ($tmpss
                      (map
                        (lambda ($arity) (generate-temporaries (iota $arity)))
                        $arities))
                    #`(let-values
                      #,(map
                        (lambda ($tmps $expr)
                          #`(#,$tmps #,$expr))
                        $tmpss
                        $exprs)
                      (values #,@(apply append $tmpss))))))))))))
)
