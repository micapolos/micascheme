(library (asm8 compiler)
  (export
    entry entry? entry-arity entry-expr
    op op? op-arg-count op-result-count op-expr-proc
    smart-bind smart-values compile-op)
  (import
    (scheme)
    (data)
    (lets)
    (list)
    (stack)
    (syntax)
    (syntaxes)
    (system))

  (data (entry arity expr))
  (data (op arg-count result-count expr-proc))

  (define-rules-syntax
    ((smart-bind expr () body)
      (let () expr body))
    ((smart-bind expr (v) body)
      (let ((v expr)) body))
    ((smart-bind expr (v vs ...) body)
      (let-values (((v vs ...) expr)) body)))

  (define-syntax (smart-values $syntax)
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

  (define (compile-op $gen $stack $op)
    ;(pretty-print `(compile-op ,$stack ,$op))
    (lets
      ((op $arg-count $result-count $body-proc) $op)
      (cond
        ((zero? $arg-count)
          (push $stack (entry $result-count ($body-proc))))
        (else
          (lets
            ((entry $entry-arity $entry-expr) (car $stack))
            ($stack (cdr $stack))
            ($arg-count (- $arg-count $entry-arity))
            ($slack-count (- (min $arg-count 0)))
            ($entry-vars (map (lambda (_) ($gen)) (iota $entry-arity)))
            (compile-op
              $gen
              $stack
              (op
                (max $arg-count 0)
                (+ $slack-count $result-count)
                (lambda $args
                  `(smart-bind ,$entry-expr ,$entry-vars
                    (smart-values
                      ,@(map
                        (lambda ($var) `(1 ,$var))
                        (list-take $entry-vars $slack-count))
                      (
                        ,$result-count
                        ,(apply $body-proc (append $entry-vars $args)))))))))))))
)
