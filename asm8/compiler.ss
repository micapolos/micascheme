(library (asm8 compiler)
  (export smart-let smart-values compile-op)
  (import
    (scheme)
    (lets)
    (list)
    (syntax)
    (syntaxes))

  (define-rules-syntax
    ((smart-let expr () body)
      (let () expr body))
    ((smart-let expr (v) body)
      (let ((v expr)) body))
    ((smart-let expr (v vs ...) body)
      (let-values (((v vs ...) expr)) body)))

  (define-case-syntax (smart-values (arity expr) ...)
    (lets
      ($tmpss
        (map
          (lambda ($arity)
            (generate-temporaries (iota $arity)))
          (datum (arity ...))))
      #`(let-values
        (
          #,@(map
            (lambda ($tmps $expr) #`((#,@$tmps) #,$expr))
            $tmpss
            #'(expr ...)))
        (values #,@(apply append $tmpss)))))

  (define (compile-op $gen $stack $arg-count $body-proc $result-count)
    (cond
      ((zero? $arg-count)
        (cons
          (cons $result-count ($body-proc))
          $stack))
      (else
        (lets
          ($entry (car $stack))
          ($stack (cdr $stack))
          ($entry-arity (car $entry))
          ($entry-body (cdr $entry))
          ($arg-count (- $arg-count $entry-arity))
          ($slack-count (- (min $arg-count 0)))
          ($vars (map (lambda (_) ($gen)) (iota $entry-arity)))
          ($slack-vars (map (lambda (_) ($gen)) (iota $slack-count)))
          (compile-op
            $gen
            $stack
            (max $arg-count 0)
            (lambda $args
              (lets
                ($wrap-result (lambda ($result)
                  (cond
                    ((zero? $slack-count) $result)
                    (else
                      (lets
                        ($result-vars (map (lambda (_) ($gen)) (iota $result-count)))
                        `(let-values ((,$result-vars ,$result))
                          (values ,@$slack-vars ,@$result-vars)))))))
                `(smart-let ,$entry-body ,$vars
                  ,($wrap-result (apply $body-proc (append $vars $args))))))
            (+ $slack-count $result-count))))))
)
