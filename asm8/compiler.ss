(library (asm8 compiler)
  (export
    entry entry? entry-arity entry-expr
    smart-let smart-values compile-op)
  (import
    (scheme)
    (data)
    (lets)
    (list)
    (stack)
    (syntax)
    (syntaxes))

  (data (entry arity expr))

  (define-rules-syntax
    ((smart-let expr () body)
      (let () expr body))
    ((smart-let expr (v) body)
      (let ((v expr)) body))
    ((smart-let expr (v vs ...) body)
      (let-values (((v vs ...) expr)) body)))

  (define-syntax (smart-values $syntax)
    (syntax-case $syntax ()
      ((_ x ... arity expr)
        (lets
          ($xs #'(x ...))
          ($x-arity (length $xs))
          ($arity (datum arity))
          (case $arity
            ((0)
              (case $x-arity
                ((0) #'(void))
                ((1) (car $xs))
                (else #'(values x ...))))
            ((1)
              (case $x-arity
                ((0) #'expr)
                (else #'(values x ... expr))))
            (else
              (lets
                ($tmps (generate-temporaries (iota $arity)))
                #`(smart-let expr #,$tmps
                  (values x ... #,@$tmps)))))))))

  (define (compile-op $gen $stack $arg-count $body-proc $result-count)
    (cond
      ((zero? $arg-count)
        (push $stack (entry $result-count ($body-proc))))
      (else
        (lets
          ((entry $entry-arity $entry-expr) (car $stack))
          ($stack (cdr $stack))
          ($arg-count (- $arg-count $entry-arity))
          ($slack-count (- (min $arg-count 0)))
          ($vars (map (lambda (_) ($gen)) (iota $entry-arity)))
          (compile-op
            $gen
            $stack
            (max $arg-count 0)
            (lambda $args
              `(smart-let ,$entry-expr ,$vars
                (smart-values
                  ,@(list-take $vars $slack-count)
                  ,$result-count
                  ,(apply $body-proc (append $vars $args)))))
            (+ $slack-count $result-count))))))
)
