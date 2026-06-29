(library (asm8 compiler)
  (export
    entry entry? entry-arity entry-expr
    op op? op-arg-count op-result-count op-expr-proc
    compile-op
    compile-ops)
  (import
    (scheme)
    (data)
    (lets)
    (list)
    (stack)
    (procedure)
    (syntax)
    (syntaxes)
    (values))

  (data (entry arity expr))
  (data (op arg-count result-count expr-proc))

  (define (compile-op $gen $stack $op)
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
            ($entry-vars (ordered-map (lambda (_) ($gen)) (iota $entry-arity)))
            ((values $slack-vars $vars) (list-split $entry-vars $slack-count))
            (compile-op
              $gen
              $stack
              (op
                (max $arg-count 0)
                (+ $slack-count $result-count)
                (lambda $args
                  `(with-values ,$entry-expr ,$entry-vars
                    (values-append
                      ,@(map
                        (lambda ($slack-var) `(1 ,$slack-var))
                        $slack-vars)
                      (
                        ,$result-count
                        ,(apply $body-proc (append $vars $args)))))))))))))

  (define (compile-ops $gen $stack $ops)
    (fold-left
      (partial compile-op $gen)
      $stack
      $ops))
)
