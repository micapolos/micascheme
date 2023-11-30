(library (proof)
  (export
    lambda-proof lambda-proof? lambda-proof-params lambda-proof-result
    values-proof values-proof? values-proof-items
    lambda? values?

    syntax->proof)
  (import (micascheme))

  (data (lambda-proof params result))
  (data (values-proof items))
  (data (pair-of? car cdr))

  (define-syntax-rule (lambda? ($param ...) $result)
    (lambda-proof (list $param ...) $result))

  (define-syntax-rule (values? $item ...)
    (values-proof (list $item ...)))

  (define syntax->proof
    (case-lambda
      (($syntax)
        (syntax->proof (lambda $ #f) $syntax))
      (($lookup $syntax)
        (syntax-case $syntax ()
          (($target $arg ...)
            (lets
              ($arg-proofs
                (map
                  (partial syntax->proof $lookup)
                  (syntax->list #'($arg ...))))
              (switch (syntax->proof $lookup #'$target)
                ((lambda-proof? $lambda-proof)
                  (lets
                    ($param-proofs (lambda-proof-params $lambda-proof))
                    ($result-proof (lambda-proof-result $lambda-proof))
                    ($params-arity (length $param-proofs))
                    ($args-arity (length $arg-proofs))
                    (cond
                      ((not (= $params-arity $args-arity))
                        (syntax-error $syntax
                          (format
                            "incorrect argument count (expected ~s) in call"
                            $params-arity)))
                      ((for-all equal? $param-proofs $arg-proofs)
                        $result-proof)
                      (else
                        (syntax-error $syntax "invalid arg types")))))
                ((else _)
                  (syntax-error #'$target "not procedure")))))
          ($other
            (switch (syntax->datum #'$other)
              ((boolean? _) boolean?)
              ((string? _) string?)
              ((number? _) number?)
              ((symbol? _)
                (switch ($lookup #'$other)
                  ((not-false? $proof) $proof)
                  ((else _) (syntax-error #'$other "unproven"))))
              ((else _)
                (syntax-error #'$other "unproven"))))))))
)
