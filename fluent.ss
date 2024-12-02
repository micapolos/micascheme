(library (fluent)
  (export fluent also)
  (import (scheme) (syntax) (procedure))

  (define-aux-keyword also)

  (define-syntax (fluent $syntax)
    (define (fluent+ $fluent $syntax)
      (let
        (
          ($arity (car $fluent))
          ($syntax-proc (cdr $fluent)))
        (syntax-case $syntax (values)
          ((arity expr)
            (and (integer? (datum arity)) (nonnegative? (datum arity)))
            (syntax-case #'expr (let also)
              ((let (var ...) expr)
                (for-all identifier? (syntaxes var ...))
                (let (($vars (syntaxes var ...)))
                  (if (= $arity (length $vars))
                    (cons
                      (datum arity)
                      (lambda ($body)
                        #`(let-values (((var ...) #,($syntax-proc $body))) expr)))
                    (syntax-error $syntax
                      (format "invalid arity ~a, expected 1 in" $arity)))))
              ((let var expr)
                (identifier? #'var)
                (fluent+ $fluent #`(arity (let (var) expr))))
              ((also (fn x ...))
                (cons
                  $arity
                  (lambda ($body)
                    (let (($tmps (generate-temporaries (iota $arity))))
                      #`(let-values (((#,@$tmps) #,($syntax-proc $body)))
                        (fn #,@$tmps x ...)
                        (values #,@$tmps))))))
              ((fn x ...)
                (cons
                  (datum arity)
                  (lambda ($body)
                    (let (($tmps (generate-temporaries (iota $arity))))
                      #`(let-values (((#,@$tmps) #,($syntax-proc $body)))
                        (fn #,@$tmps x ...))))))
              (other
                (case $arity
                  ((0)
                    (cons
                      (datum arity)
                      (lambda ($body)
                        #`(let () #,($syntax-proc $body) other))))
                  (else
                    (syntax-error #'other))))))
          ((values x ...)
            (fluent+ $fluent
              #`(
                #,(literal->syntax (length (syntaxes x ...)))
                #,$syntax)))
          (other
            (fluent+ $fluent #`(1 other))))))

    (syntax-case $syntax ()
      ((fluent x ...)
        (
          (cdr
            (fold-left
              fluent+
              (cons 0 (lambda ($body) $body))
              (syntaxes x ...)))
          #'(values))))))
