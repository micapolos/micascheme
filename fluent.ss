(library (fluent)
  (export fluent also with)
  (import (scheme) (syntax) (procedure))

  (define-aux-keywords also with)

  (define-syntax (fluent $syntax)
    (define (arity-syntax $syntax)
      (syntax-case $syntax (values)
        ((arity expr)
          (and (integer? (datum arity)) (nonnegative? (datum arity)))
          $syntax)
        ((values x ...)
          #`(
            #,(literal->syntax (length (syntaxes x ...)))
            #,$syntax))
        (_
          #`(1 #,$syntax))))

    (define (fluent+ $fluent $syntax)
      (let
        (
          ($arity (car $fluent))
          ($syntax-proc (cdr $fluent)))
        (syntax-case (arity-syntax $syntax) ()
          ((arity expr)
            (syntax-case #'expr (let also with)
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
              ((with x ...)
                (let*
                  (
                    ($tmps (generate-temporaries (iota $arity)))
                    ($x-arity-syntaxes (map (dot syntax->list arity-syntax) (syntaxes x ...)))
                    ($x-arities (map (dot syntax->datum car) $x-arity-syntaxes))
                    ($x-syntaxes (map cdr $x-arity-syntaxes))
                    ($x-tmps (map (dot generate-temporaries iota) $x-arities)))
                  (cons
                    (apply + $arity $x-arities)
                    (lambda ($body)
                      #`(let-values
                        (
                          ((#,@$tmps) #,($syntax-proc $body))
                          #,@(map syntax-cons (map list->syntax $x-tmps) $x-syntaxes))
                        (values #,@$tmps #,@(apply append $x-tmps)))))))
              ((fn x ...)
                (cons
                  (datum arity)
                  (lambda ($body)
                    (let (($tmps (generate-temporaries (iota $arity))))
                      #`(let-values (((#,@$tmps) #,($syntax-proc $body)))
                        (fn #,@$tmps x ...)))))))))))

    (syntax-case $syntax ()
      ((fluent x xs ...)
        (
          (cdr
            (fold-left
              fluent+
              (syntax-case (arity-syntax #'x) ()
                ((arity expr)
                  (cons
                    (datum arity)
                    (lambda ($body) #'expr))))
              (syntaxes xs ...)))
          #'(void))))))
