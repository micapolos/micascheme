(library (leo2 expand)
  (export
    expanded expanded? expanded-type expanded-datum
    expand
    check)
  (import
    (rename (leo2 base) (check %check))
    (leo2 term)
    (leo2 datum)
    (leo2 symbol)
    (leo2 equal)
    (leo2 reify)
    (leo2 stdlib))

  (data (expanded type datum))

  (define (env-push $env $id $expanded)
    (push $env (cons $id $expanded)))

  (define (env-ref? $env $id)
    (lets
      ($ass? (assq (syntax->datum $id) $env))
      (and $ass? (cdr $ass?))))

  ; TODO: inject $env
  (define (evaluate $env $syntax)
    (lets
      ($expanded (expand $env $syntax))
      (eval
        (syntax->datum (expanded-datum $expanded))
        (environment '(leo2 comptime) '(prefix (micascheme) %)))))

  (define (expand $env $syntax)
    (syntax-case (syntax->datum/annotation $syntax)
      (
        type
        boolean
        number
        char
        string
        a-lambda
        native
        native-lambda
        native-apply
        lambda
        at
        :)

      (s
        (and (symbol? (datum s)) (env-ref? $env (datum s)))
        (env-ref? $env (datum s)))

      (idx
        (and
          (symbol? (datum idx))
          (symbol->index? (datum idx)))
        (list-ref $env
          (-
            (length $env)
            (symbol->index? (datum idx))
            1)))

      (type (expanded (type 1) 'type))

      (boolean
        (expanded
          (symbol-type-term 'boolean)
          '(symbol boolean)))

      (number
        (expanded
          (symbol-type-term 'number)
          '(symbol number)))

      (char
        (expanded
          (symbol-type-term 'char)
          '(symbol char)))

      (string
        (expanded
          (symbol-type-term 'string)
          '(symbol string)))

      (b
        (boolean? (datum b))
        (expanded boolean-type `(literal ,(datum b))))
      (n
        (number? (datum n))
        (expanded number-type `(literal ,(datum n))))
      (ch
        (char? (datum ch))
        (expanded char-type `(literal ,(datum ch))))
      (s
        (string? (datum s))
        (expanded string-type `(literal ,(datum s))))

      ((x n)
        (and
          (symbol? (datum n))
          (symbol->index? (datum n)))
        (lets
          ($index (symbol->index? (datum n)))
          ($x (expand $env #'x))
          (expanded
            (indexed-type-term $index (expanded-type $x))
            `(indexed
              ,$index
              ,(expanded-datum $x)))))

      ((native t x)
        (expanded
          (evaluate $env #'t)
          `(native
            ,(expanded-datum (expand $env #'t))
            ,(datum x))))

      ((native-lambda id t ... r)
        (lets
          ($depth (length $env))
          ($symbols
            (map depth->symbol
              (map (partial + $depth)
                (indices (length #'(t ...))))))
          (expand $env
            #`(lambda
              #,@(map-with
                ($symbol $symbols)
                ($t #'(t ...))
                #`(#,$symbol : #,$t))
              (native-apply r id #,@$symbols)))))

      ((native-apply t id arg ...)
        (expanded
          (evaluate $env #'t)
          `(native-apply
            ,(expanded-datum (expand $env #'t))
            ,(datum id)
            ,@(map expanded-datum (map (partial expand $env) #'(arg ...))))))

      ((lambda (id : t) body)
        (lets
          ($id (datum $id))
          ($symbol (depth->symbol (length $env)))
          ($param-type (evaluate $env #'t))
          ($t (expanded-datum (expand $env #'t)))
          ($body-env
            (env-push $env $id
              (expanded $param-type
                `(variable ,$t ,$symbol))))
          (expanded
            (abstraction-type-term $param-type
              (lambda ($arg)
                (lets
                  ($body-env
                    (env-push $env $id
                      (expanded $param-type
                        `(variable ,$t ,$symbol))))
                  (expanded-type
                    (expand $body-env #'body)))))
            `(lambda
              (,$symbol ,$t)
              ,(expanded-datum
                (expand $body-env #'body))))))

      ((lambda param params ... body)
        (expand $env
          #'(lambda param
            (lambda params ... body))))

      ((a-lambda (id : t) body)
        (lets
          ($symbol (depth->symbol (length $env)))
          ($expanded-t (expand $env #'t))
          (expanded
            (type 0)
            `(a-lambda
              (,$symbol ,(expanded-datum $expanded-t))
              ,(expanded-datum
                (expand
                  (push $env
                    (cons
                      (datum id)
                      (expanded
                        (expanded-type $expanded-t)
                        `(variable ,(expanded-datum $expanded-t) ,$symbol))))
                  #'body))))))

      ((a-lambda t body)
        (expand $env #'(a-lambda (_ : t) body)))

      ((a-lambda param params ... body)
        (expand $env
          #'(a-lambda param
            (a-lambda params ... body))))

      ((fn arg)
        (lets
          ($depth (length $env))
          ($fn (expand $env #'fn))
          (switch (expanded-type $fn)
            ((abstraction-type? $abstraction-type)
              (lets
                ($arg (expand $env #'arg))
                ($expected-type (abstraction-type-param $abstraction-type))
                ($actual-type (expanded-type $arg))
                (if
                  (term=? $depth $expected-type $actual-type)
                  `(
                    ,(expanded-datum $fn)
                    ,(expanded-datum $arg))
                  (syntax-error #'arg
                    (format "expected ~a, got ~a, in "
                      (reify $depth $expected-type)
                      (reify $depth $actual-type))))))
            ((else $other)
              (syntax-error #'fn "not a-lambda")))))

      ((fn arg args ...)
        (expand $env
          #'((fn arg) args ...)))

      (id
        (symbol? (datum id))
        (syntax-error #'id "undefined"))))

  (define-rules-syntaxes
    (literals expand expanded)
    ((check (expand in) (expanded t v))
      (lets
        ($expanded (expand (list) #'in))
        (%check
          (equal?
            (expanded
              (term->datum #f #t 0 (expanded-type $expanded))
              (expanded-datum $expanded))
            (expanded
              (term->datum #f #t 0 t)
              'v))))))
)
