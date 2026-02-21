(library (leo2 expand)
  (export
    expanded expanded? expanded-type expanded-ref
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

  (data (expanded type ref))

  (define (env-ref? $env $id)
    (lets
      ($ass? (assq (syntax->datum $id) $env))
      (and $ass? (cdr $ass?))))

  ; TODO: inject $env
  (define (evaluate $env $syntax)
    (lets
      ($expanded (expand $env $syntax))
      (eval
        (syntax->datum (expanded-ref $expanded))
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
        number
        :)

      (s
        (env-ref? $env (datum s))
        (env-ref? $env (datum s)))

      (type (expanded (type 1) 'type))
      (boolean (expanded (type 0) '(symbol boolean)))
      (number (expanded (type 0) '(symbol number)))
      (char (expanded (type 0) '(symbol char)))
      (string (expanded (type 0) '(symbol string)))

      (b
        (boolean? (datum b))
        (expand-literal a-boolean b))
      (n
        (number? (datum n))
        (expand-literal a-number n))
      (ch
        (char? (datum ch))
        (expand-literal a-char ch))
      (s
        (string? (datum s))
        (expanded
          string-type
          `(native (a-symbol string) ,(datum s))))

      ((x number n)
        (lets
          ($index (- (datum n) 1))
          ($x (expand $env #'x))
          (expanded
            (indexed-type-term $index (expanded-type $x))
            `(indexed
              ,$index
              ,(expanded-ref $x)))))

      ((native t x)
        (expanded
          (evaluate $env #'t)
          `(native
            ,(expanded-ref (expand $env #'t))
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
            ,(expanded-ref (expand $env #'t))
            ,(datum id)
            ,@(map expanded-ref (map (partial expand $env) #'(arg ...))))))

      ((lambda (id : t) body)
        (lets
          ($symbol (depth->symbol (length $env)))
          ($param (evaluate $env #'t))
          ($t (expanded-ref (expand $env #'t)))
          (expanded
            (abstraction-type-term
              (evaluate $env #'t)
              (lambda (x)
                (expanded-type
                  (expand
                    (push $env
                      (cons
                        (datum id)
                        (expanded
                          $param
                          `(variable ,$t ,$symbol))))
                    #'body))))
            `(lambda
              (,$symbol ,$t)
              ,(expanded-ref
                (expand
                  (push $env
                    (cons
                      (datum id)
                      (expanded
                        $param
                        `(variable ,$t ,$symbol))))
                  #'body))))))

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
              (,$symbol ,(expanded-ref $expanded-t))
              ,(expanded-ref
                (expand
                  (push $env
                    (cons
                      (datum id)
                      (expanded
                        (expanded-type $expanded-t)
                        `(variable ,(expanded-ref $expanded-t) ,$symbol))))
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
                    ,(expanded-ref $fn)
                    ,(expanded-ref $arg))
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

  (define-rule-syntax (expand-type t)
    (expanded (type 0) '(native (type 0) 't)))

  (define-rule-syntax (expand-literal t x)
    (expanded
      (native-type 't)
      `(native (native (type 0) 't) ,(datum x))))

  (define-rules-syntaxes
    (literals expand expanded)
    ((check (expand in) (expanded t v))
      (lets
        ($expanded (expand (list) #'in))
        (%check
          (equal?
            (expanded
              (term->datum #f #t 0 (expanded-type $expanded))
              (expanded-ref $expanded))
            (expanded
              (term->datum #f #t 0 t)
              'v))))))
)
