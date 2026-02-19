(library (leo2 expand)
  (export
    expanded expanded? expanded-type expanded-ref
    expand
    check)
  (import
    (rename (leo2 base) (check %check))
    (leo2 term)
    (leo2 datum)
    (leo2 symbol))

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
        a-type
        a-boolean
        a-number
        a-char
        a-string
        a-lambda
        native
        native-apply
        lambda
        :)

      (s
        (env-ref? $env (datum s))
        (env-ref? $env (datum s)))

      (a-type (expanded (type 1) 'a-type))
      (a-boolean (expand-type a-boolean))
      (a-number (expand-type a-number))
      (a-char (expand-type a-char))
      (a-string (expand-type a-string))

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
        (expand-literal a-string s))

      ((native t x)
        (expanded
          (evaluate $env #'t)
          `(native ,(datum x))))

      ((native-apply t fn arg ...)
        (expanded
          (evaluate $env #'t)
          `(native-application
            (native ,(datum fn))
            (list ,@(map expanded-ref (map (partial expand $env) #'(arg ...)))))))

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
                        `(variable ,$symbol))))
                  #'body))))))

      ((a-lambda t body)
        (expand $env #'(a-lambda (_ : t) body)))

      ((a-lambda param params ... body)
        (expand $env
          #'(a-lambda param
            (a-lambda params ... body))))

      ((fn arg)
        TODO)

      (id
        (symbol? (datum id))
        (syntax-error #'id "undefined"))))

  (define-rule-syntax (expand-type t)
    (expanded (type 0) '(variable t)))

  (define-rule-syntax (expand-literal t x)
    (expanded (native 't) `(native ,(datum x))))

  (define-rules-syntaxes
    (literals expand expanded)
    ((check (expand in) (expanded t v))
      (lets
        ($expanded (expand (list) #'in))
        (%check
          (equal?
            (expanded
              (term->datum 0 #t (expanded-type $expanded))
              (expanded-ref $expanded))
            (expanded
              (term->datum 0 #t t)
              'v))))))
)
