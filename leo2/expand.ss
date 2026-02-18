(library (leo2 expand)
  (export
    expanded expanded? expanded-type expanded-ref
    expand
    check-expanded=?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum))

  (data (expanded type ref))

  ; TODO: inject $env
  (define (evaluate $env $syntax)
    (lets
      ($expanded (expand $env $syntax))
      (eval
        (syntax->datum (expanded-ref $expanded))
        (environment '(leo2 comptime) '(prefix (micascheme) %)))))

  (define (expand $env $syntax)
    (syntax-case (syntax->datum/annotation $syntax)
      (a-type a-boolean a-number a-string a-lambda
        native lambda)

      (a-type
        (expanded (type 1) 'a-type))
      (a-boolean
        (expanded (type 0) '(native 'a-boolean)))
      (b
        (boolean? (datum b))
        (expanded (native 'a-boolean) (native (datum b))))
      ((native t x)
        (expanded
          (evaluate $env #'t)
          (native (datum x))))))

  (define-rule-syntax (check-expanded=? in (_ t v))
    (lets
      ($expanded (expand (list) #'in))
      (check
        (equal?
          (expanded
            (term->datum 0 #f (expanded-type $expanded))
            (expanded-ref $expanded))
          (expanded
            (term->datum 0 #f t)
            'v)))))
)
