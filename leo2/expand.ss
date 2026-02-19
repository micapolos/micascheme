(library (leo2 expand)
  (export
    expanded expanded? expanded-type expanded-ref
    expand
    check)
  (import
    (rename (leo2 base) (check %check))
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
      (
        a-type
        a-boolean
        a-number
        a-char
        a-string
        a-lambda
        native
        lambda)

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
          (native (datum x))))))

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
