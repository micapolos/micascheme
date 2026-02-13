(library (micalang environment)
  (export
    environment
    environment-type?
    environment-push
    environment-symbols
    environment-types
    environment-values
    mica-environment)
  (import
    (only (micalang base) cons dot push list define lets not cdr map car cadr caddr cadddr caar assq memp lambda and define-rule-syntax quasiquote unquote ...)
    (only (micalang term) term-equal?)
    (micalang compiled)
    (except (micalang comptime) lambda))

  (define-rule-syntax (environment (id type value) ...)
    `((id . ,(compiled type value)) ...))

  (define (environment-push $environment $id $compiled)
    (push $environment
      (cons $id $compiled)))

  (define (environment-type? $environment $id)
    (lets
      ($ass? (assq $id $environment))
      (and $ass? (compiled-type (cdr $ass?)))))

  (define (environment-symbols $environment)
    (map car $environment))

  (define (environment-types $environment)
    (map (dot compiled-type cdr) $environment))

  (define (environment-values $environment)
    (map (dot compiled-ref cdr) $environment))

  (define mica-environment
    (environment
      (zero?
        (a-lambda a-number a-boolean)
        (curry %%zero? a))

      (=
        (a-lambda a-number (a-lambda a-number a-number))
        (curry %%= a b))
      (+
        (a-lambda a-number (a-lambda a-number a-number))
        (curry %%+ a b))
      (-
        (a-lambda a-number (a-lambda a-number a-number))
        (curry %%- a b))
      (<
        (a-lambda a-number (a-lambda a-number a-boolean))
        (curry %%< a b))

      (string-append
        (a-lambda a-string (a-lambda a-string a-string))
        (curry %%string-append a b))
      (string-length
        (a-lambda a-string a-number)
        (curry %%string-length a))
      (number->string
        (a-lambda a-number a-string)
        (curry %%number->string a))))
)
