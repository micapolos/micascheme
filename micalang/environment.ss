(library (micalang environment)
  (export
    environment
    environment-type?
    environment-push
    environment-symbols
    environment-types
    environment-type-terms
    environment-values
    mica-environment)
  (import
    (only (micalang base) cons dot push list define lets not cdr map car cadr caddr cadddr caar assq memp lambda and define-rule-syntax quasiquote unquote ...)
    (only (micalang term) term-equal?)
    (micalang compiled)
    (except (micalang comptime) lambda))

  (define-rule-syntax (environment (id type value) ...)
    `((id . ,(compiled type 'type value)) ...))

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

  (define (environment-type-terms $environment)
    (map (dot compiled-type-term cdr) $environment))

  (define (environment-values $environment)
    (map (dot compiled-ref cdr) $environment))

  (define mica-environment
    (environment
      (any-type    any-type   any-type)
      (any-boolean any-type   any-boolean)
      (any-number  any-type   any-number)
      (any-char    any-type   any-char)
      (any-symbol  any-type   any-symbol)
      (any-string  any-type   any-string)

      (zero?   (pi any-number any-boolean)              (curry %%zero? (a any-number)))

      (=       (pi any-number (pi any-number any-number))   (curry %%= (a any-number) (b any-number)))
      (+       (pi any-number (pi any-number any-number))   (curry %%+ (a any-number) (b any-number)))
      (-       (pi any-number (pi any-number any-number))   (curry %%- (a any-number) (b any-number)))
      (<       (pi any-number (pi any-number any-boolean))  (curry %%< (a any-number) (b any-number)))))
)
