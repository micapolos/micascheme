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
      (type    type   type)
      (boolean type   boolean)
      (number  type   number)
      (char    type   char)
      (symbol  type   symbol)
      (string  type   string)

      (zero?   (pi number boolean)              (curry %%zero? (a number)))

      (=       (pi number (pi number number))   (curry %%= (a number) (b number)))
      (+       (pi number (pi number number))   (curry %%+ (a number) (b number)))
      (-       (pi number (pi number number))   (curry %%- (a number) (b number)))
      (<       (pi number (pi number boolean))  (curry %%< (a number) (b number)))))
)
