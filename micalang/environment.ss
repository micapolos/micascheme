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
    (only (micalang base) push list define lets not cdr map car cadr caddr cadddr caar assq memp lambda and define-rule-syntax quasiquote unquote ...)
    (only (micalang term) term-equal?)
    (except (micalang comptime) lambda))

  (define-rule-syntax (environment (id type value) ...)
    `((id ,type type ,value) ...))

  (define (environment-push $environment $id $type $type-term $value)
    (push $environment (list $id $type $type-term $value)))

  (define (environment-type? $environment $id)
    (lets
      ($ass? (assq $id $environment))
      (and $ass? (cadr $ass?))))

  (define (environment-symbols $environment)
    (map car $environment))

  (define (environment-types $environment)
    (map cadr $environment))

  (define (environment-type-terms $environment)
    (map caddr $environment))

  (define (environment-values $environment)
    (map cadddr $environment))

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
