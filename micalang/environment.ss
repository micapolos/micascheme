(library (micalang environment)
  (export
    environment
    environment-type?
    environment-symbols
    environment-types
    environment-values
    mica-environment)
  (import
    (only (micalang base) define lets map car cadr caddr assq and define-rule-syntax quasiquote unquote ...)
    (micalang comptime))

  (define-rule-syntax (environment (id type value) ...)
    `((id ,type ,value) ...))

  (define (environment-type? $environment $id)
    (lets
      ($ass? (assq $id $environment))
      (and $ass? (cadr $ass?))))

  (define (environment-symbols $environment)
    (map car $environment))

  (define (environment-types $environment)
    (map cadr $environment))

  (define (environment-values $environment)
    (map caddr $environment))

  (define mica-environment
    (environment
      (type    type   type)
      (boolean type   boolean)
      (number  type   number)
      (char    type   char)
      (string  type   string)

      (zero?   (pi number boolean)              (curry %%zero? a))

      (=       (pi number (pi number number))   (curry %%= a b))
      (+       (pi number (pi number number))   (curry %%+ a b))
      (-       (pi number (pi number number))   (curry %%- a b) )
      (<       (pi number (pi number boolean))  (curry %%< a b) )))
)
