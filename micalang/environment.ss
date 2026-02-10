(library (micalang environment)
  (export
    environment
    environment-type?
    environment-symbols
    environment-types
    environment-values
    environment-id?
    mica-environment)
  (import
    (only (micalang base) define lets not cdr map car cadr caddr caar assq memp lambda and define-rule-syntax quasiquote unquote ...)
    (only (micalang term) term-equal?)
    (except (micalang comptime) lambda))

  (define-rule-syntax (environment (id type value) ...)
    `((id ,type ,value) ...))

  (define (environment-type? $environment $id)
    (lets
      ($ass? (assq $id $environment))
      (and $ass? (cadr $ass?))))

  (define (environment-id? $environment $type)
    (lets
      ($tail?
        (memp
          (lambda ($ass) (term-equal? (cadr $ass) $type))
          $environment))
      (and
        $tail?
        (not
          (memp
            (lambda ($ass) (term-equal? (cadr $ass) $type))
            (cdr $tail?)))
        (caar $tail?))))

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
