(library (micalang mica)
  (export mica check-mica)
  (import
    (micalang base)
    (micalang compiler)
    (micalang env))

  (define-syntax (mica $syntax)
    (syntax-case $syntax ()
      ((mica x)
        (literal->syntax
          (mica-evaluate mica-env (datum x))))))

  (define-rule-syntax (check-mica in out)
    (check (equal? (mica in) (mica out))))
)
