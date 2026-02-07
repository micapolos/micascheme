(library (micalang mica)
  (export mica check-mica mica-print)
  (import
    (micalang base)
    (micalang compiler)
    (micalang env))

  (define-syntax (mica $syntax)
    (syntax-case $syntax ()
      ((_ x)
        #`(mica-evaluate mica-env
          (syntax->datum/annotation #'x)))))

  (define-rule-syntax (check-mica in out)
    (check (equal? (mica in) (mica out))))

  (define-rule-syntax (mica-print x ...)
    (begin (displayln (mica x)) ...))
)
