(library (micalang mica)
  (export mica check-mica mica-print)
  (import
    (micalang base)
    (micalang compiler)
    (micalang env)
    (micalang context)
    (micalang compiled)
    (micalang term)
    (micalang reify)
    (micalang fx))

  (define-syntax (mica $syntax)
    (syntax-case $syntax ()
      ((_ x)
        #`(compiler-evaluate
          (compiler
            (lambda ($compiler $term)
              (syntax-case $term (fx)
                (fx
                  (compiled type 'type 'fx))
                ((fx n)
                  (if (fixnum? (datum n))
                    (compiled fx 'fx `(native ,(datum n)))
                    (syntax-error #'n "not fx")))
                (other
                  (compiler-compile-default $compiler #'other))))

            (lambda ($default $term)
              (switch-exhaustive $term
                ((fx? _) 'fx)))

            (lambda ($default $lhs $rhs)
              (switch-exhaustive $lhs
                ((fx? _)
                  (switch? $rhs
                    ((fx? _) #t)))))

            (environment '(micalang runtime) '(prefix (scheme) %))
            (environment '(micalang comptime) '(prefix (scheme) %))
            mica-env
            mica-context)
          (syntax->datum/annotation #'x)))))

  (define-rule-syntax (check-mica in out)
    (check (equal? (mica in) (mica out))))

  (define-rule-syntax (mica-print x ...)
    (begin (displayln (mica x)) ...))
)
