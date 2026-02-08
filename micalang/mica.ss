(library (micalang mica)
  (export mica check-mica mica-print)
  (import
    (micalang base)
    (micalang compiler)
    (micalang env)
    (micalang context)
    (micalang compiled)
    (micalang term)
    (micalang reify))

  (define-syntax (mica $syntax)
    (syntax-case $syntax ()
      ((_ x)
        #`(compiler-evaluate
          (compiler
            (lambda ($compiler $term)
              (syntax-case $term (fx)
                (fx
                  (compiled type 'type '(constant fx)))
                ((fx n)
                  (if (fixnum? (datum n))
                    (compiled (constant 'fx) 'fx `(native ,(datum n)))
                    (syntax-error #'n "not fx")))
                (other
                  (compiler-compile-default $compiler #'other))))
            default-compiler-reify
            default-compiler-term-equal?
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
