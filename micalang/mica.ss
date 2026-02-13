(library (micalang mica)
  (export
    mica
    check-mica
    print
    mica-debug)
  (import
    (micalang base)
    (micalang compiler)
    (micalang environment)
    (micalang compiled)
    (micalang term)
    (micalang reify))

  (define-syntax (mica $syntax)
    (syntax-case $syntax ()
      ((_ x)
        #`(compiler-evaluate
          (compiler
            default-compiler-recurse
            default-compiler-reify
            default-compiler-term=?
            (%environment '(micalang runtime) '(prefix (scheme) %))
            (%environment '(micalang comptime) '(prefix (scheme) %))
            mica-environment)
          (syntax->datum/annotation #'x)))))

  (define-syntax (mica-debug $syntax)
    (syntax-case $syntax ()
      ((_ x)
        (lets
          ($compiler
            (compiler
              default-compiler-recurse
              default-compiler-reify
              default-compiler-term=?
              (%environment '(micalang runtime) '(prefix (scheme) %))
              (%environment '(micalang comptime) '(prefix (scheme) %))
              mica-environment))
          ($compiled (compiler-compile $compiler (syntax->datum/annotation #'x)))
          #`(pretty-print
            '(compiled
              #,(datum->syntax #'mica-debug (compiler-reify $compiler (compiled-type $compiled)))
              #,(datum->syntax #'mica-debug (compiled-ref $compiled))))))))

  (define-rule-syntax (check-mica in out)
    (check (equal? (mica in) out)))

  (define-rule-syntax (print x ...)
    (begin (displayln (mica (let x ...)))))
)
