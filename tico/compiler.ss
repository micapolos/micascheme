(library (tico compiler)
  (export
    compiler
    compiler-compiled
    compiler+global
    compiler-bind
    compiler-lets
    compiler-flatten
    compiler-globalize
    literal-compiler
    globals-compiler)
  (import
    (micascheme)
    (tico compiled))

  (define (compiler $value)
    (lambda ($globals)
      (compiled $globals $value)))

  (define (literal-compiler $literal)
    (lambda ($globals)
      (compiled $globals
        (literal-typed $literal))))

  (define (compiler+global $compiler $global)
    (lambda ($globals)
      (app $compiler
        (push $globals $global))))

  (define (compiler-compiled $compiler)
    (app $compiler (globals)))

  (define (compiler-bind $compiler $fn)
    (lambda ($globals)
      (lets
        ($compiled (app $compiler $globals))
        (app
          (app $fn (compiled-value $compiled))
          (compiled-globals $compiled)))))

  (define-syntax compiler-lets
    (syntax-rules ()
      ((_ $body) $body)
      ((_ ($value $compiler) $decl ... $body)
        (identifier? #'$value)
        (compiler-bind $compiler
          (lambda ($value)
            (compiler-lets $decl ... $body))))))

  (define (compiler-flatten $list)
    (if (null? $list)
      (compiler (list))
      (compiler-lets
        ($car (car $list))
        ($cdr (compiler-flatten (cdr $list)))
        (compiler (cons $car $cdr)))))

  (define (globals-compiler)
    (lambda ($globals)
      (pure-compiled $globals)))

  (define (compiler-globalize $compiler)
    (lambda ($globals)
      (compiled-globalize
        (app $compiler $globals))))
)
