(library (tico compiler)
  (export
    compiler
    compiler+global
    compiler-bind
    compiler-lets
    compiler-flatten
    globals-compiler
    compiler-globalize
    compiler-compiled
    compiler-comptime

    literal-compiler
    struct-compiler)
  (import
    (micascheme)
    (tico type)
    (tico typed)
    (tico compiled))

  (define (compiler $value)
    (lambda ($globals)
      (compiled $globals $value)))

  (define (compiler+global $compiler $global)
    (lambda ($globals)
      (app $compiler (push $globals $global))))

  (define (globals-compiler)
    (lambda ($globals)
      (compiled $globals $globals)))

  (define (compiler-bind $compiler $fn)
    (lambda ($globals)
      (lets
        ($compiled (app $compiler $globals))
        (app
          ($fn (compiled-value $compiled))
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

  (define (compiler-globalize $compiler)
    (lambda ($globals)
      (compiled-globalize (app $compiler $globals))))

  (define (compiler-compiled $compiler)
    (app $compiler (globals)))

  (define (compiler-comptime $compiler)
    (compiled-comptime (compiler-compiled $compiler)))

  (define (literal-compiler $literal)
    (compiler
      (literal-typed $literal)))

  (define (struct-compiler $name $compilers)
    (compiler-lets
      ($typed-items (compiler-flatten $compilers))
      ($globals (globals-compiler))
      (compiler (typed-struct $name $globals $typed-items))))
)
