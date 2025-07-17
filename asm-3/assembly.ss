(library (asm-3 assembly)
  (export
    assembly assembly? assembly-bindings
    empty-assembly
    identifier->assembly
    assembly->datum
    check-assembly)
  (import
    (micascheme)
    (syntax lookup)
    (asm-3 block)
    (asm-3 expression))

  (data (assembly bindings))

  (define (empty-assembly)
    (assembly (list)))

  (define (assembly-ref? $assembly $identifier)
    (assid $identifier (assembly-bindings $assembly)))

  (define (assembly+binding $assembly $identifier $value)
    (assembly-with-bindings $assembly
      (push
        (assembly-bindings $assembly)
        (cons $identifier $value))))

  (define (assembly+deps $lookup $assembly $deps)
    (fold-left (partial assembly+dep $lookup) $assembly $deps))

  (define (assembly+dep $lookup $assembly $dep)
    (switch (assembly-ref? $assembly $dep)
      ((false? _)
        (switch (lookup-ref $lookup $dep)
          ((block? $block)
            (assembly+binding
              (assembly+deps $lookup $assembly (reverse (block-deps $block)))
              $dep
              $block))
          ((expression? $expression)
            (assembly+binding
              (assembly+deps $lookup $assembly (expression-deps $expression))
              $dep
              $expression))
          ((else $other)
            (syntax-error $dep "can not assemble"))))
      ((else _)
        $assembly)))

  (define (assembly->datum $assembly)
    `(assembly
      ,@(map-with ($binding (reverse (assembly-bindings $assembly)))
        `(
          ,(syntax->datum (car $binding))
          ,(switch-exhaustive (cdr $binding)
            ((block? $block) (block->datum $block))
            ((expression? $expression) (expression->datum $expression)))))))

  (define (identifier->assembly $lookup $identifier)
    (assembly+dep $lookup (empty-assembly) $identifier))

  (define-rule-syntax (check-assembly in out)
    (check (equal? (assembly->datum in) 'out)))
)
