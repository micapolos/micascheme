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
    (asm-3 dependent)
    (asm-3 block))

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

  (define (assembly+dep $lookup $assembly $dep)
    (switch (assembly-ref? $assembly $dep)
      ((false? _)
        (switch (lookup-ref $lookup $dep)
          ((dependent? $dependent)
            (assembly+binding
              (fold-left (partial assembly+dep $lookup) $assembly (reverse (dependent-dep-stack $dependent)))
              $dep
              (dependent-ref $dependent)))
          ((else $other)
            (syntax-error $dep "not dependent"))))
      ((else _)
        $assembly)))

  (define (assembly->datum $assembly)
    `(assembly
      ,@(map-with ($binding (reverse (assembly-bindings $assembly)))
        `(
          ,(syntax->datum (car $binding))
          ,(switch-exhaustive (cdr $binding)
            ((block? $block) (block->datum $block))
            ((syntax? $syntax) (syntax->datum $syntax)))))))

  (define (identifier->assembly $lookup $identifier)
    (assembly+dep $lookup (empty-assembly) $identifier))

  (define-rule-syntax (check-assembly in out)
    (check (equal? (assembly->datum in) 'out)))

  (define (assembly-syntax-lets-entries $assembly)
    (map-with
      ($binding
        (reverse
          (filter
            (lambda ($binding) (syntax? (cdr $binding)))
            (assembly-bindings $assembly))))
      #`(#,(car $binding) #,(cdr $binding))))

  (define (assembly-block-bindings $assembly)
    (sort
      (lambda ($binding-1 $binding-2)
        (>
          (block-alignment (cdr $binding-1))
          (block-alignment (cdr $binding-2))))
      (filter
        (lambda ($binding) (block? (cdr $binding)))
        (assembly-bindings $assembly))))

  (define (block-bindings->labels/runs $block-bindings)
    (fold-left
      (lambda ($labels/runs $block-binding)
        (cons 1 2))
      (cons (stack) (stack))
      $block-bindings))

  (define (assembly->syntax $assembly)
    (lets
      ((pair $label-entries $runs) (block-bindings->labels/runs (assembly-block-bindings $assembly)))
      ($define-entries (assembly-syntax-lets-entries $assembly))
      #`(lambda ($org)
        (lambda ($port)
          (lets
            #,@$label-entries
            #,@$define-entries
            (run #,@$runs))))))
)
