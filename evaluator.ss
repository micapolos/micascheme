(library (evaluator)
  (export 
    evaluator evaluator? evaluator-environment evaluator-bindings
    empty-evaluator evaluator-push
    evaluate
    evaluator-bound?
    evaluator-ref
    evaluator+)
  (import (micascheme))

  (data (evaluator environment bindings))

  (define (empty-evaluator $environment)
    (evaluator $environment (list)))

  (define (evaluator-push $evaluator $binding)
    (evaluator 
      (evaluator-environment $evaluator)
      (cons $binding (evaluator-bindings $evaluator))))

  (define (evaluator+ $evaluator $symbol $value)
    (evaluator-push $evaluator (cons $symbol $value)))

  (define (evaluate $evaluator $datum)
    (fold-right
      (lambda ($arg $fn) ($fn $arg))
      (eval
        (fold-left
          (lambda ($datum $param) `(lambda (,$param) ,$datum))
          $datum
          (map car (evaluator-bindings $evaluator)))
        (evaluator-environment $evaluator))
      (map cdr (evaluator-bindings $evaluator))))

  (define (evaluator-bound? $evaluator $symbol)
    (not
      (false?
        (or
          (assv $symbol (evaluator-bindings $evaluator))
          (top-level-bound? $symbol (evaluator-environment $evaluator))))))

  (define (evaluator-ref $evaluator $symbol)
    (or
      (lets?
        ($ass (assv $symbol (evaluator-bindings $evaluator)))
        (cdr $ass))
      (top-level-value $symbol (evaluator-environment $evaluator))))
)
