(library (evaluator)
  (export 
    evaluator evaluator? evaluator-environment evaluator-bindings
    empty-evaluator evaluator-push
    evaluate)
  (import (micascheme))

  (data (evaluator environment bindings))

  (define (empty-evaluator $environment)
    (evaluator $environment (list)))

  (define (evaluator-push $evaluator $binding)
    (evaluator 
      (evaluator-environment $evaluator)
      (cons $binding (evaluator-bindings $evaluator))))

  (define (evaluate $evaluator $datum)
    (fold-right
      (lambda ($arg $value) ($value $arg))
      (eval
        (fold-left
          (lambda ($datum $param) `(lambda (,$param) ,$datum))
          $datum
          (map car (evaluator-bindings $evaluator)))
        (evaluator-environment $evaluator))
      (map cdr (evaluator-bindings $evaluator))))
)
