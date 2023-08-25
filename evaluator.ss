(library (evaluator)
  (export 
    evaluator evaluator? evaluator-environment evaluator-bindings
    evaluate)
  (import (micascheme))

  (data (evaluator environment bindings))

  (define (evaluate $evaluator $datum)
    (fold-left
      (lambda ($value $arg) ($value $arg))
      (eval
        (fold-right
          (lambda ($param $datum) `(lambda (,$param) ,$datum))
          $datum
          (map car (evaluator-bindings $evaluator)))
        (evaluator-environment $evaluator))
      (map cdr (evaluator-bindings $evaluator))))
)