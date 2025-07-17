(import (micascheme) (syntax lookup) (asm-3 assembly) (asm-3 expression) (asm-3 block))

(check-assembly
  (empty-assembly)
  (assembly))

(check-assembly
  (assembly
    (stack
      (cons #'a (expression-with () a))
      (cons #'b (expression-with () b))))
  (assembly
    (a (expression () a))
    (b (expression () b))))

(check-assembly
  (identifier->assembly
    (lookup-with
      (a (expression-with () a))
      (b (expression-with () b))
      (c (expression-with (a b) (+ a b))))
    #'c)
  (assembly
    (a (expression () a))
    (b (expression () b))
    (c (expression (a b) (+ a b)))))
