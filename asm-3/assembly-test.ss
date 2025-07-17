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
      (c (expression-with (a b) (+ a b)))
      (block-1 (fluent (empty-block) (block+binary 1 #'binary-1)))
      (block-2 (fluent (empty-block) (block+binary 2 #'binary-2)))
      (main
        (fluent
          (empty-block)
          (block+dep #'c)
          (block+dep #'block-1)
          (block+dep #'block-2)
          (block+binary 10 #'main-binary))))
    #'main)
  (assembly
    (a (expression () a))
    (b (expression () b))
    (c (expression (a b) (+ a b)))
    (block-1 (block (alignment 1) (size 1) (deps) (put-binary $port binary-1)))
    (block-2 (block (alignment 1) (size 2) (deps) (put-binary $port binary-2)))
    (main (block (alignment 1) (size 10) (deps c block-1 block-2) (put-binary $port main-binary)))))
