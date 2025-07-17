(import (micascheme) (syntax lookup) (asm-3 assembly) (asm-3 dependent) (asm-3 block))

(check-assembly
  (empty-assembly)
  (assembly))

(check-assembly
  (assembly
    (stack
      (cons #'a #'syntax-a)
      (cons #'b #'syntax-b)))
  (assembly
    (a syntax-a)
    (b syntax-b)))

(check-assembly
  (identifier->assembly
    (lookup-with
      (a (dependent-with () #'a))
      (b (dependent-with () #'b))
      (c (dependent-with (a b) #'(+ a b)))
      (block-1
        (dependent-with ()
          (fluent (empty-block)
            (block+binary 1 #'binary-1))))
      (block-2
        (dependent-with ()
          (fluent (empty-block)
            (block+binary 2 #'binary-2))))
      (main
        (dependent-with (c block-1 block-2)
          (fluent
            (empty-block)
            (block+binary 10 #'main-binary)))))
    #'main)
  (assembly
    (a a)
    (b b)
    (c (+ a b))
    (block-1 (block (alignment 1) (size 1) (labels) (defs) (body (put-binary $port binary-1))))
    (block-2 (block (alignment 1) (size 2) (labels) (defs) (body (put-binary $port binary-2))))
    (main (block (alignment 1) (size 10) (labels) (defs) (body (put-binary $port main-binary))))))
