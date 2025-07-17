(import (micascheme) (asm-3 block) (asm-2 relocable) (asm-3 identified))

(check-block
  (fluent (empty-block))
  100
  (block
    (alignment 1)
    (size 0)
    (lets (binary-append))))

(check-block
  (fluent (empty-block)
    (block+zeros 10))
  100
  (block
    (alignment 1)
    (size 10)
    (lets
      (binary-append
        (zero-binary 10)))))

(check-block
  (fluent (empty-block)
    (block+define #'size #'(- end start))
    (block+label #'start)
    (block+binary 1 (relocable-with #`binary-1))
    (block+binary 2 (relocable-with #`binary-2))
    (block-align 8)
    (block+label #'mid)
    (block+binary 3 (relocable-with #`binary-3))
    (block+label #'end))
  100
  (block
    (alignment 8)
    (size 11)
    (lets
      (start 100)
      (mid 108)
      (end 111)
      (size (- end start))
      (binary-append
        binary-1
        binary-2
        (zero-binary 5)
        binary-3))))

(check-block
  (block-append
    (block 4 30
      (stack
        (identified-with label-1-10 10)
        (identified-with label-1-20 20))
      (stack
        (identified-with expr-1-10 #'syntax-1-10)
        (identified-with expr-1-20 #'syntax-1-20))
      (stack
        (relocable-with ($org) #`(binary #,(literal->syntax $org)))
        (relocable-with ($org) #`(binary #,(literal->syntax (+ $org 10))))))
    (block 2 50
      (stack
        (identified-with label-2-10 20)
        (identified-with label-2-20 40))
      (stack
        (identified-with expr-2-10 #'syntax-2-10)
        (identified-with expr-2-20 #'syntax-2-20))
      (stack
        (relocable-with ($org) #`(binary #,(literal->syntax $org)))
        (relocable-with ($org) #`(binary #,(literal->syntax (+ $org 20)))))))
  100
  (block
    (alignment 4)
    (size 80)
    (lets
      (label-1-10 110)
      (label-1-20 120)
      (label-2-10 150)
      (label-2-20 170)
      (expr-1-10 syntax-1-10)
      (expr-1-20 syntax-1-20)
      (expr-2-10 syntax-2-10)
      (expr-2-20 syntax-2-20)
      (binary-append
        (binary 100)
        (binary 110)
        (binary 130)
        (binary 150)))))
