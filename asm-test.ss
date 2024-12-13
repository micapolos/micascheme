(import (micascheme) (asm))

(check-datum=?
  (asm->put-syntax
    (asm 10
      (stack
        #'(label-1 10)
        #'(label-2 20))
      (stack
        #'(value-1 30)
        #'(value-2 (+ value-1 label-1)))
      (stack
        (lambda ($port) #'(put-u8 $port 1))
        (lambda ($port) #'(put-u8 $port 2))))
    #'$port)
  `(lets
    (label-1 10)
    (label-2 20)
    (value-1 30)
    (value-2 (+ value-1 label-1))
    (run
      (put-u8 $port 1)
      (put-u8 $port 2))))
