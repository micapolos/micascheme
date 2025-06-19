(import (micascheme) (asm-2 block))

(check-datum=?
  (block-bytevector-syntax
    (block
      2
      (stack
        (cons #'a 10)
        (cons #'b 20))
      (stack
        #`(lambda ($port) (put-u8 $port a))
        #`(lambda ($port) (put-u8 $port b)))))
  '(lets
    (a 10)
    (b 20)
    (call-with-bytevector-output-port
      (lambda ($port)
        (for-each
          (lambda ($put) ($put $port))
          (list
            (lambda ($port) (put-u8 $port a))
            (lambda ($port) (put-u8 $port b))))))))
