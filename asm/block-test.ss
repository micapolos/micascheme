(import (micascheme) (asm block))

(check-datum=?
  (block->syntax (empty-block))
  '(blob 0 (lambda ($port) (void))))

(check-datum=?
  (block->syntax (u8-block 10 20 30))
  '(blob 3
    (lambda ($port)
      (put-u8 $port 10)
      (put-u8 $port 20)
      (put-u8 $port 30))))

(check-datum=?
  (block->syntax
    (block-with ($port 2)
      (put-u8 $port 10)
      (put-u8 $port 20)))
  '(blob 2
    (lambda ($port)
      (put-u8 $port 10)
      (put-u8 $port 20))))

(check-datum=?
  (block->syntax
    (block-append
      (block-with ($port 2)
        (put-u8 $port 10)
        (put-u8 $port 20))
      (block-with ($port 2)
        (put-u8 $port 30)
        (put-u8 $port 40))))
  '(blob 4
    (lambda ($port)
      (put-u8 $port 10)
      (put-u8 $port 20)
      (put-u8 $port 30)
      (put-u8 $port 40))))
