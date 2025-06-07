(import (micascheme) (asm block))

(lets
  ($block (empty-block))
  (run
    (check (equal? (block-size $block) 0))
    (check-datum=?
      (block->put-proc-syntax $block)
      '(lambda ($port)
        (void)))))

(lets
  ($block (u8-block 10 20 30))
  (run
    (check (equal? (block-size $block) 3))
    (check-datum=?
      (block->put-proc-syntax $block)
      '(lambda ($port)
        (put-u8 $port 10)
        (put-u8 $port 20)
        (put-u8 $port 30)))))

; === block-size

(check
  (equal?
    (block-size
      (block-append
        (block-with (2 $port)
          (put-u8 $port 10)
          (put-u8 $port 20))
        (block-with (2 $port)
          (put-u8 $port 30)
          (put-u8 $port 40))))
    4))

; === block-syntax

(check-datum=?
  (block->put-proc-syntax
    (block-with (2 $port)
      (put-u8 $port 10)
      (put-u8 $port 20)))
  '(lambda ($port)
    (put-u8 $port 10)
    (put-u8 $port 20)))

(check-datum=?
  (block->put-proc-syntax
    (block-append
      (block-with (2 $port)
        (put-u8 $port 10)
        (put-u8 $port 20))
      (block-with (2 $port)
        (put-u8 $port 30)
        (put-u8 $port 40))))
  '(lambda ($port)
    (put-u8 $port 10)
    (put-u8 $port 20)
    (put-u8 $port 30)
    (put-u8 $port 40)))
