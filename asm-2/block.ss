(library (asm-2 block)
  (export
    block block?
    block-size block-labels block-puts
    block-with-size block-with-labels  block-with-puts
    empty-block
    block-apply block-bytevector-syntax
    block+label block+data block+u8)
  (import (micascheme))

  (data (block size labels puts))

  (define (u8? $obj)
    (and (integer? $obj) (>= $obj #x00) (<= $obj #xff)))

  (define (empty-block)
    (block 0 (stack) (stack)))

  (define (block+label $block $label)
    (block-with-labels $block
      (push
        (block-labels $block)
        (cons $label (block-size $block)))))

  (define (block+data $block $size $put)
    (block-with-puts
      (block-with-size $block (+ (block-size $block) $size))
      (push (block-puts $block) $put)))

  (define (block+u8 $block $expr)
    (block+data $block 1
      #`(lambda ($port)
        (switch #,$expr
          ((u8? $u8) (put-u8 $port #,$expr))
          ((else $other)
            (syntax-error #'$expr
              (format "expected u8, got ~s, in" $other)))))))

  (define (block-apply $block $fn)
    ($fn $block))

  (define (block-bytevector-syntax $block)
    #`(lets
      #,@(map-with
        ($label (reverse (block-labels $block)))
        #`(#,(car $label) #,(cdr $label)))
      (call-with-bytevector-output-port
        (lambda ($port)
          (for-each
            (lambda ($put) ($put $port))
            (list #,@(reverse (block-puts $block))))))))
)
