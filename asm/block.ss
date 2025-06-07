(library (asm block)
  (export
    block block? block-size block-put-syntax-stack-proc
    block-with
    empty-block
    block-append
    block->put-proc-syntax
    u8-block)
  (import (micascheme))

  (data (block size put-syntax-stack-proc))

  (define-rule-syntax (block-with ($size $port-identifier) body ...)
    (block $size
      (lambda ($port-identifier)
        (stack #'body ...))))

  (define (empty-block)
    (block-with (0 _)))

  (define (block-append . $blocks)
    (block
      (apply + (map block-size $blocks))
      (lambda ($port-identifier)
        (apply append
          (map
            (lambda ($put-syntax-stack-proc) ($put-syntax-stack-proc $port-identifier))
            (map block-put-syntax-stack-proc (reverse $blocks)))))))

  (define-rule-syntax (u8-block u8 ...)
    (lets
      ($u8s (list #'u8 ...))
      (block (length $u8s)
        (lambda ($port)
          (stack #`(put-u8 #,$port #,u8) ...)))))

  (define (block->put-proc-syntax $block)
    #`(lambda ($port)
      #,@(lets
        ($syntaxes (reverse ((block-put-syntax-stack-proc $block) #'$port)))
        (if (null? $syntaxes) (list #'(void)) $syntaxes))))
)
