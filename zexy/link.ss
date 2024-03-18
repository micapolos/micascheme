(library (zexy link)
  (export link)
  (import
    (micascheme)
    (only (zexy ops) label org db dw))

  (define (link $ops $cont)
    (let ()
      (define $entries (stack))
      (define $linked (stack))
      (define $pc 0)
      (for-each
        (lambda ($op)
          (syntax-case $op (label org db dw)
            ((label $name) (identifier? #'$name)
              (set! $entries (push $entries #`($name #,$pc))))
            ((org $expr) (number? (datum $expr))
              (set! $pc (datum $expr)))
            ((db $expr)
              (run
                (set! $linked (push $linked $op))
                (set! $pc (+ $pc 1))))
            ((dw $expr)
              (run
                (set! $linked (push $linked $op))
                (set! $pc (+ $pc 2))))))
        $ops)
      #`(lets
        #,@(reverse $entries)
        #,($cont (reverse $linked)))))
)
