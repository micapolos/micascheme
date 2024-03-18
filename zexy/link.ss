(library (zexy link)
  (export link)
  (import
    (micascheme)
    (only (zexy ops) label align org db dw ds))

  (define (link $ops $cont)
    (let ()
      (define $entries (stack))
      (define $linked (stack))
      (define $pc 0)
      (for-each
        (lambda ($op)
          (syntax-case $op (label align org db dw ds)
            ((label $name) (identifier? #'$name)
              (set! $entries (push $entries #`($name #,$pc))))
            ((align $expr) (size? (datum $expr))
              (lets
                ($align (datum $expr))
                ($mask (- $align 1))
                ($new-pc (bitwise-and (+ $pc $mask) (bitwise-not $mask)))
                ($slack (- $new-pc $pc))
                (run
                  (if (not (zero? $slack))
                    (set! $linked (push $linked #`(ds #,$slack)))
                    (set! $pc $new-pc)))))
            ((org $expr) (size? (datum $expr))
              (set! $pc (datum $expr)))
            ((db $expr)
              (run
                (set! $linked (push $linked $op))
                (set! $pc (+ $pc 1))))
            ((dw $expr)
              (run
                (set! $linked (push $linked $op))
                (set! $pc (+ $pc 2))))
            ((ds $expr) (size? (datum $expr))
              (run
                (set! $linked (push $linked $op))
                (set! $pc (+ $pc (datum $expr)))))))
        $ops)
      #`(lets
        #,@(reverse $entries)
        #,($cont (reverse $linked)))))

  (define (size? $datum)
    (and (integer? $datum) (nonnegative? $datum)))
)
