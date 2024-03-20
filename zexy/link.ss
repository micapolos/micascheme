(library (zexy link)
  (export link)
  (import
    (micascheme)
    (only (zexy ops) label align org db dw ds))

  (define link
    (case-lambda
      (($ops $cont)
        (link (lambda ($id $key) #f) $ops $cont))
      (($lookup $ops $cont)
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
                    ($new-pc (bitwise-align $pc (datum $expr)))
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
          #`(let
            (#,@(reverse $entries))
            #,($cont (reverse $linked)))))))

  (define (size? $datum)
    (and (integer? $datum) (nonnegative? $datum)))
)
