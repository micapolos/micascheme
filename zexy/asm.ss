(library (zexy asm)
  (export
    push-op
    push-ops)
  (import
    (micascheme))

  (define-syntax ior (identifier-syntax bitwise-ior))
  (define-syntax shl (identifier-syntax bitwise-arithmetic-shift-left))
  (define-syntax shr (identifier-syntax bitwise-arithmetic-shift-right))

  (define (push-ops $stack $syntax-list)
    (fold-left push-op $stack $syntax-list))

  (define (push-op $stack $syntax)
    (or
      (syntax-case $syntax ()
        (($op) (identifier? #'$op)
          (case (datum $op)
            ((nop) (push-nop $stack))
            ((ret) (push-ret $stack))
            (else #f)))
        (($op $lhs $rhs) (identifier? #'$op)
          (case (datum $op)
            ((ld)
              (push-ld $stack #'$lhs #'$rhs))
            (else #f)))
        (else #f))
      (syntax-error $syntax)))

  (define (push-nop $stack)
    (push $stack #x0))

  (define (push-ret $stack)
    (push $stack #xc9))

  (define (push-ld $stack $lhs $rhs)
    (or
      (push-ld-r-r $stack $lhs $rhs)))

  (define (push-ld-r-r $stack $lhs $rhs)
    (lets
      ($r1 (r $lhs))
      ($r2 (r $rhs))
      (and $r1 $r2
        (push $stack
          (ior #b01000000 (shl $r1 3) $r2)))))

  (define (r $syntax)
    (syntax-case $syntax ()
      ($r
        (identifier? #'$r)
        (case (datum $r)
          ((b) #b000)
          ((c) #b001)
          ((d) #b010)
          ((e) #b011)
          ((h) #b100)
          ((l) #b101)
          ((a) #b111)
          (else #f)))))
)
