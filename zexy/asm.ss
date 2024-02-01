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
            ((ld) (push-ld $stack #'$lhs #'$rhs))
            (else #f)))
        (else #f))
      (syntax-error $syntax)))

  (define (push-nop $stack)
    (push $stack #x0))

  (define (push-ret $stack)
    (push $stack #xc9))

  (define (push-ld $stack $lhs $rhs)
    (lets
      ($lhs-r (r $lhs))
      ($rhs-r (r $rhs))
      ($rhs-n (n $rhs))
      ($lhs-ihl? (ihl? $lhs))
      ($rhs-ihl? (ihl? $rhs))
      (or
        (and $lhs-r $rhs-r (push-ld-r-r $stack $lhs-r $rhs-r))
        (and $lhs-r $rhs-n (push-ld-r-n $stack $lhs-r $rhs-n))
        (and $lhs-r $rhs-ihl? (push-ld-r-ihl $stack $lhs-r)))))

  (define (push-ld-r-r $stack $r1 $r2)
    (push $stack
      (ior #b01000000 (shl $r1 3) $r2)))

  (define (push-ld-r-n $stack $r $n)
    (push... $stack
      (ior #b00000110 (shl $r 3))
      $n))

  (define (push-ld-r-ihl $stack $r)
    (push $stack
      (ior #b01000110 (shl $r 3))))

  (define (r $syntax)
    (case (syntax->datum $syntax)
      ((b) #b000)
      ((c) #b001)
      ((d) #b010)
      ((e) #b011)
      ((h) #b100)
      ((l) #b101)
      ((a) #b111)
      (else #f)))

  (define (n $syntax)
    (lets
      ($datum (syntax->datum $syntax))
      (and
        (integer? $datum)
        (>= $datum #x00)
        (<= $datum #xff)
        $datum)))

  (define (ihl? $syntax)
    (syntax-case $syntax ()
      (($op) (identifier-named? #'$op hl))
      (else #f)))
)
