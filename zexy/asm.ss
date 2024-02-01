(library (zexy asm)
  (export
    asm asm? asm-stack
    empty-asm
    asm-bytevector
    asm-op
    asm-ops)
  (import
    (micascheme)
    (zexy bin))

  (data (asm stack))

  (define (empty-asm)
    (asm (stack)))

  (define (asm-bytevector $asm)
    (u8-list->bytevector (reverse (asm-stack $asm))))

  (define-syntax-rule (asm... $asm $u8 ...)
    (asm (push... (asm-stack $asm) $u8 ...)))

  (define (asm-ops $asm $syntax-list)
    (fold-left asm-op $asm $syntax-list))

  (define (asm-op $asm $syntax)
    (or
      (syntax-case $syntax ()
        (($op) (identifier? #'$op)
          (case (datum $op)
            ((nop) (asm-nop $asm))
            ((ret) (asm-ret $asm))
            (else #f)))
        (($op $lhs $rhs) (identifier? #'$op)
          (case (datum $op)
            ((ld) (asm-ld $asm #'$lhs #'$rhs))
            (else #f)))
        (else #f))
      (syntax-error $syntax)))

  (define (asm-nop $asm)
    (asm... $asm #x0))

  (define (asm-ret $asm)
    (asm... $asm #xc9))

  (define (asm-ld $stack $lhs $rhs)
    (lets
      ($lhs-r (r $lhs))
      ($rhs-r (r $rhs))
      ($rhs-n (n $rhs))
      ($lhs-ihl? (ihl? $lhs))
      ($rhs-ihl? (ihl? $rhs))
      (or
        (and $lhs-r $rhs-r (asm-ld-r-r $stack $lhs-r $rhs-r))
        (and $lhs-r $rhs-n (asm-ld-r-n $stack $lhs-r $rhs-n))
        (and $lhs-r $rhs-ihl? (asm-ld-r-ihl $stack $lhs-r)))))

  (define (asm-ld-r-r $asm $r1 $r2)
    (asm... $asm
      (ior #b01000000 (shl $r1 3) $r2)))

  (define (asm-ld-r-n $asm $r $n)
    (asm... $asm
      (ior #b00000110 (shl $r 3))
      $n))

  (define (asm-ld-r-ihl $asm $r)
    (asm... $asm
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
