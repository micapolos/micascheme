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
        (($op $arg) (identifier? #'$op)
          (case (datum $op)
            ((call) (asm-call1 $asm #'$arg))
            ((ret) (asm-ret1 $asm #'$arg))
            ((rst) (asm-rst1 $asm #'$arg))
            (else #f)))
        (($op $lhs $rhs) (identifier? #'$op)
          (case (datum $op)
            ((ld) (asm-ld2 $asm #'$lhs #'$rhs))
            ((call) (asm-call2 $asm #'$lhs #'$rhs))
            (else #f)))
        (else #f))
      (syntax-error $syntax)))

  (define (asm-nop $asm)
    (asm... $asm #b00000000))

  (define (asm-ret $asm)
    (asm... $asm #b11001001))

  (define (asm-ret1 $asm $arg)
    (lets
      ($c (c $arg))
      (and $c (asm-ret-c $asm $c))))

  (define (asm-ret-c $asm $c)
    (asm... $asm (bor #b11000000 (shl $c 3))))

  (define (asm-ld2 $asm $lhs $rhs)
    (lets
      ($lhs-r (r $lhs))
      ($rhs-r (r $rhs))
      ($rhs-n (n $rhs))
      ($rhs-nm (nm $rhs))
      ($lhs-inm (inm $lhs))
      ($rhs-inm (inm $rhs))
      (or
        (and $lhs-r $rhs-r (asm-ld-r-r $asm $lhs-r $rhs-r))
        (and $lhs-r $rhs-n (asm-ld-r-n $asm $lhs-r $rhs-n))
        (and $lhs-r (== $rhs (hl)) (asm-ld-r-ihl $asm $lhs-r))
        (and (== $lhs (hl)) $rhs-r (asm-ld-ihl-r $asm $rhs-r))
        (and (== $lhs (hl)) $rhs-n (asm-ld-ihl-n $asm $rhs-n))
        (and (== $lhs (hl)) $rhs-n (asm-ld-ihl-n $asm $rhs-n))
        (and (== $lhs a) (== $rhs (bc)) (asm-ld-a-ibc $asm))
        (and (== $lhs a) (== $rhs (de)) (asm-ld-a-ide $asm))
        (and (== $lhs a) $rhs-inm (asm-ld-a-inm $asm $rhs-inm))
        (and (== $lhs (bc)) (== $rhs a) (asm-ld-ibc-a $asm))
        (and (== $lhs (de)) (== $rhs a) (asm-ld-ide-a $asm))
        (and $lhs-inm (== $rhs a) (asm-ld-inm-a $asm $lhs-inm))
        )))

  (define (asm-ld-r-r $asm $r1 $r2)
    (asm... $asm
      (bor #b01000000 (shl $r1 3) $r2)))

  (define (asm-ld-r-n $asm $r $n)
    (asm... $asm
      (bor #b00000110 (shl $r 3))
      $n))

  (define (asm-ld-r-ihl $asm $r)
    (asm... $asm
      (bor #b01000110 (shl $r 3))))

  (define (asm-ld-ihl-r $asm $r)
    (asm... $asm
      (bor #b01110000 $r)))

  (define (asm-ld-ihl-n $asm $n)
    (asm... $asm #b00110110 $n))

  (define (asm-ld-a-ibc $asm)
    (asm... $asm #b00001010))

  (define (asm-ld-a-ide $asm)
    (asm... $asm #b00011010))

  (define (asm-ld-a-inm $asm $nm)
    (asm... $asm #b00111010 (lsb $nm) (msb $nm)))

  (define (asm-ld-ibc-a $asm)
    (asm... $asm #b00000010))

  (define (asm-ld-ide-a $asm)
    (asm... $asm #b00010010))

  (define (asm-ld-inm-a $asm $nm)
    (asm... $asm #b00110010 (lsb $nm) (msb $nm)))

  (define (asm-call1 $asm $arg)
    (lets
      ($nm (nm $arg))
      (and $nm (asm-call-nm $asm $nm))))

  (define (asm-call2 $asm $lhs $rhs)
    (lets
      ($c (c $lhs))
      ($nm (nm $rhs))
      (and $nm (asm-call-c-nm $asm $c $nm))))

  (define (asm-call-nm $asm $nm)
    (asm... $asm #b11001101 (lsb $nm) (msb $nm)))

  (define (asm-call-c-nm $asm $c $nm)
    (asm... $asm (bor #b11000100 (shl $c 3)) (lsb $nm) (msb $nm)))

  (define (asm-rst1 $asm $arg)
    (lets
      ($p (p $arg))
      (and $p (asm-rst-p $asm $p))))

  (define (asm-rst-p $asm $p)
    (asm... $asm (bor #b11000111 $p)))

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

  (define (c $syntax)
    (case (syntax->datum $syntax)
      ((nz) #b000)
      ((z) #b001)
      ((nc) #b010)
      ((c) #b011)
      ((po) #b100)
      ((pe) #b101)
      ((p) #b110)
      ((m) #b111)
      (else #f)))

  (define (n $syntax)
    (lets
      ($datum (syntax->datum $syntax))
      (and
        (integer? $datum)
        (>= $datum #x00)
        (<= $datum #xff)
        $datum)))

  (define (p $syntax)
    (lets
      ($n (n $syntax))
      (and $n
        (zero? (band $n #b11000111))
        $n)))

  (define (nm $syntax)
    (lets
      ($datum (syntax->datum $syntax))
      (and
        (integer? $datum)
        (>= $datum #x0000)
        (<= $datum #xffff)
        $datum)))

  (define (inm $syntax)
    (syntax-case $syntax ()
      (($op) (nm #'$op))
      (else #f)))

  (define-syntax-rule (== $syntax $datum)
    (equal? (syntax->datum $syntax) '$datum))
)
