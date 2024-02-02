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
            ((ret) (asm... $asm #xc9))
            ((daa) (asm... $asm #x27))
            ((cpl) (asm... $asm #x2f))
            ((neg) (asm... $asm #xed #x44))
            ((ccf) (asm... $asm #x3f))
            ((scf) (asm... $asm #x37))
            ((nop) (asm... $asm #x00))
            ((halt) (asm... $asm #x76))
            ((di) (asm... $asm #xf3))
            ((ei) (asm... $asm #xfb))
            ((exx) (asm... $asm #xd9))
            ((rla) (asm... $asm #x17))
            ((rlca) (asm... $asm #x07))
            ((rra) (asm... $asm #x1f))
            ((rrca) (asm... $asm #x0f))
            ((rld) (asm... $asm #xed #x6f))
            ((rrd) (asm... $asm #xed #x67))
            (else #f)))
        (($op $arg) (identifier? #'$op)
          (or
            (asm-aluop1 $asm #'$op #'$arg)
            (asm-rotop1 $asm #'$op #'$arg)
            (case (datum $op)
              ((call) (asm-call1 $asm #'$arg))
              ((ret) (asm-ret1 $asm #'$arg))
              ((rst) (asm-rst1 $asm #'$arg))
              ((jp) (asm-jp1 $asm #'$arg))
              ((djnz) (asm-djnz1 $asm #'$arg))
              ((push) (asm-push1 $asm #'$arg))
              ((pop) (asm-pop1 $asm #'$arg))
              ((inc) (asm-inc1 $asm #'$arg))
              ((dec) (asm-dec1 $asm #'$arg))
              ((im) (asm-im1 $asm #'$arg))
              (else #f))))
        (($op $lhs $rhs) (identifier? #'$op)
          (or
            (asm-bitop2 $asm #'$op #'$lhs #'$rhs)
            (case (datum $op)
              ((ld) (asm-ld2 $asm #'$lhs #'$rhs))
              ((call) (asm-call2 $asm #'$lhs #'$rhs))
              ((jp) (asm-jp2 $asm #'$lhs #'$rhs))
              ((add) (asm-add2 $asm #'$lhs #'$rhs))
              ((adc) (asm-adc2 $asm #'$lhs #'$rhs))
              ((sbc) (asm-sbc2 $asm #'$lhs #'$rhs))
              ((ex) (asm-ex2 $asm #'$lhs #'$rhs))
              (else #f))))
        (else #f))
      (syntax-error $syntax)))

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
      ($lhs-rr (rr-sp $lhs))
      ($rhs-rr (rr-sp $rhs))
      (or
        ; 8-bit
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

        ; 16-bit
        (and $lhs-rr $rhs-nm (asm-ld-rr-nm $asm $lhs-rr $rhs-nm))
        (and (== $lhs hl) $rhs-inm (asm-ld-hl-inm $asm $rhs-inm))
        (and $lhs-rr $rhs-inm (asm-ld-rr-inm $asm $lhs-rr $rhs-inm))
        (and $lhs-inm (== $rhs hl) (asm-ld-inm-hl $asm $lhs-inm))
        (and $lhs-inm $rhs-rr (asm-ld-inm-rr $asm $lhs-inm $rhs-rr))
        (and (== $lhs sp) (== $rhs hl) (asm-ld-sp-hl $asm))
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

  (define (asm-ld-rr-nm $asm $rr $nm)
    (asm... $asm
      (bor #b00000001 (shl $rr 4))
      (lsb $nm)
      (msb $nm)))

  (define (asm-ld-hl-inm $asm $inm)
    (asm... $asm
      #x2a
      (lsb $inm)
      (msb $inm)))

  (define (asm-ld-rr-inm $asm $rr $inm)
    (asm... $asm
      #xed
      (bor #b01001011 (shl $rr 4))
      (lsb $inm)
      (msb $inm)))

  (define (asm-ld-inm-hl $asm $inm)
    (asm... $asm
      #x22
      (lsb $inm)
      (msb $inm)))

  (define (asm-ld-inm-rr $asm $inm $rr)
    (asm... $asm
      #xed
      (bor #b01000011 (shl $rr 4))
      (lsb $inm)
      (msb $inm)))

  (define (asm-ld-sp-hl $asm)
    (asm... $asm #xf9))

  (define (asm-aluop1 $asm $lhs $rhs)
    (lets
      ($op (aluop $lhs))
      ($r (r-hl $rhs))
      ($n (n $rhs))
      (or
        (and $op $r (asm-alu-r $asm $op $r))
        (and $op $n (asm-alu-n $asm $op $n)))))

  (define (asm-alu-r $asm $op $r)
    (asm... $asm (bor #b10000000 (shl $op 3) $r)))

  (define (asm-alu-n $asm $op $n)
    (asm... $asm (bor #b11000110 (shl $op 3)) $n))

  (define (asm-inc1 $asm $arg)
    (lets
      ($r (r-hl $arg))
      ($rr (rr-sp $arg))
      (or
        (and $r (asm-inc-r $asm $r))
        (and $rr (asm-inc-rr $asm $rr)))))

  (define (asm-inc-r $asm $r)
    (asm... $asm (bor #b00000100 (shl $r 3))))

  (define (asm-inc-rr $asm $rr)
    (asm... $asm (bor #b00000011 (shl $rr 4))))

  (define (asm-dec1 $asm $arg)
    (lets
      ($r (r-hl $arg))
      ($rr (rr-sp $arg))
      (or
        (and $r (asm-dec-r $asm $r))
        (and $rr (asm-dec-rr $asm $rr)))))

  (define (asm-dec-r $asm $r)
    (asm... $asm (bor #b00000101 (shl $r 3))))

  (define (asm-dec-rr $asm $rr)
    (asm... $asm (bor #b00001011 (shl $rr 4))))

  (define (asm-add2 $asm $lhs $rhs)
    (lets
      ($lhs-hl? (== $lhs hl))
      ($rhs-rr (rr-sp $rhs))
      (and $lhs-hl? $rhs-rr (asm-add-hl-rr $asm $rhs-rr))))

  (define (asm-add-hl-rr $asm $rr)
    (asm... $asm (bor #b00001001 (shl $rr 4))))

  (define (asm-adc2 $asm $lhs $rhs)
    (lets
      ($lhs-hl? (== $lhs hl))
      ($rhs-rr (rr-sp $rhs))
      (and $lhs-hl? $rhs-rr (asm-adc-hl-rr $asm $rhs-rr))))

  (define (asm-adc-hl-rr $asm $rr)
    (asm... $asm
      #xed
      (bor #b01001010 (shl $rr 4))))

  (define (asm-sbc2 $asm $lhs $rhs)
    (lets
      ($lhs-hl? (== $lhs hl))
      ($rhs-rr (rr-sp $rhs))
      (and $lhs-hl? $rhs-rr (asm-sbc-hl-rr $asm $rhs-rr))))

  (define (asm-sbc-hl-rr $asm $rr)
    (asm... $asm
      #xed
      (bor #b01000010 (shl $rr 4))))

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

  (define (asm-jp1 $asm $arg)
    (lets
      ($nm (nm $arg))
      (or
        (and $nm (asm-jp-nm $asm $nm))
        (and (== $arg (hl)) (asm-jp-ihl $asm)))))

  (define (asm-jp2 $asm $lhs $rhs)
    (lets
      ($c (c $lhs))
      ($nm (nm $rhs))
      (and $nm (asm-jp-c-nm $asm $c $nm))))

  (define (asm-jp-nm $asm $nm)
    (asm... $asm #b11000011 (lsb $nm) (msb $nm)))

  (define (asm-jp-ihl $asm)
    (asm... $asm #b11101001))

  (define (asm-jp-c-nm $asm $c $nm)
    (asm... $asm (bor #b11000010 (shl $c 3)) (lsb $nm) (msb $nm)))

  (define (asm-djnz1 $asm $arg)
    (lets
      ($e (n $arg))
      (and $e (asm-djnz-e $asm $e))))

  (define (asm-djnz-e $asm $e)
    (asm... $asm #b00010000 $e))

  (define (asm-push1 $asm $arg)
    (lets
      ($rr (rr-af $arg))
      ($nm (nm $arg))
      (or
        (and $rr (asm-push-rr $asm $rr))
        (and $nm (asm-push-nm $asm $nm)))))

  (define (asm-push-rr $asm $rr)
    (asm... $asm (bor #b11000101 (shl $rr 4))))

  (define (asm-push-nm $asm $nm)
    (asm... $asm #xed #x8a (lsb $nm) (msb $nm)))

  (define (asm-pop1 $asm $arg)
    (lets
      ($rr (rr-af $arg))
      (and $rr (asm-pop-rr $asm $rr))))

  (define (asm-pop-rr $asm $rr)
    (asm... $asm (bor #b11000001 (shl $rr 4))))

  (define (asm-im1 $asm $arg)
    (case (syntax->datum $arg)
      ((0) (asm-im-0 $asm))
      ((1) (asm-im-1 $asm))
      ((2) (asm-im-2 $asm))
      (else #f)))

  (define (asm-im-0 $asm) (asm... $asm #xed #x46))
  (define (asm-im-1 $asm) (asm... $asm #xed #x56))
  (define (asm-im-2 $asm) (asm... $asm #xed #x5e))

  (define (asm-ex2 $asm $lhs $rhs)
    (or
      (and (== $lhs af) (== $rhs af2) (asm-ex-af-af2 $asm))
      (and (== $lhs de) (== $rhs hl) (asm-ex-de-hl $asm))
      (and (== $lhs (sp)) (== $rhs hl) (asm-ex-isp-hl $asm))))

  (define (asm-ex-af-af2 $asm) (asm... $asm #x08))
  (define (asm-ex-de-hl $asm) (asm... $asm #xeb))
  (define (asm-ex-isp-hl $asm) (asm... $asm #xe3))

  (define (asm-bitop2 $asm $op $lhs $rhs)
    (lets
      ($op (bitop $op))
      ($bit (bit $lhs))
      ($r (r-hl $rhs))
      (and $op $bit $r (asm-bit-op-bit-r $asm $op $bit $r))))

  (define (asm-bit-op-bit-r $asm $op $bit $r)
    (asm... $asm
      #xcb
      (bor (shl $op 6) (shl $bit 3) $r)))

  (define (bit $syntax)
    (lets
      ($datum (syntax->datum $syntax))
      (and
        (integer? $datum)
        (>= $datum 0)
        (<= $datum 7)
        $datum)))

  (define (asm-rotop1 $asm $op $arg)
    (lets
      ($op (rotop $op))
      ($r (r-hl $arg))
      (and $op $r (asm-rot-op-r $asm $op $r))))

  (define (asm-rot-op-r $asm $op $r)
    (asm... $asm
      #xcb
      (bor (shl $op 3) $r)))

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

  (define (r-hl $syntax)
    (or
      (r $syntax)
      (and (== $syntax (hl)) #b110)))

  (define (rr-af $syntax)
    (case (syntax->datum $syntax)
      ((bc) #b00)
      ((de) #b01)
      ((hl) #b10)
      ((af) #b11)
      (else #f)))

  (define (rr-sp $syntax)
    (case (syntax->datum $syntax)
      ((bc) #b00)
      ((de) #b01)
      ((hl) #b10)
      ((sp) #b11)
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
        (>= $datum -127)
        (<= $datum 255)
        (band $datum #xff))))

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

  (define (aluop $syntax)
    (case (syntax->datum $syntax)
      ((add) #b000)
      ((adc) #b001)
      ((sub) #b010)
      ((sbc) #b011)
      ((and) #b100)
      ((xor) #b101)
      ((or) #b110)
      ((cp) #b111)
      (else #f)))

  (define (bitop $syntax)
    (case (syntax->datum $syntax)
      ((bit) #b01)
      ((set) #b11)
      ((res) #b10)
      (else #f)))

  (define (rotop $syntax)
    (case (syntax->datum $syntax)
      ((rlc) #b000)
      ((rrc) #b001)
      ((rl) #b010)
      ((rr) #b011)
      ((sla) #b100)
      ((sra) #b101)
      ((sli) #b110)
      ((srl) #b111)
      (else #f)))

  (define-syntax-rule (== $syntax $datum)
    (equal? (syntax->datum $syntax) '$datum))
)
