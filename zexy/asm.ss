(library (zexy asm)
  (export
    asm asm? asm-stack
    empty-asm
    asm-bytevector
    asm-op
    asm-ops)
  (import
    (micascheme)
    (zexy math)
    (zexy env))

  (data (asm stack org env imports))

  (define (empty-asm)
    (asm (stack) 0 (empty-env) (stack)))

  (define (asm-with-stack $asm $stack)
    (asm $stack (asm-org $asm) (asm-env $asm) (asm-imports $asm)))

  (define (asm-with-org $asm $org)
    (asm (asm-stack $asm) $org (asm-env $asm) (asm-imports $asm)))

  (define (asm-with-env $asm $env)
    (asm (asm-stack $asm) (asm-org $asm) $env (asm-imports $asm)))

  (define (asm-with-imports $asm $imports)
    (asm (asm-stack $asm) (asm-org $asm) (asm-env $asm) $imports))

  (define (asm+bytevector $asm $bytevector)
    (fold-left asm-u8 $asm (bytevector->u8-list $bytevector)))

  (define (asm-bytevector $asm)
    (u8-list->bytevector
      (apply append
        (reverse
          (map
            (lambda ($syntax)
              (syntax-case $syntax ()
                (($op $value)
                  (case (datum $op)
                    ((db)
                      (list (asm-eval-u8 $asm #'$value)))
                    ((dw)
                      (lets
                        ($nm (asm-eval-u16 $asm #'$value))
                        (list (lsb $nm) (msb $nm))))
                    ((de)
                      (list (asm-eval-s8 $asm #'$value)))
                    (else (syntax-error $syntax))))))
            (asm-stack $asm))))))

  (define (asm-reduce $asm)
    (asm-with-stack $asm
      (map
        (lambda ($syntax)
          (syntax-case $syntax ()
            (($op $value)
              #`(#,#'$op
                #,(env-reduce (asm-env $asm) #'$value)))))
        (asm-stack $asm))))

  (define (asm-local $asm $ops)
    (lets
      ($local-asm
        (asm-reduce
          (asm-ops
            (asm-with-stack $asm (stack))
            $ops)))
      (fluent $asm
        (asm-with-stack
          (push-all
            (asm-stack $asm)
            (asm-stack $local-asm)))
        (asm-with-org (asm-org $local-asm))
        (asm-with-imports (asm-imports $local-asm)))))

  (define (asm-proc $asm $label $ops)
    (fluent $asm
      (asm+label $label)
      (asm-local $ops)))

  (define (asm-eval $asm $syntax)
    (env-eval (asm-env $asm) $syntax))

  (define (asm-eval-u8 $asm $syntax)
    (lets
      ($value (asm-eval $asm $syntax))
      (or
        (and
          (integer? $value)
          (>= $value #x00)
          (<= $value #xff)
          $value)
        (syntax-error $syntax))))

  (define (asm-eval-s8 $asm $syntax)
    (lets
      ($value (asm-eval $asm $syntax))
      (or
        (and
          (integer? $value)
          (>= $value -127)
          (<= $value 128)
          (band $value #xff))
        (syntax-error $syntax))))

  (define (asm-eval-u16 $asm $syntax)
    (lets
      ($value (asm-eval $asm $syntax))
      (or
        (and
          (integer? $value)
          (>= $value #x0000)
          (<= $value #xffff)
          $value)
        (syntax-error $syntax))))

  (define (asm+ $asm $syntax $size)
    (fluent $asm
      (asm-with-stack (push (asm-stack $asm) $syntax))
      (asm-with-org (nm+ (asm-org $asm) $size))))

  (define (asm-u8 $asm $u8)
    (fluent $asm
      (asm-with-stack (push (asm-stack $asm) #`(db #,$u8)))
      (asm-with-org (inc-nm (asm-org $asm)))))

  (define (asm-u16 $asm $u16)
    (fluent $asm
      (asm-with-stack (push (asm-stack $asm) #`(dw #,$u16)))
      (asm-with-org (nm+ (asm-org $asm) 2))))

  (define-syntax-rule (asm... $asm $u8 ...)
    (fold-left asm-u8 $asm (list $u8 ...)))

  (define (asm+label $asm $label)
    (asm-val $asm $label (asm-org $asm)))

  (define (asm-val $asm $label $value)
    (asm-with-env $asm
      (env-put (asm-env $asm) $label $value)))

  (define (asm-str $asm $str)
    (fold-left asm-u8 $asm (bytevector->u8-list (string->utf8 $str))))

  (define (asm-ops $asm $syntax-list)
    (fold-left asm-op $asm $syntax-list))

  (define (asm-op $asm $syntax)
    (or
      (syntax-case $syntax ()
        ($op (identifier? #'$op)
          (asm+label $asm (datum $op)))
        (($local $arg ...)
          (identifier-named? #'$local local)
          (asm-local $asm (syntax->list #'($arg ...))))
        (($proc $arg ...)
          (identifier-named? #'$proc proc)
          (syntax-case #'($arg ...) ()
            (($label $arg ...)
              (asm-proc $asm (label #'$label) (syntax->list #'($arg ...))))
            (else
              (syntax-error #'$proc "label expected after"))))
        (($op $arg ...)
          (case (datum $op)
            (else
              (lets
                ($proc
                  (case (datum $op)
                    ((defb db) asm-db...)
                    ((defw dw) asm-dw...)
                    ((defw dz) asm-dz...)
                    ((import) asm-import...)
                    (else #f)))
                (and $proc
                  (apply $proc
                    (cons $asm (syntax->list #'($arg ...)))))))))
        (else #f))
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

            ((cpd) (asm... $asm #xed #xa9))
            ((cpdr) (asm... $asm #xed #xb9))
            ((cpi) (asm... $asm #xed #xa1))
            ((cpir) (asm... $asm #xed #xb1))
            ((ldd) (asm... $asm #xed #xa8))
            ((lddr) (asm... $asm #xed #xb8))
            ((ldi) (asm... $asm #xed #xa0))
            ((ldir) (asm... $asm #xed #xb0))

            ((ind) (asm... $asm #xed #xaa))
            ((indr) (asm... $asm #xed #xba))
            ((ini) (asm... $asm #xed #xa2))
            ((inir) (asm... $asm #xed #xb2))

            ((outi) (asm... $asm #xed #xa3))
            ((otir) (asm... $asm #xed #xb3))
            ((outd) (asm... $asm #xed #xab))
            ((otdr) (asm... $asm #xed #xbb))

            ((swapnib) (asm... $asm #xed #x23))

            (else #f)))
        (($op $arg) (identifier? #'$op)
          (or
            (asm-aluop1 $asm #'$op #'$arg)
            (asm-rotop1 $asm #'$op #'$arg)
            (case (datum $op)
              ((org) (asm-org1 $asm #'$arg))

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
              ((in) (asm-in1 $asm #'$arg))
              (else #f))))
        (($op $lhs $rhs) (identifier? #'$op)
          (or
            (asm-bitop2 $asm #'$op #'$lhs #'$rhs)
            (case (datum $op)
              ((val) (asm-val2 $asm #'$lhs #'$rhs))

              ((ld) (asm-ld2 $asm #'$lhs #'$rhs))
              ((call) (asm-call2 $asm #'$lhs #'$rhs))
              ((jp) (asm-jp2 $asm #'$lhs #'$rhs))
              ((add) (asm-add2 $asm #'$lhs #'$rhs))
              ((adc) (asm-adc2 $asm #'$lhs #'$rhs))
              ((sbc) (asm-sbc2 $asm #'$lhs #'$rhs))
              ((ex) (asm-ex2 $asm #'$lhs #'$rhs))
              ((in) (asm-in2 $asm #'$lhs #'$rhs))
              ((out) (asm-out2 $asm #'$lhs #'$rhs))
              ((nextreg) (asm-nextreg2 $asm #'$lhs #'$rhs))
              (else #f))))
        (else #f))
      (syntax-error $syntax)))

  (define (asm-db... $asm . $args)
    (fold-left asm-db $asm $args))

  (define (asm-dz... $asm . $args)
    (asm-u8 (fold-left asm-db $asm $args) #x0))

  (define (asm-dw... $asm . $args)
    (fold-left asm-dw $asm $args))

  (define (asm-import... $asm . $args)
    (fold-left asm-import1 $asm $args))

  (define (asm-db $asm $arg)
    (lets
      ($chr (chr $arg))
      ($str (str $arg))
      (or
        (and $chr (asm-u8 $asm $chr))
        (and $str (asm-str $asm $str))
        (asm+ $asm #`(db #,$arg) 1))))

  (define (asm-de $asm $arg $offset)
    (switch (syntax->datum $arg)
      ((symbol? $symbol)
        (asm+ $asm #`(de (- #,$arg #,(asm-org $asm) #,$offset)) 1))
      ((else $other)
        (asm+ $asm #`(de #,$arg) 1))))

  (define (asm-dw $asm $arg)
    (asm+ $asm #`(dw #,$arg) 2))

  (define (asm-import1 $asm $arg)
    (switch (syntax->datum $arg)
      ((string? $string)
        (asm-import $asm $string))
      ((else $other)
        (syntax-error $arg "invalid import"))))

  (define (asm-import $asm $path)
    (cond
      ((member $path (asm-imports $asm)) $asm)
      (else
        (asm-ops
          (asm-with-imports $asm
            (push (asm-imports $asm) $path))
          (load-syntax-list $path)))))

  (define (asm-org1 $asm $arg)
    (asm-with-org $asm (env-eval (asm-env $asm) $arg)))

  (define (asm-val2 $asm $lhs $rhs)
    (and
      (identifier? $lhs)
      (asm-val $asm (syntax->datum $lhs) $rhs)))

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
      ($lhs-i (i $lhs))
      ($rhs-i (i $rhs))
      ($lhs-rr (rr-sp $lhs))
      ($rhs-rr (rr-sp $rhs))
      (or
        ; 8-bit
        (and (== $lhs a) (== $rhs (bc)) (asm-ld-a-ibc $asm))
        (and (== $lhs a) (== $rhs (de)) (asm-ld-a-ide $asm))
        (and $lhs-r (== $rhs (hl)) (asm-ld-r-ihl $asm $lhs-r))

        (and (== $lhs a) $rhs-i (asm-ld-a-inm $asm $rhs-i))
        (and $lhs-r $rhs-r (asm-ld-r-r $asm $lhs-r $rhs-r))
        (and $lhs-r (asm-ld-r-n $asm $lhs-r $rhs))

        (and (== $lhs (hl)) $rhs-r (asm-ld-ihl-r $asm $rhs-r))
        (and (== $lhs (hl)) (asm-ld-ihl-n $asm $rhs))

        (and (== $lhs (bc)) (== $rhs a) (asm-ld-ibc-a $asm))
        (and (== $lhs (de)) (== $rhs a) (asm-ld-ide-a $asm))
        (and $lhs-i (== $rhs a) (asm-ld-inm-a $asm $lhs-i))

        ; 16-bit
        (and (== $lhs sp) (== $rhs hl) (asm-ld-sp-hl $asm))
        (and (== $lhs hl) $rhs-i (asm-ld-hl-inm $asm $rhs-i))
        (and $lhs-rr $rhs-i (asm-ld-rr-inm $asm $lhs-rr $rhs-i))
        (and $lhs-rr (asm-ld-rr-nm $asm $lhs-rr $rhs))
        (and $lhs-i (== $rhs hl) (asm-ld-inm-hl $asm $lhs-i))
        (and $lhs-i $rhs-rr (asm-ld-inm-rr $asm $lhs-i $rhs-rr))
      )))

  (define (asm-ld-r-r $asm $r1 $r2)
    (asm... $asm
      (bor #b01000000 (shl $r1 3) $r2)))

  (define (asm-ld-r-n $asm $r $n)
    (fluent $asm
      (asm-db (bor #b00000110 (shl $r 3)))
      (asm-db $n)))

  (define (asm-ld-r-ihl $asm $r)
    (asm... $asm
      (bor #b01000110 (shl $r 3))))

  (define (asm-ld-ihl-r $asm $r)
    (asm... $asm
      (bor #b01110000 $r)))

  (define (asm-ld-ihl-n $asm $n)
    (fluent $asm
      (asm-db #b00110110)
      (asm-db $n)))

  (define (asm-ld-a-ibc $asm)
    (asm... $asm #b00001010))

  (define (asm-ld-a-ide $asm)
    (asm... $asm #b00011010))

  (define (asm-ld-a-inm $asm $nm)
    (fluent $asm
      (asm-db #b00111010)
      (asm-dw $nm)))

  (define (asm-ld-ibc-a $asm)
    (asm... $asm #b00000010))

  (define (asm-ld-ide-a $asm)
    (asm... $asm #b00010010))

  (define (asm-ld-inm-a $asm $nm)
    (fluent $asm
      (asm-db #b00110010)
      (asm-dw $nm)))

  (define (asm-ld-rr-nm $asm $rr $nm)
    (fluent $asm
      (asm-db (bor #b00000001 (shl $rr 4)))
      (asm-dw $nm)))

  (define (asm-ld-hl-inm $asm $inm)
    (fluent $asm
      (asm-db #x2a)
      (asm-dw $inm)))

  (define (asm-ld-rr-inm $asm $rr $inm)
    (fluent $asm
      (asm-db #xed)
      (asm-db (bor #b01001011 (shl $rr 4)))
      (asm-dw $inm)))

  (define (asm-ld-inm-hl $asm $inm)
    (fluent $asm
      (asm-db #x22)
      (asm-dw $inm)))

  (define (asm-ld-inm-rr $asm $inm $rr)
    (fluent $asm
      (asm-db #xed)
      (asm-db (bor #b01000011 (shl $rr 4)))
      (asm-dw $inm)))

  (define (asm-ld-sp-hl $asm)
    (asm... $asm #xf9))

  (define (asm-aluop1 $asm $lhs $rhs)
    (lets
      ($op (aluop $lhs))
      ($r (r-hl $rhs))
      (or
        (and $op $r (asm-alu-r $asm $op $r))
        (and $op (asm-alu-n $asm $op $rhs)))))

  (define (asm-alu-r $asm $op $r)
    (asm... $asm (bor #b10000000 (shl $op 3) $r)))

  (define (asm-alu-n $asm $op $n)
    (fluent $asm
      (asm-db (bor #b11000110 (shl $op 3)))
      (asm-db $n)))

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
      (or
        (and $lhs-hl? $rhs-rr (asm-add-hl-rr $asm $rhs-rr))

        (and (== $lhs hl) (== $rhs a) (asm... $asm #xed #b00110001))
        (and (== $lhs de) (== $rhs a) (asm... $asm #xed #b00110010))
        (and (== $lhs bc) (== $rhs a) (asm... $asm #xed #b00110011))

        (and (== $lhs hl)
          (fluent $asm
            (asm-db #xed)
            (asm-db #b00110100)
            (asm-dw $rhs)))
        (and (== $lhs de)
          (fluent $asm
            (asm-db #xed)
            (asm-db #b00110101)
            (asm-dw $rhs)))
        (and (== $lhs bc)
          (fluent $asm
            (asm-db #xed)
            (asm-db #b00110110)
            (asm-dw $rhs))))))

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
    (asm-call-nm $asm $arg))

  (define (asm-call2 $asm $lhs $rhs)
    (lets
      ($c (c $lhs))
      (and $c (asm-call-c-nm $asm $c $rhs))))

  (define (asm-call-nm $asm $nm)
    (fluent $asm
      (asm-db #xcd)
      (asm-dw $nm)))

  (define (asm-call-c-nm $asm $c $nm)
    (fluent $asm
      (asm-db (bor #b11000100 (shl $c 3)))
      (asm-dw $nm)))

  (define (asm-rst1 $asm $arg)
    (lets
      ($p (p $arg))
      (and $p (asm-rst-p $asm $p))))

  (define (asm-rst-p $asm $p)
    (asm... $asm (bor #b11000111 $p)))

  (define (asm-jp1 $asm $arg)
    (or
      (and (== $arg (hl)) (asm-jp-ihl $asm))
      (and (asm-jp-nm $asm $arg))))

  (define (asm-jp2 $asm $lhs $rhs)
    (lets
      ($c (c $lhs))
      (and $c (asm-jp-c-nm $asm $c $rhs))))

  (define (asm-jp-nm $asm $nm)
    (fluent $asm
      (asm-db #b11000011)
      (asm-dw $nm)))

  (define (asm-jp-ihl $asm)
    (asm... $asm #b11101001))

  (define (asm-jp-c-nm $asm $c $nm)
    (fluent $asm
      (asm-db (bor #b11000010 (shl $c 3)))
      (asm-dw $nm)))

  (define (asm-djnz1 $asm $arg)
    (asm-djnz-e $asm $arg))

  (define (asm-djnz-e $asm $e)
    (fluent $asm
      (asm-db #x10)
      (asm-de $e 1)))

  (define (asm-push1 $asm $arg)
    (lets
      ($rr (rr-af $arg))
      (or
        (and $rr (asm-push-rr $asm $rr))
        (asm-push-nm $asm $arg))))

  (define (asm-push-rr $asm $rr)
    (asm... $asm (bor #b11000101 (shl $rr 4))))

  (define (asm-push-nm $asm $nm)
    (fluent $asm
      (asm-db #xed)
      (asm-db #x8a)
      (asm-dw $nm)))

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

  (define (asm-in1 $asm $arg)
    (and (== $arg (c)) (asm... $asm #xed #x70)))

  (define (asm-in2 $asm $lhs $rhs)
    (lets
      ($lhs-r (r $lhs))
      ($rhs-i (i $rhs))
      (or
        (and $lhs-r (== $rhs (c))
          (fluent $asm
            (asm-db #xed)
            (asm-db (bor #b01000000 (shl $lhs-r 3)))))
        (and (== $lhs a) $rhs-i
          (fluent $asm
            (asm-db #xdb)
            (asm-db $rhs-i))))))

  (define (asm-out2 $asm $lhs $rhs)
    (lets
      ($lhs-i (i $lhs))
      ($rhs-r (r $rhs))
      (or
        (and (== $lhs (c)) $rhs-r
          (fluent $asm
            (asm-db #xed)
            (asm-db (bor #b01000001 (shl $rhs-r 3)))))
        (and (== $lhs (c)) (== $rhs 0)
          (fluent $asm
            (asm-db #xed)
            (asm-db #x71)))
        (and $lhs-i (== $rhs a)
          (fluent $asm
            (asm-db #xd3)
            (asm-db $lhs-i))))))

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

  (define (asm-nextreg2 $asm $lhs $rhs)
    (or
      (and (== $rhs a)
        (fluent $asm
          (asm-db #xed)
          (asm-db #x92)
          (asm-db $lhs)))
      (fluent $asm
          (asm-db #xed)
          (asm-db #x91)
          (asm-db $lhs)
          (asm-db $rhs))))

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

  (define (i $syntax)
    (syntax-case $syntax ()
      (($op) #'$op)
      (else #f)))

  (define (label $syntax)
    (switch (syntax->datum $syntax)
      ((symbol? $symbol) $symbol)
      ((else _) (syntax-error $syntax "invalid label"))))

  (define (str $syntax)
    (switch-opt (syntax->datum $syntax)
      ((string? $string) $string)))

  (define (chr $syntax)
    (switch-opt (syntax->datum $syntax)
      ((char? $char) (char->integer $char))))

  (define (p $syntax)
    (lets
      ($p (syntax->datum $syntax))
      (and
        (integer? $p)
        (zero? (band $p #b11000111))
        $p)))

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
