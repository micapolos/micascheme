(library (zexy z80)
  (export
    z80 z80? make-z80
    z80-run
    mem-ref mem-set!)
  (import
    (except (micascheme) define)
    (only (scheme) define)
    (zexy unit))

  (define-unit z80
    (fields
      (unsigned-8 a)
      (unsigned-8 f)
      (unsigned-8 b)
      (unsigned-8 c)
      (unsigned-8 d)
      (unsigned-8 e)
      (unsigned-8 h)
      (unsigned-8 l)

      (unsigned-8 a2)
      (unsigned-8 f2)
      (unsigned-8 b2)
      (unsigned-8 c2)
      (unsigned-8 d2)
      (unsigned-8 e2)
      (unsigned-8 h2)
      (unsigned-8 l2)

      (unsigned-16 pc)
      (unsigned-16 sp)
      (unsigned-16 ix)
      (unsigned-16 iy)))

  (define (z80-run $z80 $mem $in $out)
    (define-syntax-rule (define-macro $body ...)
      (define-syntax-rule $body ...))

    (define $a (z80-a $z80))
    (define $f (z80-f $z80))
    (define $b (z80-b $z80))
    (define $c (z80-c $z80))
    (define $d (z80-d $z80))
    (define $e (z80-e $z80))
    (define $h (z80-h $z80))
    (define $l (z80-l $z80))

    (define $a2 (z80-a2 $z80))
    (define $f2 (z80-f2 $z80))
    (define $b2 (z80-b2 $z80))
    (define $c2 (z80-c2 $z80))
    (define $d2 (z80-d2 $z80))
    (define $e2 (z80-e2 $z80))
    (define $h2 (z80-h2 $z80))
    (define $l2 (z80-l2 $z80))

    (define $pc (z80-pc $z80))
    (define $sp (z80-sp $z80))
    (define $ix (z80-ix $z80))
    (define $iy (z80-iy $z80))

    (define $op 0)
    (define $n 0)
    (define $m 0)

    (define $halt? #f)

    (define-macro (fetch-op)
      (begin
        (set! $op (mem-ref $mem $pc))
        (set! $pc (fx+ $pc 1))))

    (do ()
      ($halt?
        (set-z80-a! $z80 $a)
        (set-z80-f! $z80 $f)
        (set-z80-b! $z80 $b)
        (set-z80-c! $z80 $c)
        (set-z80-d! $z80 $d)
        (set-z80-e! $z80 $e)
        (set-z80-h! $z80 $h)
        (set-z80-l! $z80 $l)

        (set-z80-a2! $z80 $a2)
        (set-z80-f2! $z80 $f2)
        (set-z80-b2! $z80 $b2)
        (set-z80-c2! $z80 $c2)
        (set-z80-d2! $z80 $d2)
        (set-z80-e2! $z80 $e2)
        (set-z80-h2! $z80 $h2)
        (set-z80-l2! $z80 $l2)

        (set-z80-pc! $z80 $pc)
        (set-z80-sp! $z80 $sp)
        (set-z80-ix! $z80 $ix)
        (set-z80-iy! $z80 $iy))

      (fetch-op)

      (case $op
        ((#x00) (nop))
        ((#x01) (ld-rr-nm $b $c $n $m $mem $pc))
        ((#x06) (ld-r-n $b $n $mem $pc))
        ((#x0e) (ld-r-n $c $n $mem $pc))

        ((#x11) (ld-rr-nm $d $e $n $m $mem $pc))
        ((#x16) (ld-r-n $d $n $mem $pc))
        ((#x1e) (ld-r-n $e $n $mem $pc))

        ((#x21) (ld-rr-nm $h $l $n $m $mem $pc))
        ((#x26) (ld-r-n $h $n $mem $pc))
        ((#x2e) (ld-r-n $l $n $mem $pc))

        ((#x31) (ld-sp-nm $sp $n $m $mem $pc))
        ((#x36) (ld-ihl-n $h $l $n $mem $pc))
        ((#x3e) (ld-r-n $a $n $mem $pc))

        ((#x40) (ld-r-r $b $b))
        ((#x41) (ld-r-r $b $c))
        ((#x42) (ld-r-r $b $d))
        ((#x43) (ld-r-r $b $e))
        ((#x44) (ld-r-r $b $h))
        ((#x45) (ld-r-r $b $l))
        ((#x46) (ld-r-ihl $b $mem $h $l))
        ((#x47) (ld-r-r $b $a))

        ((#x48) (ld-r-r $c $b))
        ((#x49) (ld-r-r $c $c))
        ((#x4a) (ld-r-r $c $d))
        ((#x4b) (ld-r-r $c $e))
        ((#x4c) (ld-r-r $c $h))
        ((#x4d) (ld-r-r $c $l))
        ((#x4e) (ld-r-ihl $c $mem $h $l))
        ((#x4f) (ld-r-r $c $a))

        ((#x50) (ld-r-r $d $b))
        ((#x51) (ld-r-r $d $c))
        ((#x52) (ld-r-r $d $d))
        ((#x53) (ld-r-r $d $e))
        ((#x54) (ld-r-r $d $h))
        ((#x55) (ld-r-r $d $l))
        ((#x56) (ld-r-ihl $d $mem $h $l))
        ((#x57) (ld-r-r $d $a))

        ((#x58) (ld-r-r $e $b))
        ((#x59) (ld-r-r $e $c))
        ((#x5a) (ld-r-r $e $d))
        ((#x5b) (ld-r-r $e $e))
        ((#x5c) (ld-r-r $e $h))
        ((#x5d) (ld-r-r $e $l))
        ((#x5e) (ld-r-ihl $e $mem $h $l))
        ((#x5f) (ld-r-r $e $a))

        ((#x60) (ld-r-r $h $b))
        ((#x61) (ld-r-r $h $c))
        ((#x62) (ld-r-r $h $d))
        ((#x63) (ld-r-r $h $e))
        ((#x64) (ld-r-r $h $h))
        ((#x65) (ld-r-r $h $l))
        ((#x66) (ld-r-ihl $h $mem $h $l))
        ((#x67) (ld-r-r $h $a))

        ((#x68) (ld-r-r $l $b))
        ((#x69) (ld-r-r $l $c))
        ((#x6a) (ld-r-r $l $d))
        ((#x6b) (ld-r-r $l $e))
        ((#x6c) (ld-r-r $l $h))
        ((#x6d) (ld-r-r $l $l))
        ((#x6e) (ld-r-ihl $l $mem $h $l))
        ((#x6f) (ld-r-r $l $a))

        ((#x70) (ld-ihl-r $mem $h $l $b))
        ((#x71) (ld-ihl-r $mem $h $l $c))
        ((#x72) (ld-ihl-r $mem $h $l $d))
        ((#x73) (ld-ihl-r $mem $h $l $e))
        ((#x74) (ld-ihl-r $mem $h $l $h))
        ((#x75) (ld-ihl-r $mem $h $l $l))
        ((#x76) (set! $halt? #t))
        ((#x77) (ld-ihl-r $mem $h $l $a))

        ((#x78) (ld-r-r $a $b))
        ((#x79) (ld-r-r $a $c))
        ((#x7a) (ld-r-r $a $d))
        ((#x7b) (ld-r-r $a $e))
        ((#x7c) (ld-r-r $a $h))
        ((#x7d) (ld-r-r $a $l))
        ((#x7e) (ld-r-ihl $a $mem $h $l))
        ((#x7f) (ld-r-r $a $a))

        ((#xed)
          (fetch-op)

          (case $op
            ((#x41) (out-c-r $out $b $c $b))
            ((#x49) (out-c-r $out $b $c $c))
            ((#x51) (out-c-r $out $b $c $d))
            ((#x59) (out-c-r $out $b $c $e))
            ((#x61) (out-c-r $out $b $c $h))
            ((#x69) (out-c-r $out $b $c $l))
            ((#x71) (out-c-r $out $b $c 0))
            ((#x79) (out-c-r $out $b $c $a))
            (else (throw illegal-ed-op $op))))

        (else (throw illegal-op $op))
      )))

  (define-syntax-rule (fetch-op $op $mem $pc)
    (begin
      (set! $op (mem-ref $mem $pc))
      (set! $pc (fx+ $pc 1))))

  (define-syntax-rule (fetch-n $n $mem $pc)
    (begin
      (set! $n (mem-ref $mem $pc))
      (set! $pc (fxand (fx+ $pc 1) #xffff))))

  (define-syntax-rule (fetch-m $m $mem $pc)
    (begin
      (set! $m (mem-ref $mem $pc))
      (set! $pc (fxand (fx+ $pc 1) #xffff))))

  (define-syntax-rule (fetch-nm $n $m $mem $pc)
    (begin
      (fetch-m $m $mem $pc)
      (fetch-n $n $mem $pc)))

  (define-syntax-rule (nop)
    (void))

  (define-syntax-rule (ld-r-r $r1 $r2)
    (begin
      (set! $r1 $r2)))

  (define-syntax-rule (ld-r-n $r $n $mem $pc)
    (begin
      (fetch-n $n $mem $pc)
      (set! $r $n)))

  (define-syntax-rule (ld-r-ihl $r $mem $h $l)
    (set! $r (mem-ref $mem (nm $h $l))))

  (define-syntax-rule (ld-ihl-r $mem $h $l $r)
    (mem-set! $mem (nm $h $l) $r))

  (define-syntax-rule (ld-ihl-n $h $l $n $mem $pc)
    (begin
      (fetch-n $n $mem $pc)
      (mem-set! $mem (nm $h $l) $n)))

  (define-syntax-rule (ld-rr-nm $h $l $n $m $mem $pc)
    (begin
      (fetch-nm $n $m $mem $pc)
      (rr-set-nm! $h $l $n $m)))

  (define-syntax-rule (ld-sp-nm $sp $n $m $mem $pc)
    (begin
      (fetch-n $m $mem $pc)
      (fetch-m $n $mem $pc)
      (set! $sp (nm $n $m))))

  (define-syntax-rule (out-c-r $out $b $c $r)
    ($out (nm $b $c) $r))

  (define-syntax-rule (nm $n $m)
    (fxior (fxsll $n 8) $m))

  (define-syntax-rule (mem-ref $mem $addr)
    (bytevector-u8-ref $mem $addr))

  (define-syntax-rule (mem-set! $mem $addr $value)
    (bytevector-u8-set! $mem $addr $value))

  (define-syntax-rule (rr-set-nm! $h $l $n $m)
    (begin
      (set! $h $n)
      (set! $l $m)))
)
