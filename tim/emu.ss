(import (syntax) (syntaxes) (lets) (procedure))

(parameterize ((optimize-level 3))
  (display
    (let ()
      (define $pc 0)
      (define $sp 0)

      (define $a 0)
      (define $f 0)
      (define $b 0)
      (define $c 0)
      (define $d 0)
      (define $e 0)
      (define $h 0)
      (define $l 0)

      (define $ixh 0)
      (define $ixl 0)
      (define $iyh 0)
      (define $iyl 0)

      (define $i 0)
      (define $r 0)

      (define $regs (make-bytevector 16 0))
      (define $mem (make-bytevector #x10000))

      (define-rule-syntax (native-endianness) 'big)

      (define-rules-syntax ()
        ((r $idx) (bytevector-u8-ref $regs $idx))
        ((r $idx $u8) (bytevector-u8-set! $regs $idx $u8)))

      (define-rule-syntax (define-r8 $id $idx)
        (define-rules-syntax ()
          (($id) (bytevector-u8-ref $regs $idx))
          (($id $u8) (bytevector-u8-set! $regs $idx $u8))))

      (define-rule-syntax (define-r16 $id $idx)
        (define-rules-syntax ()
          (($id) (bytevector-u16-ref $regs $idx (native-endianness)))
          (($id $u16) (bytevector-u16-set! $regs $idx $u16 (native-endianness)))))

      (define-rule-syntax (u8+ $a $b)
        (fxand (fx+/wraparound $a $b) #xff))

      (define-rule-syntax (u16+ $a $b)
        (fxand (fx+/wraparound $a $b) #xffff))

      (define-r8 b 0)
      (define-r8 c 1)
      (define-r8 d 2)
      (define-r8 e 3)
      (define-r8 h 4)
      (define-r8 l 5)
      (define-r8 a 7)

      (define-r16 bc 0)
      (define-r16 de 2)
      (define-r16 hl 4)

      (define-rules-syntax ()
        ((mem $addr) (bytevector-u8-ref $mem $addr))
        ((mem $addr $u8) (bytevector-u8-set! $mem $addr $u8)))

      (define-rule-syntax (fetch-8)
        (let ()
          (define $u8 (mem $pc))
          (set! $pc (u16+ $pc 1))
          $u8))

      (define-rule-syntax (fetch-16)
        (lets
          ($l (fetch-8))
          ($h (fetch-8))
          (fxior (fxsrl $h 8) $l)))

      (define-rule-syntax (for-each-r $r $body ...)
        (repeat-indexed 8 $r
          (if (not (fx= $r #b110))
            (let () $body ...))))

      (define (r-ihl? $r) (fx= $r #b110))

      (define (u8-233 $a $b $c)
        (fxior (fxsll $a 6) (fxsll $b 3) $c))

      (define-aux-keyword op)

      (define-rule-syntax (build-ops $op-id $bodys ...)
        (let ()
          (define $vec (make-vector 256 (lambda () (void))))

          (define-rule-syntax ($op-id $idx $body)
            (vector-set! $vec $idx (lambda () $body)))

          $bodys ...

          (vector->immutable-vector $vec)))

      (define $ops
        (build-ops op
          ; (ld r r)
          (for-each-r $l
            (for-each-r $r
              (op
                (u8-233 #b01 $l $r)
                (r $l (r $r)))))

          ; (ld (hl) r)
          (for-each-r $r
            (op
              (u8-233 #b01 $r #b110)
              (mem (hl) (r $r))))

          ; (ld r (hl))
          (for-each-r $r
            (op
              (u8-233 #b01 #b110 $r)
              (r $r (mem (hl)))))

          ; (ld r n)
          (for-each-r $r
            (op
              (u8-233 #b00 $r #b110)
              (r $r (fetch-8))))

          ; (ld (hl) n)
          (op #b00110110 (mem (hl) (fetch-8)))

          ; (ld a (bc))
          (op #b00001010 (a (mem (bc))))

          ; (ld a (de))
          (op #b00011010 (a (mem (de))))

          ; (ld a (nn))
          (op #b00111010 (a (mem (fetch-16))))

          ; (ld (bc) a)
          (op #b00000010 (mem (bc) (a)))

          ; (ld (de) a)
          (op #b00010010 (mem (de) (a)))

          ; (ld (nn) a)
          (op #b00110010 (mem (fetch-16) (a)))
      ))

      ; Fill memory with random values
      (do
        (($i 0 (add1 $i)))
        ((= $i #x10000) (void))
        (bytevector-u8-set! $mem $i (random #x100)))

      ; Run and measure.
      (time
        (do
          (($i 35000000 (fx-/wraparound $i 1)))
          ((fx= $i 0) (a))
          (app (vector-ref $ops (fetch-8)))))))
  (newline))
