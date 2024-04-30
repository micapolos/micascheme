(import (syntax) (system) (syntaxes) (lets) (procedure) (list))

(parameterize ((optimize-level 3))
  (displayln
    (run
      (define $pc 0)
      (define $sp 0)

      (define $a 0)
      (define $f 0)
      (define $b 0)
      (define $c 0)
      (define $d 0)
      (define $e 0)

      (define $h (make-bytevector 3 0))
      (define $l (make-bytevector 3 0))
      (define $hl-offset 0)

      (define $i 0)
      (define $r 0)

      (define $mem (make-bytevector #x10000))

      (define-rule-syntax (native-endianness) 'big)

      (define-rule-syntax (define-r8 $id $var)
        (define-rules-syntax ()
          (($id) $var)
          (($id $u8) (set! $var $u8))))

      (define-r8 b $b)
      (define-r8 c $c)
      (define-r8 d $d)
      (define-r8 e $e)
      (define-r8 a $a)
      (define-r8 f $f)

      (define-rule-syntax (define-indexed-r8 $id $var)
        (define-rules-syntax ()
          (($id) (bytevector-u8-ref $var $hl-offset))
          (($id $u8) (bytevector-u8-set! $var $hl-offset $u8))))

      (define-indexed-r8 h $h)
      (define-indexed-r8 l $l)

      (define-rule-syntax (define-r16 $id $h $l)
        (define-rules-syntax ()
          (($id) (fxior (fxsll ($h) 8) ($l)))
          (($id $u16)
            (lets
              ($val $u16)
              ($h (fxsrl $val 8))
              ($l (fxand $val #xff))))))

      (define-r16 bc b c)
      (define-r16 de d e)
      (define-r16 hl h l)
      (define-r16 af a f)

      (define-rule-syntax (u8+ $a $b)
        (fxand (fx+/wraparound $a $b) #xff))

      (define-rule-syntax (u16+ $a $b)
        (fxand (fx+/wraparound $a $b) #xffff))

      (define-rules-syntax ()
        ((r 0) (b))
        ((r 1) (c))
        ((r 2) (d))
        ((r 3) (e))
        ((r 4) (h))
        ((r 5) (l))
        ((r 6) (mem (hl)))
        ((r 7) (a))
        ((r 0 $u8) (b $u8))
        ((r 1 $u8) (c $u8))
        ((r 2 $u8) (d $u8))
        ((r 3 $u8) (e $u8))
        ((r 4 $u8) (h $u8))
        ((r 5 $u8) (l $u8))
        ((r 6 $u8) (mem (hl) $u8))
        ((r 7 $u8) (a $u8)))

      (define-rules-syntaxes ()
        ((dd) (set! $hl-offset 1))
        ((fd) (set! $hl-offset 2)))

      (define-rules-syntax ()
        ((mem $addr) (bytevector-u8-ref $mem $addr))
        ((mem $addr $u8) (bytevector-u8-set! $mem $addr $u8)))

      (define-rule-syntax (fetch-8)
        (run
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
            (run $body ...))))

      (define-syntax (with-r $syntax)
        (syntax-case $syntax ()
          ((_ $id $idx $body ...)
            #`(begin
              #,@(build-list 8
                (lambda ($index)
                  #`(let-syntax
                    (($id
                      (lambda ($syntax)
                        (syntax-case $syntax ()
                          ((_) #`(r #,#,$index))
                          ((_ $expr) #`(r #,#,$index $expr))))))
                    (let
                      (($idx #,$index))
                      $body ...))))))))

      (define (u8-233 $a $b $c)
        (fxior (fxsll $a 6) (fxsll $b 3) $c))

      (define-rule-syntax (build-ops $op-id $bodys ...)
        (run
          (define $vec (make-vector 256 (lambda () (void))))

          (define-rule-syntax ($op-id $idx $body)
            (vector-set! $vec $idx (lambda () $body)))

          $bodys ...

          (vector->immutable-vector $vec)))

      (define $ops
        (build-ops op
          (op #xdd (dd))
          (op #xfd (fd))

          ; (ld r r)
          (with-r l $l
            (with-r r $r
              (op
                (u8-233 #b01 $l $r)
                (l (r)))))

          ; (ld r n)
          (with-r r $r
            (op
              (u8-233 #b00 $r #b110)
              (r (fetch-8))))

          ; halt
          (op #b01110110 (void))

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
          (set! $hl-offset 0)
          (app (vector-ref $ops (fetch-8))))))))
