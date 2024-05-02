(import (syntax) (system) (syntaxes) (lets) (procedure) (list) (emu u) (emu reg) (emu mem))

(parameterize ((optimize-level 3))
  (displayln
    (run
      (define-mem mem #x10000)

      (define-r16 pc)
      (define-r16 sp)

      (define $h (make-bytevector 3 0))
      (define $l (make-bytevector 3 0))
      (define $hl-offset 0)

      (define-r8 b)
      (define-r8 c)
      (define-r8 d)
      (define-r8 e)
      (define-r8 a)
      (define-r8 f)

      (define-rule-syntax (define-indexed-r8 $id $var)
        (define-rules-syntax ()
          (($id) (bytevector-u8-ref $var $hl-offset))
          (($id $u8) (bytevector-u8-set! $var $hl-offset $u8))))

      (define-indexed-r8 h $h)
      (define-indexed-r8 l $l)

      (define-r16 bc b c)
      (define-r16 de d e)
      (define-r16 hl h l)
      (define-r16 af a f)

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

      (define-rule-syntax (fetch-8)
        (run
          (define $u8 (mem (pc)))
          (pc (u16+1 (pc)))
          $u8))

      (define-rule-syntax (fetch-16)
        (lets
          ($l (fetch-8))
          ($h (fetch-8))
          (u16-88 $h $l)))

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
        (mem $i (random #x100)))

      ; Run and measure.
      (time
        (do
          (($i 35000000 (fx-/wraparound $i 1)))
          ((fxzero? $i) (a))
          (set! $hl-offset 0)
          (app (vector-ref $ops (fetch-8))))))))
